{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fmax-pmcheck-models=200 #-}

-- |
-- Module      :  Disco.Interpret.CESK
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- CESK machine interpreter for Disco.
module Disco.Interpret.CESK (
  CESK,
  runCESK,
  step,
  eval,
  evalApp,
  runTest,
)
where

import Unbound.Generics.LocallyNameless (Bind, Name)

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AdjMap
import Control.Arrow ((***), (>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (first, second)
import Data.List (find)
import qualified Data.List.Infinite as InfList
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ratio
import Disco.AST.Core
import Disco.AST.Generic (
  Ellipsis (..),
  Side (..),
  selectSide,
 )
import Disco.AST.Typed (AProperty)
import Disco.Compile
import Disco.Context as Ctx
import Disco.Enumerate
import Disco.Error
import Disco.Names
import Disco.Property
import Disco.Types hiding (V)
import Disco.Value
import Math.Combinatorics.Exact.Binomial (choose)
import Math.Combinatorics.Exact.Factorial (factorial)
import Math.NumberTheory.Primes (factorise, unPrime)
import Math.NumberTheory.Primes.Testing (isPrime)

-- import Math.OEIS (
--   catalogNums,
--   extendSequence,
--   lookupSequence,
--  )

import Disco.Effects.Fresh
import Disco.Effects.Input
import Polysemy
import Polysemy.Error
import Polysemy.Random
import Polysemy.State

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

------------------------------------------------------------
-- Frames and continuations
------------------------------------------------------------

-- The CESK machine carries a current continuation explaining what to
-- do with the value of the currently focused expression, once it has
-- been fully evaluated.

-- | A continuation is just a stack of frames.
type Cont = [Frame]

-- | A frame represents a single step of the context, explaining what
--   to do with a value in that context (ultimately transforming it
--   into another value, which may in turn be handed to the next frame
--   in the continuation stack, and so on).
--
--   As an invariant, any 'Frame' containing unevaluated 'Core'
--   expressions must also carry an 'Env' in which to evaluate them.
data Frame
  = -- | Inject the value into a sum type.
    FInj Side
  | -- | Do a case analysis on the value.
    FCase Env (Bind (Name Core) Core) (Bind (Name Core) Core)
  | -- | Evaluate the right-hand value of a pair once we have finished
    --   evaluating the left-hand side.
    FPairR Env Core
  | -- | Put the value into the right-hand side of a pair together with
    --   this previously evaluated left-hand side.
    FPairL Value
  | -- | Project one or the other side of a pair.
    FProj Side
  | -- | Evaluate the argument of an application once we have finished
    --   evaluating the function.
    FArg Env Core
  | -- | Apply an evaluated function to this already-evaluated argument.
    FArgV Value
  | -- | Apply a previously evaluated function to the value.
    FApp Value
  | -- | Force evaluation of the contents of a memory cell.
    FForce
  | -- | Update the contents of a memory cell with its evaluation.
    FUpdate Int
  | -- | Record the results of a test.
    FTest TestVars Env
  deriving (Show)

------------------------------------------------------------
-- The CESK machine
------------------------------------------------------------

-- | The CESK machine has two basic kinds of states.
data CESK
  = -- | The 'In' constructor represents the state when we are recursing
    --   "into" a term.  There is a currently focused expression which
    --   is to be evaluated in the given context.  Generally, evaluation
    --   proceeds by pattern-matching on the focused expression and
    --   either immediately turning it into a value (if it is simple),
    --   or focusing on a subexpression and pushing a new frame on the
    --   continuation stack indicating how to continue evaluating the
    --   whole expression once finished with the subexpression.
    In Core Env Cont
  | -- | The 'Out' constructor represents the state when we have
    --   completed evaluating an expression and are now on our way back
    --   "out" of the recursion.  Generally, evaluation proceeds by
    --   pattern-matching on the top frame of the continuation stack
    --   (and sometimes on the value as well), to see what is to be done
    --   with the value.
    Out Value Cont
  | -- | There is also an 'Up' constructor representing an exception
    --   that is propagating up the continuation stack.  Disco does
    --   not have user-level exceptions or try/catch blocks etc., but
    --   exceptions may be caught by test frames and turned into a
    --   test result rather than crashing the entire computation.
    Up EvalError Cont
  deriving (Show)

-- | Is the CESK machine in a final state?
isFinal :: CESK -> Maybe (Either EvalError Value)
isFinal (Up e []) = Just (Left e)
isFinal (Out v []) = Just (Right v)
isFinal _ = Nothing

-- | Run a CESK machine to completion.
runCESK :: Members '[Fresh, Random, State Mem] r => CESK -> Sem r (Either EvalError Value)
runCESK cesk = case isFinal cesk of
  Just res -> return res
  Nothing -> step cesk >>= runCESK

-- | Advance the CESK machine by one step.
step :: Members '[Fresh, Random, State Mem] r => CESK -> Sem r CESK
step cesk = case cesk of
  (In (CVar x) e k) -> case Ctx.lookup' x e of
    Nothing -> return $ Up (UnboundError x) k
    Just v -> return $ Out v k
  (In (CNum d r) _ k) -> return $ Out (VNum d r) k
  (In (CConst OMatchErr) _ k) -> return $ Up NonExhaustive k
  (In (CConst OEmptyGraph) _ k) -> return $ Out (VGraph empty) k
  (In (CConst op) _ k) -> return $ Out (VConst op) k
  (In (CInj s c) e k) -> return $ In c e (FInj s : k)
  (In (CCase c b1 b2) e k) -> return $ In c e (FCase e b1 b2 : k)
  (In CUnit _ k) -> return $ Out VUnit k
  (In (CPair c1 c2) e k) -> return $ In c1 e (FPairR e c2 : k)
  (In (CProj s c) e k) -> return $ In c e (FProj s : k)
  (In (CAbs b) e k) -> do
    (xs, body) <- unbind b
    return $ Out (VClo e xs body) k
  (In (CApp c1 c2) e k) -> return $ In c1 e (FArg e c2 : k)
  (In (CType ty) _ k) -> return $ Out (VType ty) k
  (In (CDelay b) e k) -> do
    (xs, cs) <- unbind b
    locs <- allocateRec e (zip (map localName xs) cs)
    return $ Out (foldr (VPair . VRef) VUnit locs) k
  (In (CForce c) e k) -> return $ In c e (FForce : k)
  (In (CTest vars c) e k) -> return $ In c e (FTest (TestVars vars) e : k)
  (Out v (FInj s : k)) -> return $ Out (VInj s v) k
  (Out (VInj L v) (FCase e b1 _ : k)) -> do
    (x, c1) <- unbind b1
    return $ In c1 (Ctx.insert (localName x) v e) k
  (Out (VInj R v) (FCase e _ b2 : k)) -> do
    (x, c2) <- unbind b2
    return $ In c2 (Ctx.insert (localName x) v e) k
  (Out v1 (FPairR e c2 : k)) -> return $ In c2 e (FPairL v1 : k)
  (Out v2 (FPairL v1 : k)) -> return $ Out (VPair v1 v2) k
  (Out (VPair v1 v2) (FProj s : k)) -> return $ Out (selectSide s v1 v2) k
  (Out v (FArg e c2 : k)) -> return $ In c2 e (FApp v : k)
  (Out v2 (FApp (VClo e [x] b) : k)) -> return $ In b (Ctx.insert (localName x) v2 e) k
  (Out v2 (FApp (VClo e (x : xs) b) : k)) -> return $ Out (VClo (Ctx.insert (localName x) v2 e) xs b) k
  (Out v2 (FApp (VConst op) : k)) -> appConst k op v2
  (Out v2 (FApp (VFun f) : k)) -> return $ Out (f v2) k
  -- Annoying to repeat this code, not sure of a better way.
  -- The usual evaluation order (function then argument) doesn't work when
  -- we're applying a test function to randomly generated values.
  (Out (VClo e [x] b) (FArgV v : k)) -> return $ In b (Ctx.insert (localName x) v e) k
  (Out (VClo e (x : xs) b) (FArgV v : k)) -> return $ Out (VClo (Ctx.insert (localName x) v e) xs b) k
  (Out (VConst op) (FArgV v : k)) -> appConst k op v
  (Out (VFun f) (FArgV v : k)) -> return $ Out (f v) k
  (Out (VRef n) (FForce : k)) -> do
    cell <- lkup n
    case cell of
      Nothing -> error $ "impossible: location " ++ show n ++ " not found in memory"
      Just (V v) -> return $ Out v k
      Just (E e t) -> do
        set n Blackhole
        return $ In t e (FUpdate n : k)
      Just Blackhole -> return $ Up InfiniteLoop k
  (Out v (FUpdate n : k)) -> do
    set n (V v)
    return $ Out v k
  (Up err (f@FTest {} : k)) ->
    return $ Out (VProp (VPDone (TestResult False (TestRuntimeError err) emptyTestEnv))) (f : k)
  (Up err (_ : ks)) -> return $ Up err ks
  (Out v (FTest vs e : k)) -> do
    let result = ensureProp v
        res = getTestEnv vs e
    case res of
      Left err -> return $ Up err k
      Right e' -> return $ Out (VProp $ extendPropEnv e' result) k
  _ -> error "Impossible! Bad CESK machine state"

------------------------------------------------------------
-- Interpreting constants
------------------------------------------------------------

arity2 :: (Value -> Value -> a) -> Value -> a
arity2 f (VPair x y) = f x y
arity2 _f _v = error "arity2 on a non-pair!"

arity3 :: (Value -> Value -> Value -> a) -> Value -> a
arity3 f (VPair x (VPair y z)) = f x y z
arity3 _f _v = error "arity3 on a non-triple!"

appConst ::
  Members '[Random, State Mem] r =>
  Cont ->
  Op ->
  Value ->
  Sem r CESK
appConst k = \case
  --------------------------------------------------
  -- Basics

  OCrash -> up . Crash . vlist vchar
  OId -> out
  --------------------------------------------------
  -- Arithmetic

  OAdd -> numOp2 (+) >=> out
  ONeg -> numOp1 negate >=> out
  OSqrt -> numOp1 integerSqrt >=> out
  OFloor -> numOp1 ((% 1) . floor) >=> out
  OCeil -> numOp1 ((% 1) . ceiling) >=> out
  OAbs -> numOp1 abs >=> out
  OMul -> numOp2 (*) >=> out
  ODiv -> numOp2' divOp >>> outWithErr
   where
    divOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
    divOp _ 0 = throw DivByZero
    divOp m n = return $ ratv (m / n)
  OExp -> numOp2' expOp >>> outWithErr
   where
    expOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
    expOp m n
      | m == 0 && n < 0 = throw DivByZero
      | otherwise = return $ ratv (m ^^ numerator n)
  OMod -> numOp2' modOp >>> outWithErr
   where
    modOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
    modOp m n
      | n == 0 = throw DivByZero
      | otherwise = return $ intv (numerator m `mod` numerator n)
  ODivides -> numOp2' (\m n -> return (boolv $ divides m n)) >=> out
   where
    divides 0 0 = True
    divides 0 _ = False
    divides x y = denominator (y / x) == 1

  --------------------------------------------------
  -- Number theory

  OIsPrime -> intOp1 (boolv . isPrime) >=> out
  OFactor -> intOp1' primFactor >>> outWithErr
   where
    -- Semantics of the @$factor@ prim: turn a natural number into its
    -- bag of prime factors.  Crash if called on 0, which does not have
    -- a prime factorization.
    primFactor :: Member (Error EvalError) r => Integer -> Sem r Value
    primFactor 0 = throw (Crash "0 has no prime factorization!")
    primFactor n = return . VBag $ map ((intv . unPrime) *** fromIntegral) (factorise n)
  OFrac -> numOp1' (return . primFrac) >=> out
   where
    -- Semantics of the @$frac@ prim: turn a rational number into a pair
    -- of its numerator and denominator.
    primFrac :: Rational -> Value
    primFrac r = VPair (intv (numerator r)) (intv (denominator r))

  --------------------------------------------------
  -- Combinatorics

  OMultinom -> arity2 multinomOp >=> out
   where
    multinomOp :: Value -> Value -> Sem r Value
    multinomOp (vint -> n0) (vlist vint -> ks0) = return . intv $ multinomial n0 ks0
     where
      multinomial :: Integer -> [Integer] -> Integer
      multinomial _ [] = 1
      multinomial n (k' : ks)
        | k' > n = 0
        | otherwise = choose n k' * multinomial (n - k') ks
  OFact -> numOp1' factOp >>> outWithErr
   where
    factOp :: Member (Error EvalError) r => Rational -> Sem r Value
    factOp (numerator -> n)
      | n > fromIntegral (maxBound :: Int) = throw Overflow
      | otherwise = return . intv $ factorial (fromIntegral n)
  OEnum -> out . enumOp
   where
    enumOp :: Value -> Value
    enumOp (VType ty) = listv id (enumerateType ty)
    enumOp v = error $ "Impossible! enumOp on non-type " ++ show v
  OCount -> out . countOp
   where
    countOp :: Value -> Value
    countOp (VType ty) = case countType ty of
      Just num -> VInj R (intv num)
      Nothing -> VNil
    countOp v = error $ "Impossible! countOp on non-type " ++ show v

  --------------------------------------------------
  -- Sequences

  OUntil -> arity2 $ \v1 -> out . ellipsis (Until v1)
  -- OLookupSeq -> out . oeisLookup
  -- OExtendSeq -> out . oeisExtend
  --------------------------------------------------
  -- Comparison

  OEq -> arity2 $ \v1 v2 -> out $ boolv (valEq v1 v2)
  OLt -> arity2 $ \v1 v2 -> out $ boolv (valLt v1 v2)
  --------------------------------------------------
  -- Container operations

  OPower -> withBag OPower $ out . VBag . sortNCount . map (first VBag) . choices
   where
    choices :: [(Value, Integer)] -> [([(Value, Integer)], Integer)]
    choices [] = [([], 1)]
    choices ((x, n) : xs) = xs' ++ concatMap (\k' -> map (cons n (x, k')) xs') [1 .. n]
     where
      xs' = choices xs
    cons n (x, k') (zs, m) = ((x, k') : zs, choose n k' * m)
  OBagElem -> arity2 $ \x ->
    withBag OBagElem $
      out . boolv . isJust . find (valEq x) . map fst
  OListElem -> arity2 $ \x -> out . boolv . isJust . find (valEq x) . vlist id
  OEachSet -> arity2 $ \f ->
    withBag OEachSet $
      outWithErr . fmap (VBag . countValues) . mapM (evalApp f . (: []) . fst)
  OEachBag -> arity2 $ \f ->
    withBag OEachBag $
      outWithErr . fmap (VBag . sortNCount) . mapM (\(x, n) -> (,n) <$> evalApp f [x])
  OFilterBag -> arity2 $ \f -> withBag OFilterBag $ \xs ->
    outWithErr $ do
      bs <- mapM (evalApp f . (: []) . fst) xs
      return . VBag . map snd . Prelude.filter (isTrue . fst) $ zip bs xs
   where
    isTrue (VInj R VUnit) = True
    isTrue _ = False
  OMerge -> arity3 $ \f bxs bys ->
    case (bxs, bys) of
      (VBag xs, VBag ys) -> outWithErr (VBag <$> mergeM f xs ys)
      (VBag _, _) -> error $ "Impossible! OMerge on non-VBag " ++ show bys
      _ -> error $ "Impossible! OMerge on non-VBag " ++ show bxs
  OBagUnions -> withBag OBagUnions $ \cts ->
    out . VBag $ sortNCount [(x, m * n) | (VBag xs, n) <- cts, (x, m) <- xs]
  --------------------------------------------------
  -- Container conversions

  OBagToSet -> withBag OBagToSet $ out . VBag . (map . second) (const 1)
  OSetToList -> withBag OSetToList $ out . listv id . map fst
  OBagToList -> withBag OBagToList $ out . listv id . concatMap (uncurry (flip (replicate . fromIntegral)))
  OListToSet -> out . VBag . (map . fmap) (const 1) . countValues . vlist id
  OListToBag -> out . VBag . countValues . vlist id
  OBagToCounts -> withBag OBagToCounts $ out . VBag . map ((,1) . pairv id intv)
  -- Bag (a, N) -> Bag a
  --   Notionally this takes a set of pairs instead of a bag, but operationally we need to
  --   be prepared for a bag, because of the way literal bags desugar, e.g.
  --
  --   Disco> :desugar let x = 3 in ⟅ 'a' # (2 + x), 'b', 'b' ⟆
  --   (λx. bagFromCounts(bag(('a', 2 + x) :: ('b', 1) :: ('b', 1) :: [])))(3)

  OCountsToBag ->
    withBag OCountsToBag $
      out . VBag . sortNCount . map (second (uncurry (*)) . assoc . first (vpair id vint))
   where
    assoc ((a, b), c) = (a, (b, c))
  OUnsafeCountsToBag ->
    withBag OUnsafeCountsToBag $
      out . VBag . map (second (uncurry (*)) . assoc . first (vpair id vint))
   where
    assoc ((a, b), c) = (a, (b, c))

  --------------------------------------------------
  -- Maps

  OMapToSet ->
    withMap OMapToSet $
      out . VBag . map (\(k', v) -> (VPair (fromSimpleValue k') v, 1)) . M.assocs
  OSetToMap ->
    withBag OSetToMap $
      out . VMap . M.fromList . map (convertAssoc . fst)
   where
    convertAssoc (VPair k' v) = (toSimpleValue k', v)
    convertAssoc v = error $ "Impossible! convertAssoc on non-VPair " ++ show v
  OInsert -> arity3 $ \k' v ->
    withMap OInsert $
      out . VMap . M.insert (toSimpleValue k') v
  OLookup -> arity2 $ \k' ->
    withMap OLookup $
      out . toMaybe . M.lookup (toSimpleValue k')
   where
    toMaybe = maybe (VInj L VUnit) (VInj R)

  --------------------------------------------------
  -- Graph operations

  OVertex -> out . VGraph . Vertex . toSimpleValue
  OOverlay -> arity2 $ withGraph2 OOverlay $ \g1 g2 ->
    out $ VGraph (Overlay g1 g2)
  OConnect -> arity2 $ withGraph2 OConnect $ \g1 g2 ->
    out $ VGraph (Connect g1 g2)
  OSummary -> withGraph OSummary $ out . graphSummary
  --------------------------------------------------
  -- Propositions

  OForall tys -> out . (\v -> VProp (VPSearch SMForall tys v emptyTestEnv))
  OExists tys -> out . (\v -> VProp (VPSearch SMExists tys v emptyTestEnv))
  OHolds -> testProperty Exhaustive >=> resultToBool >>> outWithErr
  ONotProp -> out . VProp . notProp . ensureProp
  OShouldEq ty -> arity2 $ \v1 v2 ->
    out $ VProp (VPDone (TestResult (valEq v1 v2) (TestEqual ty v1 v2) emptyTestEnv))
  OShouldLt ty -> arity2 $ \v1 v2 ->
    out $ VProp (VPDone (TestResult (valLt v1 v2) (TestLt ty v1 v2) emptyTestEnv))
  OAnd -> arity2 $ \p1 p2 ->
    out $ VProp (VPBin LAnd (ensureProp p1) (ensureProp p2))
  OOr -> arity2 $ \p1 p2 ->
    out $ VProp (VPBin LOr (ensureProp p1) (ensureProp p2))
  OImpl -> arity2 $ \p1 p2 ->
    out $ VProp (VPBin LImpl (ensureProp p1) (ensureProp p2))
  c -> error $ "Unimplemented: appConst " ++ show c
 where
  outWithErr :: Sem (Error EvalError ': r) Value -> Sem r CESK
  outWithErr m = either (`Up` k) (`Out` k) <$> runError m
  out v = return $ Out v k
  up e = return $ Up e k

  withBag :: Op -> ([(Value, Integer)] -> Sem r a) -> Value -> Sem r a
  withBag op f = \case
    VBag xs -> f xs
    v -> error $ "Impossible! " ++ show op ++ " on non-VBag " ++ show v

  withMap :: Op -> (M.Map SimpleValue Value -> Sem r a) -> Value -> Sem r a
  withMap op f = \case
    VMap m -> f m
    v -> error $ "Impossible! " ++ show op ++ " on non-VMap " ++ show v

  withGraph :: Op -> (Graph SimpleValue -> Sem r a) -> Value -> Sem r a
  withGraph op f = \case
    VGraph g -> f g
    v -> error $ "Impossible! " ++ show op ++ " on non-VGraph " ++ show v

  withGraph2 :: Op -> (Graph SimpleValue -> Graph SimpleValue -> Sem r a) -> Value -> Value -> Sem r a
  withGraph2 op f v1 v2 = case (v1, v2) of
    (VGraph g1, VGraph g2) -> f g1 g2
    (_, VGraph _) -> error $ "Impossible! " ++ show op ++ " on non-VGraph " ++ show v1
    _ -> error $ "Impossible! " ++ show op ++ " on non-VGraph " ++ show v2

--------------------------------------------------
-- Arithmetic

intOp1 :: (Integer -> Value) -> Value -> Sem r Value
intOp1 f = intOp1' (return . f)

intOp1' :: (Integer -> Sem r Value) -> Value -> Sem r Value
intOp1' f = f . vint

numOp1 :: (Rational -> Rational) -> Value -> Sem r Value
numOp1 f = numOp1' $ return . ratv . f

numOp1' :: (Rational -> Sem r Value) -> Value -> Sem r Value
numOp1' f (VNum _ m) = f m
numOp1' _ v = error $ "Impossible! numOp1' on non-VNum " ++ show v

numOp2 :: (Rational -> Rational -> Rational) -> Value -> Sem r Value
numOp2 (#) = numOp2' $ \m n -> return (ratv (m # n))

numOp2' :: (Rational -> Rational -> Sem r Value) -> Value -> Sem r Value
numOp2' (#) =
  arity2 $ \v1 v2 -> case (v1, v2) of
    (VNum d1 n1, VNum d2 n2) -> do
      res <- n1 # n2
      case res of
        VNum _ r -> return $ VNum (d1 <> d2) r
        _ -> return res
    (VNum {}, _) -> error $ "Impossible! numOp2' on non-VNum " ++ show v2
    _ -> error $ "Impossible! numOp2' on non-VNum " ++ show v1

-- | Perform a square root operation. If the program typechecks,
--   then the argument and output will really be Natural.
integerSqrt :: Rational -> Rational
integerSqrt n = integerSqrt' (numerator n) % 1

-- | implementation of `integerSqrt'` taken from the Haskell wiki:
--   https://wiki.haskell.org/Generic_number_type#squareRoot
integerSqrt' :: Integer -> Integer
integerSqrt' 0 = 0
integerSqrt' 1 = 1
integerSqrt' n =
  let twopows = iterate (^! 2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = InfList.iterate' newtonStep (integerSqrt' (div n lowerN) * lowerRoot)
      isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
   in InfList.head $ InfList.dropWhile (not . isRoot) iters

-- this operator is used for `integerSqrt'`
(^!) :: Num a => a -> Int -> a
(^!) x n = x ^ n

------------------------------------------------------------
-- Comparison
------------------------------------------------------------

valEq :: Value -> Value -> Bool
valEq v1 v2 = valCmp v1 v2 == EQ

valLt :: Value -> Value -> Bool
valLt v1 v2 = valCmp v1 v2 == LT

valCmp :: Value -> Value -> Ordering
valCmp (VNum _ r1) (VNum _ r2) = compare r1 r2
valCmp (VInj L _) (VInj R _) = LT
valCmp (VInj R _) (VInj L _) = GT
valCmp (VInj L v1) (VInj L v2) = valCmp v1 v2
valCmp (VInj R v1) (VInj R v2) = valCmp v1 v2
valCmp VUnit VUnit = EQ
valCmp (VPair v11 v12) (VPair v21 v22) = valCmp v11 v21 <> valCmp v12 v22
valCmp (VType ty1) (VType ty2) = compare ty1 ty2
valCmp (VBag cs1) (VBag cs2) = compareBags cs1 cs2
valCmp (VMap m1) (VMap m2) = compareMaps (M.assocs m1) (M.assocs m2)
valCmp (VGraph g1) (VGraph g2) = valCmp (graphSummary g1) (graphSummary g2)
valCmp v1 v2 = error $ "valCmp\n  " ++ take 100 (show v1) ++ "...\n  " ++ take 100 (show v2) ++ "..."

compareBags :: [(Value, Integer)] -> [(Value, Integer)] -> Ordering
compareBags [] [] = EQ
compareBags [] _ = LT
compareBags _ [] = GT
compareBags ((x, xn) : xs) ((y, yn) : ys) =
  valCmp x y <> compare xn yn <> compareBags xs ys

compareMaps :: [(SimpleValue, Value)] -> [(SimpleValue, Value)] -> Ordering
compareMaps [] [] = EQ
compareMaps [] _ = LT
compareMaps _ [] = GT
compareMaps ((k1, v1) : as1) ((k2, v2) : as2) =
  valCmp (fromSimpleValue k1) (fromSimpleValue k2) <> valCmp v1 v2 <> compareMaps as1 as2

------------------------------------------------------------
-- Polynomial sequences [a,b,c,d .. e]
------------------------------------------------------------

ellipsis :: Ellipsis Value -> Value -> Value
ellipsis (fmap vrat -> end) (vlist vrat -> rs) = listv ratv $ enumEllipsis rs end

enumEllipsis :: (Enum a, Num a, Ord a) => [a] -> Ellipsis a -> [a]
enumEllipsis [] _ = error "Impossible! Disco.Interpret.CESK.enumEllipsis []"
enumEllipsis [x] (Until y)
  | x <= y = [x .. y]
  | otherwise = [x, pred x .. y]
enumEllipsis xs (Until y)
  | d > 0 = takeWhile (<= y) nums
  | d < 0 = takeWhile (>= y) nums
  | otherwise = nums
 where
  d = constdiff xs
  nums = babbage xs

-- | Extend a sequence infinitely by interpolating it as a polynomial
--   sequence, via forward differences.  Essentially the same
--   algorithm used by Babbage's famous Difference Engine.
babbage :: Num a => [a] -> [a]
babbage [] = []
babbage [x] = repeat x
babbage (x : xs) = scanl (+) x (babbage (diff (x : xs)))

-- | Compute the forward difference of the given sequence, that is,
--   differences of consecutive pairs of elements.
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) (drop 1 xs) xs

-- | Take forward differences until the result is constant, and return
--   the constant.  The sign of the constant difference tells us the
--   limiting behavior of the sequence.
constdiff :: (Eq a, Num a) => [a] -> a
constdiff [] = error "Impossible! Disco.Interpret.Core.constdiff []"
constdiff (x : xs)
  | all (== x) xs = x
  | otherwise = constdiff (diff (x : xs))

------------------------------------------------------------
-- OEIS
------------------------------------------------------------

-- -- | Looks up a sequence of integers in OEIS.
-- --   Returns 'left()' if the sequence is unknown in OEIS,
-- --   otherwise 'right "https://oeis.org/<oeis_sequence_id>"'
-- oeisLookup :: Value -> Value
-- oeisLookup (vlist vint -> ns) = maybe VNil parseResult (lookupSequence ns)
--  where
--   parseResult r = VInj R (listv charv ("https://oeis.org/" ++ seqNum r))
--   seqNum = getCatalogNum . catalogNums

--   getCatalogNum [] = error "No catalog info"
--   getCatalogNum (n : _) = n

-- -- | Extends a Disco integer list with data from a known OEIS
-- --   sequence.  Returns a list of integers upon success, otherwise the
-- --   original list (unmodified).
-- oeisExtend :: Value -> Value
-- oeisExtend = listv intv . extendSequence . vlist vint

------------------------------------------------------------
-- Normalizing bags/sets
------------------------------------------------------------

-- | Given a list of disco values, sort and collate them into a list
--   pairing each unique value with its count.  Used to
--   construct/normalize bags and sets.  Prerequisite: the values must
--   be comparable.
countValues :: [Value] -> [(Value, Integer)]
countValues = sortNCount . map (,1)

-- | Normalize a list of values where each value is paired with a
--   count, but there could be duplicate values.  This function uses
--   merge sort to sort the values, adding the counts of multiple
--   instances of the same value.  Prerequisite: the values must be
--   comparable.
sortNCount :: [(Value, Integer)] -> [(Value, Integer)]
sortNCount [] = []
sortNCount [x] = [x]
sortNCount xs = merge (+) (sortNCount firstHalf) (sortNCount secondHalf)
 where
  (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

-- | Generic function for merging two sorted, count-annotated lists of
--   type @[(a,Integer)]@ a la merge sort, using the given comparison
--   function, and using the provided count combining function to
--   decide what count to assign to each element of the output.  For
--   example, @(+)@ corresponds to bag union; @min@ corresponds to
--   intersection; and so on.
merge ::
  (Integer -> Integer -> Integer) ->
  [(Value, Integer)] ->
  [(Value, Integer)] ->
  [(Value, Integer)]
merge g = go
 where
  go [] [] = []
  go [] ((y, n) : ys) = mergeCons y 0 n (go [] ys)
  go ((x, n) : xs) [] = mergeCons x n 0 (go xs [])
  go ((x, n1) : xs) ((y, n2) : ys) = case valCmp x y of
    LT -> mergeCons x n1 0 (go xs ((y, n2) : ys))
    EQ -> mergeCons x n1 n2 (go xs ys)
    GT -> mergeCons y 0 n2 (go ((x, n1) : xs) ys)

  mergeCons a m1 m2 zs = case g m1 m2 of
    0 -> zs
    n -> (a, n) : zs

mergeM ::
  Members '[Random, Error EvalError, State Mem] r =>
  Value ->
  [(Value, Integer)] ->
  [(Value, Integer)] ->
  Sem r [(Value, Integer)]
mergeM g = go
 where
  go [] [] = return []
  go [] ((y, n) : ys) = mergeCons y 0 n =<< go [] ys
  go ((x, n) : xs) [] = mergeCons x n 0 =<< go xs []
  go ((x, n1) : xs) ((y, n2) : ys) = case valCmp x y of
    LT -> mergeCons x n1 0 =<< go xs ((y, n2) : ys)
    EQ -> mergeCons x n1 n2 =<< go xs ys
    GT -> mergeCons y 0 n2 =<< go ((x, n1) : xs) ys

  mergeCons a m1 m2 zs = do
    nm <- evalApp g [VPair (intv m1) (intv m2)]
    return $ case nm of
      VNum _ 0 -> zs
      VNum _ n -> (a, numerator n) : zs
      v -> error $ "Impossible! merge function in mergeM returned non-VNum " ++ show v

------------------------------------------------------------
-- Graphs
------------------------------------------------------------

graphSummary :: Graph SimpleValue -> Value
graphSummary = toDiscoAdjMap . reifyGraph
 where
  reifyGraph :: Graph SimpleValue -> [(SimpleValue, [SimpleValue])]
  reifyGraph =
    AdjMap.adjacencyList . foldg AdjMap.empty AdjMap.vertex AdjMap.overlay AdjMap.connect

  toDiscoAdjMap :: [(SimpleValue, [SimpleValue])] -> Value
  toDiscoAdjMap =
    VMap . M.fromList . map (second (VBag . countValues . map fromSimpleValue))

------------------------------------------------------------
-- Propositions / tests
------------------------------------------------------------

resultToBool :: Member (Error EvalError) r => TestResult -> Sem r Value
resultToBool (TestResult _ (TestRuntimeError e) _) = throw e
resultToBool (TestResult b _ _) = return $ boolv b

notProp :: ValProp -> ValProp
notProp (VPDone r) = VPDone (invertPropResult r)
notProp (VPSearch sm tys p e) = VPSearch (invertMotive sm) tys p e
notProp (VPBin LAnd vp1 vp2) = VPBin LOr (notProp vp1) (notProp vp2)
notProp (VPBin LOr vp1 vp2) = VPBin LAnd (notProp vp1) (notProp vp2)
notProp (VPBin LImpl vp1 vp2) = VPBin LAnd vp1 (notProp vp2)

-- | Convert a @Value@ to a @ValProp@, embedding booleans if necessary.
ensureProp :: Value -> ValProp
ensureProp (VProp p) = p
ensureProp (VInj L _) = VPDone (TestResult False TestBool emptyTestEnv)
ensureProp (VInj R _) = VPDone (TestResult True TestBool emptyTestEnv)
ensureProp _ = error "ensureProp: non-prop value"

combineTestResultBool :: LOp -> TestResult -> TestResult -> Bool
combineTestResultBool op (TestResult b1 _ _) (TestResult b2 _ _) = interpLOp op b1 b2

testProperty ::
  Members '[Random, State Mem] r =>
  SearchType ->
  Value ->
  Sem r TestResult
testProperty initialSt = checkProp . ensureProp
 where
  checkProp ::
    Members '[Random, State Mem] r =>
    ValProp ->
    Sem r TestResult
  checkProp (VPDone r) = return r
  checkProp (VPBin op vp1 vp2) = do
    tr1 <- checkProp vp1
    tr2 <- checkProp vp2
    return $ TestResult (combineTestResultBool op tr1 tr2) (TestBin op tr1 tr2) emptyTestEnv
  checkProp (VPSearch sm tys f e) =
    extendResultEnv e <$> (generateSamples initialSt vals >>= go)
   where
    vals = enumTypes tys
    (SearchMotive (whenFound, wantsSuccess)) = sm

    go ::
      Members '[Random, State Mem] r =>
      ([[Value]], SearchType) ->
      Sem r TestResult
    go ([], st) = return $ TestResult (not whenFound) (TestNotFound st) emptyTestEnv
    go (x : xs, st) = do
      mprop <- runError (ensureProp <$> evalApp f x)
      case mprop of
        Left err -> return $ TestResult False (TestRuntimeError err) emptyTestEnv
        Right (VPDone r) -> continue st xs r
        Right prop -> checkProp prop >>= continue st xs

    continue ::
      Members '[Random, State Mem] r =>
      SearchType ->
      [[Value]] ->
      TestResult ->
      Sem r TestResult
    continue st xs r@(TestResult _ _ e')
      | testIsError r = return r
      | testIsOk r == wantsSuccess =
          return $ TestResult whenFound (TestFound r) e'
      | otherwise = go (xs, st)

evalApp ::
  Members '[Random, Error EvalError, State Mem] r =>
  Value ->
  [Value] ->
  Sem r Value
evalApp f xs =
  runFresh (runCESK (Out f (map FArgV xs))) >>= either throw return

runTest ::
  Members '[Random, Error EvalError, Input Env, State Mem] r =>
  Int ->
  AProperty ->
  Sem r TestResult
runTest n p = testProperty (Randomized n' n') =<< eval (compileProperty p)
 where
  n' = fromIntegral (n `div` 2)

------------------------------------------------------------
-- Top-level evaluation
------------------------------------------------------------

eval :: Members '[Random, Error EvalError, Input Env, State Mem] r => Core -> Sem r Value
eval c = do
  e <- input @Env
  runFresh (runCESK (In c e [])) >>= either throw return

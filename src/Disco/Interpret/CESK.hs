{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Interpret.CESK
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- CESK machine interpreter for Disco.
module Disco.Interpret.CESK
  ( CESK,
    runCESK,
    step,
    eval,
    runTest,
  )
where

import           Control.Arrow                      ((***))
import           Control.Monad                      ((>=>))
import           Data.Bifunctor                     (first, second)
import           Data.List                          (find)
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (isJust)
import           Data.Ratio
import           Debug.Trace
import           Disco.AST.Core
import           Disco.AST.Generic                  (Ellipsis (..), Side (..),
                                                     selectSide)
import           Disco.AST.Typed                    (AProperty)
import           Disco.Effects.Fresh
import           Disco.Effects.Input
import           Disco.Enumerate
import           Disco.Error
import           Disco.Syntax.Operators             (BOp (..))
import           Disco.Types                        hiding (V)
import           Disco.Value
import           Math.Combinatorics.Exact.Binomial  (choose)
import           Math.Combinatorics.Exact.Factorial (factorial)
import           Math.NumberTheory.Primes           (factorise, unPrime)
import           Math.NumberTheory.Primes.Testing   (isPrime)
import           Math.OEIS                          (catalogNums,
                                                     extendSequence,
                                                     lookupSequence)
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Unbound.Generics.LocallyNameless   (Bind, Name)

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
  | -- | Apply a previously evaluated function to the value.
    FApp Value
  | -- | Force evaluation of the contents of a memory cell.
    FForce
  | -- | Update the contents of a memory cell with its evaluation.
    FUpdate Int
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
  deriving (Show)

-- | Is the CESK machine in a final state?
isFinal :: CESK -> Maybe Value
isFinal (Out v []) = Just v
isFinal _          = Nothing

-- | Run a CESK machine to completion.
runCESK :: Members '[Fresh, Error EvalError, State Mem] r => CESK -> Sem r Value
runCESK cesk = case isFinal cesk of
  Just vm -> return vm
  Nothing -> step cesk >>= runCESK

(!!!) :: (Ord k, Show k) => Map k v -> k -> v
m !!! k = case M.lookup k m of
  Nothing -> error $ "variable not found in environment: " ++ show k
  Just v  -> v

-- | Advance the CESK machine by one step.
step :: Members '[Fresh, Error EvalError, State Mem] r => CESK -> Sem r CESK
step (In (CVar x) e k) = return $ Out (e !!! x) k
step (In (CNum d r) _ k) = return $ Out (VNum d r) k
step (In (CConst OMatchErr) _ k) = throw NonExhaustive
step (In (CConst op) _ k) = return $ Out (VConst op) k
step (In (CInj s c) e k) = return $ In c e (FInj s : k)
step (In (CCase c b1 b2) e k) = return $ In c e (FCase e b1 b2 : k)
step (In CUnit _ k) = return $ Out VUnit k
step (In (CPair c1 c2) e k) = return $ In c1 e (FPairR e c2 : k)
step (In (CProj s c) e k) = return $ In c e (FProj s : k)
step (In (CAbs b) e k) = do
  (xs, body) <- unbind b
  return $ Out (VClo e xs body) k
step (In (CApp c1 c2) e k) = return $ In c1 e (FArg e c2 : k)
step (In (CType ty) _ k) = return $ Out (VType ty) k
step (In (CDelay b) e k) = do
  (xs, cs) <- unbind b
  locs <- allocateRec e (zip xs cs)
  return $ Out (foldr (VPair . VRef) VUnit locs) k
step (In (CForce c) e k) = return $ In c e (FForce : k)
step (Out v (FInj s : k)) = return $ Out (VInj s v) k
step (Out (VInj L v) (FCase e b1 _ : k)) = do
  (x, c1) <- unbind b1
  return $ In c1 (M.insert x v e) k
step (Out (VInj R v) (FCase e _ b2 : k)) = do
  (x, c2) <- unbind b2
  return $ In c2 (M.insert x v e) k
step (Out v1 (FPairR e c2 : k)) = return $ In c2 e (FPairL v1 : k)
step (Out v2 (FPairL v1 : k)) = return $ Out (VPair v1 v2) k
step (Out (VPair v1 v2) (FProj s : k)) = return $ Out (selectSide s v1 v2) k
step (Out v (FArg e c2 : k)) = return $ In c2 e (FApp v : k)
step (Out v2 (FApp (VClo e [x] b) : k)) = return $ In b (M.insert x v2 e) k
step (Out v2 (FApp (VClo e (x : xs) b) : k)) = return $ Out (VClo (M.insert x v2 e) xs b) k
step (Out v2 (FApp (VConst op) : k)) = appConst k op v2
step (Out (VRef n) (FForce : k)) = do
  cell <- lkup n
  case cell of
    Nothing -> undefined -- XXX ?
    Just (V v) -> return $ Out v k
    Just (E e t) -> do
      set n Blackhole
      return $ In t e (FUpdate n : k)
    Just Blackhole -> error "Infinite loop detected!" -- XXX make a real error
step (Out v (FUpdate n : k)) = do
  set n (V v)
  return $ Out v k
step c = error $ "step " ++ show c

------------------------------------------------------------
-- Interpreting constants
------------------------------------------------------------

arity2 :: (Value -> Value -> a) -> Value -> a
arity2 f (VPair x y) = f x y
arity2 f v           = error "arity2 on a non-pair!" -- XXX

arity3 :: (Value -> Value -> Value -> a) -> Value -> a
arity3 f (VPair x (VPair y z)) = f x y z
arity3 f v                     = error "arity3 on a non-triple!"

appConst :: Member (Error EvalError) r => Cont -> Op -> Value -> Sem r CESK
appConst k = \case
  --------------------------------------------------
  -- Basics

  OCrash -> throw . Crash . vlist vchar
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
  ODiv -> numOp2' divOp >=> out
    where
      divOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
      divOp _ 0 = throw DivByZero
      divOp m n = return $ ratv (m / n)
  OExp -> (numOp2 $ \m n -> m ^^ numerator n) >=> out
  OMod -> numOp2' modOp >=> out
    where
      modOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
      modOp m n
        | n == 0 = throw DivByZero
        | otherwise = return $ intv (numerator m `mod` numerator n)
  ODivides -> (numOp2' $ \m n -> return (enumv $ divides m n)) >=> out
    where
      divides 0 0 = True
      divides 0 _ = False
      divides x y = denominator (y / x) == 1

  --------------------------------------------------
  -- Number theory

  OIsPrime -> intOp1 (enumv . isPrime) >=> out
  OFactor -> intOp1' primFactor >=> out
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
      multinomOp (vint -> n) (vlist vint -> ks) = return . intv $ multinomial n ks
        where
          multinomial :: Integer -> [Integer] -> Integer
          multinomial _ [] = 1
          multinomial n (k : ks)
            | k > n = 0
            | otherwise = choose n k * multinomial (n - k) ks
  OFact -> numOp1' factOp >=> out
    where
      factOp :: Member (Error EvalError) r => Rational -> Sem r Value
      factOp (numerator -> n)
        | n > fromIntegral (maxBound :: Int) = throw Overflow
        | otherwise = return . intv $ factorial (fromIntegral n)
  OEnum -> out . enumOp
    where
      enumOp :: Value -> Value
      enumOp (VType ty) = listv id (enumerateType ty)
      enumOp v          = error $ "Impossible! enumOp on non-type " ++ show v -- XXX
  OCount -> out . countOp
    where
      countOp :: Value -> Value
      countOp (VType ty) = case countType ty of
        Just num -> VInj R (intv num)
        Nothing  -> VNil
      countOp v = error $ "Impossible! countOp on non-type " ++ show v

  --------------------------------------------------
  -- Sequences

  OUntil -> arity2 $ \v1 -> out . ellipsis (Until v1)
  OLookupSeq -> out . oeisLookup
  OExtendSeq -> out . oeisExtend
  --------------------------------------------------
  -- Comparison

  OEq -> arity2 $ \v1 v2 -> out $ enumv (valEq v1 v2)
  OLt -> arity2 $ \v1 v2 -> out $ enumv (valLt v1 v2)
  --------------------------------------------------
  -- Container operations

  OSize -> out . sizeOp
    where
      sizeOp (VBag xs) = intv (sum (map snd xs))
  OPower -> \(VBag xs) -> out . VBag . sortNCount . map (first VBag) . choices $ xs
    where
      choices :: [(Value, Integer)] -> [([(Value, Integer)], Integer)]
      choices [] = [([], 1)]
      choices ((x, n) : xs) = xs' ++ concatMap (\k -> map (cons n (x, k)) xs') [1 .. n]
        where
          xs' = choices xs
      cons n (x, k) (zs, m) = ((x, k) : zs, choose n k * m)
  OBagElem -> arity2 $ \x (VBag xs) ->
    out . enumv . isJust . find (valEq x) . map fst $ xs
  OListElem -> arity2 $ \x -> out . enumv . isJust . find (valEq x) . vlist id
  -- appConst (OEachSet ty)                      = _wA
  -- each@Set f = set . each@List f . list

  -- appConst OReduceList                        = _wB
  -- appConst OReduceBag                         = _wC

  -- appConst OFilterBag                         = _wE

  OMerge op -> arity2 $ \(VBag xs) (VBag ys) -> out . VBag $ merge (interpOp op) xs ys
    where
      interpOp Max  = max
      interpOp Min  = min
      interpOp Add  = (+)
      interpOp SSub = \x y -> max 0 (x - y)
      interpOp op   = error $ "unknown op in OMerge: " ++ show op

  -- appConst OConcat                            = _wG
  -- appConst (OBagUnions ty)                    = _wH
  -- appConst (OUnions ty)                       = _wI

  --------------------------------------------------
  -- Container conversions

  OBagToSet -> \(VBag cs) -> out . VBag . (map . second) (const 1) $ cs
  OSetToList -> \(VBag cs) -> out . listv id . map fst $ cs
  OBagToList -> \(VBag cs) -> out . listv id . concatMap (uncurry (flip (replicate . fromIntegral))) $ cs
  OListToSet -> out . VBag . (map . fmap) (const 1) . countValues . vlist id
  OListToBag -> out . VBag . countValues . vlist id
  -- Bag a -> Set (a, N)
  OBagToCounts -> \(VBag cs) -> out . VBag . map ((,1) . pairv id intv) $ cs
  -- Bag (a, N) -> Bag a
  --   Notionally this takes a set of pairs instead of a bag, but operationally we need to
  --   be prepared for a bag, because of the way literal bags desugar, e.g.
  --
  --   Disco> :desugar let x = 3 in ⟅ 'a' # (2 + x), 'b', 'b' ⟆
  --   (λx. bagFromCounts(bag(('a', 2 + x) :: ('b', 1) :: ('b', 1) :: [])))(3)

  OCountsToBag -> \(VBag cs) ->
    out . VBag . sortNCount
      . map (second (uncurry (*)) . assoc . first (vpair id vint))
      $ cs
    where
      assoc ((a, b), c) = (a, (b, c))

  -- appConst (OMapToSet ty ty')                 = _w10
  -- appConst OSetToMap                          = _w11

  --------------------------------------------------
  -- Graph operations

  -- appConst OSummary                           = _wJ
  -- appConst (OEmptyGraph ty)                   = _wK
  -- appConst (OVertex ty)                       = _wL
  -- appConst (OOverlay ty)                      = _wM
  -- appConst (OConnect ty)                      = _wN
  -- appConst OEmptyMap                          = _wO
  -- appConst OInsert                            = _wP
  -- appConst OLookup                            = _wQ
  -- appConst (OForall tys)                      = _w15
  -- appConst (OExists tys)                      = _w16
  -- appConst OHolds                             = _w17
  -- appConst ONotProp                           = _w18
  -- appConst (OShouldEq ty)                     = _w19
  -- appConst OMatchErr                          = _w1a

  c -> error $ "Unimplemented: appConst " ++ show c
  where
    out v = return $ Out v k

--------------------------------------------------
-- Arithmetic

intOp1 :: (Integer -> Value) -> Value -> Sem r Value
intOp1 f = intOp1' (return . f)

intOp1' :: (Integer -> Sem r Value) -> Value -> Sem r Value
intOp1' f = f . vint

numOp1 :: (Rational -> Rational) -> Value -> Sem r Value
numOp1 f = numOp1' $ return . ratv . f

numOp1' :: (Rational -> Sem r Value) -> Value -> Sem r Value
numOp1' f (VNum d m) = f m

numOp2 :: (Rational -> Rational -> Rational) -> Value -> Sem r Value
numOp2 (#) = numOp2' $ \m n -> return (ratv (m # n))

numOp2' :: (Rational -> Rational -> Sem r Value) -> Value -> Sem r Value
numOp2' (#) =
  arity2 $ \(VNum d1 n1) (VNum d2 n2) -> do
    res <- n1 # n2
    case res of
      VNum _ r -> return $ VNum (d1 <> d2) r
      _        -> return res

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
      iters = iterate newtonStep (integerSqrt' (div n lowerN) * lowerRoot)
      isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
   in head $ dropWhile (not . isRoot) iters

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
valCmp v1 v2 = error $ "valCmp " ++ show v1 ++ " " ++ show v2

compareBags :: [(Value, Integer)] -> [(Value, Integer)] -> Ordering
compareBags [] [] = EQ
compareBags [] _ = LT
compareBags _ [] = GT
compareBags ((x, xn) : xs) ((y, yn) : ys) = valCmp x y <> compare xn yn <> compareBags xs ys

-- VConst, VClo, VFun_
-- VBag, VGraph, VMap

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
babbage []       = []
babbage [x]      = repeat x
babbage (x : xs) = scanl (+) x (babbage (diff (x : xs)))

-- | Compute the forward difference of the given sequence, that is,
--   differences of consecutive pairs of elements.
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

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

-- | Looks up a sequence of integers in OEIS.
--   Returns 'left()' if the sequence is unknown in OEIS,
--   otherwise 'right "https://oeis.org/<oeis_sequence_id>"'
oeisLookup :: Value -> Value
oeisLookup (vlist vint -> ns) = maybe VNil parseResult (lookupSequence ns)
  where
    parseResult r = VInj R (listv charv ("https://oeis.org/" ++ seqNum r))
    seqNum = getCatalogNum . catalogNums

    getCatalogNum []      = error "No catalog info"
    getCatalogNum (n : _) = n

-- | Extends a Disco integer list with data from a known OEIS
--   sequence.  Returns a list of integers upon success, otherwise the
--   original list (unmodified).
oeisExtend :: Value -> Value
oeisExtend = listv intv . extendSequence . vlist vint

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

------------------------------------------------------------
-- Tests
------------------------------------------------------------

runTest :: Members EvalEffects r => Int -> AProperty -> Sem r TestResult
runTest n p = undefined

-- testProperty (Randomized n' n') =<< mkValue (compileProperty p)
-- where
--   n' = fromIntegral (n `div` 2)

{-

  -- | A "test frame" under which a test case is run. Records the
  --   types and legible names of theariables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core

-}

------------------------------------------------------------
-- XXX
------------------------------------------------------------

eval :: Members '[Error EvalError, Input Env, State Mem] r => Core -> Sem r Value
eval c = do
  e <- input @Env
  runFresh $ runCESK (In c e [])

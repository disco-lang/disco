{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interpret.CESK
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- CESK machine interpreter for Disco.
--
-----------------------------------------------------------------------------

module Disco.Interpret.CESK
  ( Mem

  , CESK, runCESK, step, eval

  , runTest
  ) where

import           Debug.Trace

import           Control.Arrow                      ((***))
import           Data.IntMap                        (IntMap)
import qualified Data.IntMap                        as IM
import           Data.Map                           (Map, (!))
import qualified Data.Map                           as M
import           Data.Ratio

import           Math.Combinatorics.Exact.Binomial  (choose)
import           Math.Combinatorics.Exact.Factorial (factorial)
import           Math.NumberTheory.Primes           (factorise, unPrime)
import           Math.NumberTheory.Primes.Testing   (isPrime)

import           Disco.Effects.Fresh
import           Disco.Effects.Input
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           Unbound.Generics.LocallyNameless   (Bind, Name, bind)

import           Disco.AST.Core
import           Disco.AST.Generic                  (Side (..), selectSide)
import           Disco.AST.Typed                    (AProperty)
import           Disco.Enumerate
import           Disco.Error
import           Disco.Types                        hiding (V)
import           Disco.Value

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
  -- | Inject the value into a sum type.
  = FInj Side

  -- | Do a case analysis on the value.
  | FCase Env (Bind (Name Core) Core) (Bind (Name Core) Core)

  -- | Evaluate the right-hand value of a pair once we have finished
  --   evaluating the left-hand side.
  | FPairR Env Core

  -- | Put the value into the right-hand side of a pair together with
  --   this previously evaluated left-hand side.
  | FPairL Value

  -- | Project one or the other side of a pair.
  | FProj Side

  -- | Evaluate the argument of an application once we have finished
  --   evaluating the function.
  | FArg Env Core

  -- | Apply a previously evaluated function to the value.
  | FApp Value

  -- | Force evaluation of the contents of a memory cell.
  | FForce

  -- | Update the contents of a memory cell with its evaluation.
  | FUpdate Int
  deriving Show

------------------------------------------------------------
-- Memory
------------------------------------------------------------

-- | 'Mem' represents a memory, containing 'Cell's
data Mem = Mem { next :: Int, mu :: IntMap Cell } deriving Show
data Cell = Blackhole | E Env Core | V Value deriving Show

emptyMem :: Mem
emptyMem = Mem 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the updated memory and the
--   index of the allocated cell.
allocate :: Members '[State Mem] r => Env -> Core -> Sem r Int
allocate e t = do
  Mem n m <- get
  put $ Mem (n+1) (IM.insert n (E e t) m)
  return n

-- | XXX
allocateRec :: Members '[State Mem] r => Name Core -> Env -> Core -> Sem r Int
allocateRec x e c = do
  Mem n m <- get
  put $ Mem (n+1) (IM.insert n (E (M.insert x (VRef n) e) c) m)
  return n

-- | Look up the cell at a given index.
lkup :: Members '[State Mem] r => Int -> Sem r (Maybe Cell)
lkup n = gets (IM.lookup n . mu)

-- | Set the cell at a given index.
set :: Members '[State Mem] r => Int -> Cell -> Sem r ()
set n c = modify $ \(Mem nxt m) -> Mem nxt (IM.insert n c m)

------------------------------------------------------------
-- The CESK machine
------------------------------------------------------------

-- | The CESK machine has two basic kinds of states.
data CESK
  -- | The 'In' constructor represents the state when we are recursing
  --   "into" a term.  There is a currently focused expression which
  --   is to be evaluated in the given context.  Generally, evaluation
  --   proceeds by pattern-matching on the focused expression and
  --   either immediately turning it into a value (if it is simple),
  --   or focusing on a subexpression and pushing a new frame on the
  --   continuation stack indicating how to continue evaluating the
  --   whole expression once finished with the subexpression.
  = In Core Env Cont

  -- | The 'Out' constructor represents the state when we have
  --   completed evaluating an expression and are now on our way back
  --   "out" of the recursion.  Generally, evaluation proceeds by
  --   pattern-matching on the top frame of the continuation stack
  --   (and sometimes on the value as well), to see what is to be done
  --   with the value.
  | Out Value Cont
  deriving Show

-- | Is the CESK machine in a final state?
isFinal :: CESK -> Maybe Value
isFinal (Out v []) = Just v
isFinal _          = Nothing

-- | Run a CESK machine to completion.
runCESK :: Members '[Fresh, Error EvalError, State Mem] r => CESK -> Sem r Value
runCESK cesk = case isFinal cesk of
  Just vm -> return vm
  Nothing -> do
    traceShowM cesk
    step cesk >>= runCESK

(!!!) :: (Ord k, Show k) => Map k v -> k -> v
m !!! k = case M.lookup k m of
  Nothing -> error $ "variable not found in environment: " ++ show k
  Just v  -> v

-- | Advance the CESK machine by one step.
step :: Members '[Fresh, Error EvalError, State Mem] r => CESK -> Sem r CESK
step (In (CVar x) e k)                   = return $ Out (e!!!x) k
step (In (CNum d r) _ k)                 = return $ Out (VNum d r) k
step (In (CConst op) _ k)                = return $ Out (VConst op) k
step (In (CInj s c) e k)                 = return $ In c e (FInj s : k)
step (In (CCase c b1 b2) e k)            = return $ In c e (FCase e b1 b2 : k)
step (In CUnit _ k)                      = return $ Out VUnit k
step (In (CPair c1 c2) e k)              = return $ In c1 e (FPairR e c2 : k)
step (In (CProj s c) e k)                = return $ In c e (FProj s : k)
step (In (CAbs b) e k)                   = do
  (xs, body) <- unbind b
  return $ Out (VClo e xs body) k
step (In (CApp c1 c2) e k)               = return $ In c1 e (FArg e c2 : k)
step (In (CType ty) _ k)                 = return $ Out (VType ty) k
step (In (CDelay b) e k)                 = do
  (x, c) <- unbind b
  loc <- allocateRec x e c
  return $ Out (VRef loc) k
step (In (CForce c) e k)                 = return $ In c e (FForce : k)

step (Out v (FInj s : k))                = return $ Out (VInj s v) k
step (Out (VInj L v) (FCase e b1 _ : k)) = do
  (x,c1) <- unbind b1
  return $ In c1 (M.insert x v e) k
step (Out (VInj R v) (FCase e _ b2 : k)) = do
  (x,c2) <- unbind b2
  return $ In c2 (M.insert x v e) k
step (Out v1 (FPairR e c2 : k))          = return $ In c2 e (FPairL v1 : k)
step (Out v2 (FPairL v1 : k))            = return $ Out (VPair v1 v2) k
step (Out (VPair v1 v2) (FProj s : k))   = return $ Out (selectSide s v1 v2) k
step (Out v (FArg e c2 : k))             = return $ In c2 e (FApp v : k)
step (Out v2 (FApp (VClo e [x] b) : k))  = return $ In b (M.insert x v2 e) k
step (Out v2 (FApp (VClo e (x:xs) b) : k)) = return $ Out (VClo (M.insert x v2 e) xs b) k
step (Out v2 (FApp (VConst op) : k))     = Out <$> appConst op v2 <*> pure k
step (Out (VRef n) (FForce : k))         = do
  cell <- lkup n
  case cell of
    Nothing        -> undefined  -- XXX ?
    Just (V v)     -> return $ Out v k
    Just (E e t)   -> do
      set n Blackhole
      return $ In t e (FUpdate n : k)
    Just Blackhole -> error "Infinite loop detected!"  -- XXX make a real error
step (Out v (FUpdate n : k))             = do
  set n (V v)
  return $ Out v k

------------------------------------------------------------
-- Interpreting constants
------------------------------------------------------------

arity2 :: (Value -> Value -> a) -> Value -> a
arity2 f (VPair x y) = f x y
arity2 f v           = error "arity2 on a non-pair!"  -- XXX

appConst :: Member (Error EvalError) r => Op -> Value -> Sem r Value

--------------------------------------------------
-- Arithmetic

appConst OAdd   = numOp2 (+)
appConst ONeg   = numOp1 negate
appConst OSqrt  = numOp1 integerSqrt
appConst OFloor = numOp1 $ (%1) . floor
appConst OCeil  = numOp1 $ (%1) . ceiling
appConst OAbs   = numOp1 abs
appConst OMul   = numOp2 (*)
appConst ODiv   = numOp2' divOp
  where
    divOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
    divOp _ 0 = throw DivByZero
    divOp m n = return $ ratv (m / n)
appConst OExp   = numOp2 $ \m n -> m ^^ numerator n
appConst OMod   = numOp2' modOp
  where
    modOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
    modOp m n
      | n == 0    = throw DivByZero
      | otherwise = return $ intv (numerator m `mod` numerator n)
appConst ODivides = numOp2' $ \m n -> return (enumv $ divides m n)
  where
    divides 0 0 = True
    divides 0 _ = False
    divides x y = denominator (y / x) == 1

--------------------------------------------------
-- Number theory

appConst OIsPrime = intOp1 (enumv . isPrime)
appConst OFactor  = intOp1' primFactor
  where
    -- Semantics of the @$factor@ prim: turn a natural number into its
    -- bag of prime factors.  Crash if called on 0, which does not have
    -- a prime factorization.
    primFactor :: Member (Error EvalError) r => Integer -> Sem r Value
    primFactor 0 = throw (Crash "0 has no prime factorization!")
    prinFactor n = return . VBag $ map ((intv . unPrime) *** fromIntegral) (factorise n)

appConst OFrac    = numOp1' (return . primFrac)
  where
    -- Semantics of the @$frac@ prim: turn a rational number into a pair
    -- of its numerator and denominator.
    primFrac :: Rational -> Value
    primFrac r = VPair (intv (numerator r)) (intv (denominator r))

--------------------------------------------------
-- Combinatorics

appConst OMultinom                          = arity2 multinomOp
appConst OFact                              = numOp1' factOp
  where
    factOp :: Member (Error EvalError) r => Rational -> Sem r Value
    factOp (numerator -> n)
      | n > fromIntegral (maxBound :: Int) = throw Overflow
      | otherwise = return . intv $ factorial (fromIntegral n)

appConst OEnum                             = return . enumOp
  where
    enumOp :: Value -> Value
    enumOp (VType ty) = listv id (enumerateType ty)
    enumOp v          = error $ "Impossible! enumOp on non-type " ++ show v  -- XXX

appConst OCount                             = return . countOp
  where
    countOp :: Value -> Value
    countOp (VType ty) = case countType ty of
      Just num -> VInj R (intv num)
      Nothing  -> VNil
    countOp v = error $ "Impossible! countOp on non-type " ++ show v

--------------------------------------------------
-- Graphs

--------------------------------------------------
-- Maps

--------------------------------------------------
-- Comparison

appConst (OEq ty)                          = arity2 $ \v1 v2 -> enumv <$> valEq ty v1 v2
appConst (OLt ty)                          = arity2 $ \v1 v2 -> enumv <$> valLt ty v1 v2

--------------------------------------------------
-- Container operations

appConst OSize                              = return . sizeOp
  where
    sizeOp (VBag xs) = intv (sum (map snd xs))

-- appConst (OPower ty)                        = _wv
-- appConst (OBagElem ty)                      = _ww
-- appConst (OListElem ty)                     = _wx

--------------------------------------------------
-- Container conversions

-- appConst OBagToSet                          = return .
-- appConst OSetToList                         = _wT
-- appConst OBagToList                         = _wV
-- appConst (OListToSet ty)                    = _wW
-- appConst (OListToBag ty)                    = _wX
-- appConst OBagToCounts                       = _wY
-- appConst (OCountsToBag ty)                  = _wZ
-- appConst (OMapToSet ty ty')                 = _w10
-- appConst OSetToMap                          = _w11

--------------------------------------------------

-- appConst OEachList                          = _wy
-- appConst (OEachBag ty)                      = _wz
-- appConst (OEachSet ty)                      = _wA
-- appConst OReduceList                        = _wB
-- appConst OReduceBag                         = _wC
-- appConst OFilterList                        = _wD
-- appConst OFilterBag                         = _wE
-- appConst (OMerge ty)                        = _wF
-- appConst OConcat                            = _wG
-- appConst (OBagUnions ty)                    = _wH
-- appConst (OUnions ty)                       = _wI
-- appConst OSummary                           = _wJ
-- appConst (OEmptyGraph ty)                   = _wK
-- appConst (OVertex ty)                       = _wL
-- appConst (OOverlay ty)                      = _wM
-- appConst (OConnect ty)                      = _wN
-- appConst OEmptyMap                          = _wO
-- appConst OInsert                            = _wP
-- appConst OLookup                            = _wQ
-- appConst OForever                           = _wR
-- appConst OUntil                             = _wS
-- appConst OIsPrime                           = _w12
-- appConst OFactor                            = _w13
-- appConst OFrac                              = _w14
-- appConst (OForall tys)                      = _w15
-- appConst (OExists tys)                      = _w16
-- appConst OHolds                             = _w17
-- appConst ONotProp                           = _w18
-- appConst (OShouldEq ty)                     = _w19
-- appConst OMatchErr                          = _w1a

appConst OCrash                                = throw . Crash . vlist vchar

-- appConst OId                                = _w1c
-- appConst OLookupSeq                         = _w1d
-- appConst OExtendSeq                         = _w1e

appConst c = error $ "Unimplemented: appConst " ++ show c

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
numOp2' (#)
  = arity2 $ \(VNum d1 n1) (VNum d2 n2) -> do
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
  let twopows = iterate (^!2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (integerSqrt' (div n lowerN ) * lowerRoot)
      isRoot r = r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

-- this operator is used for `integerSqrt'`
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

-- | Multinomial coefficient.  The first argument is a number, the
--   second is a list.
multinomOp :: Value -> Value -> Sem r Value
multinomOp (vint -> n) (vlist vint -> ks) = return . intv $ multinomial n ks
  where
    multinomial :: Integer -> [Integer] -> Integer
    multinomial _ []     = 1
    multinomial n (k:ks)
      | k > n     = 0
      | otherwise = choose n k * multinomial (n-k) ks

------------------------------------------------------------
-- Comparison
------------------------------------------------------------

valEq :: Type -> Value -> Value -> Sem r Bool

-- XXX make list a defined type
valEq (TyList tyElt) v1 v2 = valEq (TyUnit :+: tyElt :*: TyList tyElt) v1 v2

valEq _ (VNum _ r1) (VNum _ r2) = return $ r1 == r2
valEq (ty1 :+: _) (VInj L v1) (VInj L v2) = valEq ty1 v1 v2
valEq (_ :+: ty2) (VInj R v1) (VInj R v2) = valEq ty2 v1 v2
valEq _ VUnit VUnit = return True
valEq (ty1 :*: ty2) (VPair v11 v12) (VPair v21 v22)
  = (&&) <$> valEq ty1 v11 v21 <*> valEq ty2 v12 v22
valEq _ (VType ty1) (VType ty2) = return $ ty1 == ty2
valEq _ _ _ = return False

-- VConst, VClo, VFun_
-- VBag, VGraph, VMap

valLt :: Type -> Value -> Value -> Sem r Bool
valLt ty v1 v2 = (==LT) <$> valOrd ty v1 v2

-- XXX combine valEq and valOrd ?
valOrd :: Type -> Value -> Value -> Sem r Ordering
valOrd (TyList tyElt) v1 v2 = valOrd (TyUnit :+: tyElt :*: TyList tyElt) v1 v2
valOrd _ (VNum _ r1) (VNum _ r2) = return $ compare r1 r2
valOrd _ (VInj L _) (VInj R _) = return LT
valOrd _ (VInj R _) (VInj L _) = return GT
valOrd (ty1 :+: _) (VInj L v1) (VInj L v2) = valOrd ty1 v1 v2
valOrd (_ :+: ty2) (VInj R v1) (VInj R v2) = valOrd ty2 v1 v2
valOrd _ VUnit VUnit = return EQ
valOrd (ty1 :*: ty2) (VPair v11 v12) (VPair v21 v22) =
  (<>) <$> valOrd ty1 v11 v21 <*> valOrd ty2 v12 v22
valOrd _ (VType ty1) (VType ty2) = return $ compare ty1 ty2
valOrd _ _ _ = return EQ  -- XXX

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

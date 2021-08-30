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
  ( CESK, runCESK, step, eval

  , runTest
  ) where

import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IM
import           Data.Map                         (Map, (!))
import qualified Data.Map                         as M

import           Polysemy
import           Polysemy.Error
import           Unbound.Generics.LocallyNameless (Bind, Name)

import           Disco.AST.Core
import           Disco.AST.Generic                (Side (..), selectSide)
import           Disco.AST.Typed                  (AProperty)
import           Disco.Effects.Fresh
import           Disco.Error
import           Disco.Value

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

------------------------------------------------------------
-- Memory
------------------------------------------------------------

-- | 'Mem' represents a memory, containing 'Cell's
data Mem = Mem { next :: Int, mu :: IntMap Cell }
data Cell = Blackhole | E Env Core | V Value

emptyMem :: Mem
emptyMem = Mem 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the updated memory and the
--   index of the allocated cell.
allocate :: Env -> Core -> Mem -> (Mem, Int)
allocate e t (Mem n m) = (Mem (n+1) (IM.insert n (E e t) m), n)

-- | Look up the cell at a given index.
lkup :: Int -> Mem -> Maybe Cell
lkup n (Mem _ m) = IM.lookup n m

-- | Set the cell at a given index.
set :: Int -> Cell -> Mem -> Mem
set n c (Mem nxt m) = Mem nxt (IM.insert n c m)

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
  = In Core Env Mem Cont

  -- | The 'Out' constructor represents the state when we have
  --   completed evaluating an expression and are now on our way back
  --   "out" of the recursion.  Generally, evaluation proceeds by
  --   pattern-matching on the top frame of the continuation stack
  --   (and sometimes on the value as well), to see what is to be done
  --   with the value.
  | Out Value Mem Cont

-- | Is the CESK machine in a final state?
isFinal :: CESK -> Maybe Value
isFinal (Out v _ []) = Just v
isFinal _            = Nothing

-- | Run a CESK machine to completion.
runCESK :: Members '[Fresh, Error EvalError] r => CESK -> Sem r Value
runCESK cesk = case isFinal cesk of
  Just v  -> return v
  Nothing -> step cesk >>= runCESK

-- | Advance the CESK machine by one step.
step :: Members '[Fresh, Error EvalError] r => CESK -> Sem r CESK
step (In (CVar x) e m k)                   = return $ Out (e!x) m k
step (In (CNum d r) _ m k)                 = return $ Out (VNum d r) m k
step (In (CConst op) _ m k)                = return $ Out (VConst op) m k
step (In (CInj s c) e m k)                 = return $ In c e m (FInj s : k)
step (In (CCase c b1 b2) e m k)            = return $ In c e m (FCase e b1 b2 : k)
step (In CUnit _ m k)                      = return $ Out VUnit m k
step (In (CPair c1 c2) e m k)              = return $ In c1 e m (FPairR e c2 : k)
step (In (CProj s c) e m k)                = return $ In c e m (FProj s : k)
step (In (CAbs b) e m k)                   = return $ Out (VClo e b) m k
step (In (CApp c1 c2) e m k)               = return $ In c1 e m (FArg e c2 : k)
step (In (CType ty) _ m k)                 = return $ Out (VType ty) m k
step (In (CDelay b) e m k)                 = do
  (x, c) <- unbind b
  let (m', loc) = allocate (M.insert x (VRef loc) e) c m
  return $ Out (VRef loc) m' k
step (In (CForce c) e m k)                 = return $ In c e m (FForce : k)

step (Out v m (FInj s : k))                = return $ Out (VInj s v) m k
step (Out (VInj L v) m (FCase e b1 _ : k)) = do
  (x,c1) <- unbind b1
  return $ In c1 (M.insert x v e) m k
step (Out (VInj R v) m (FCase e _ b2 : k)) = do
  (x,c2) <- unbind b2
  return $ In c2 (M.insert x v e) m k
step (Out v1 m (FPairR e c2 : k))          = return $ In c2 e m (FPairL v1 : k)
step (Out v2 m (FPairL v1 : k))            = return $ Out (VPair v1 v2) m k
step (Out (VPair v1 v2) m (FProj s : k))   = return $ Out (selectSide s v1 v2) m k
step (Out v m (FArg e c2 : k))             = return $ In c2 e m (FApp v : k)
step (Out v2 m (FApp (VClo e b) : k))      = do
  -- Any closure we are evaluating here must have a single argument.
  -- Multi-argument CAbs terms are only used for quantifiers.
  (xs,c1) <- unbind b
  let [x] = xs
  return $ In c1 (M.insert x v2 e) m k
step (Out v2 m (FApp (VConst op) : k))     = return $ Out (appConst op v2) m k
step (Out (VRef n) m (FForce : k))         =
  case lkup n m of
    Nothing        -> undefined  -- XXX ?
    Just (V v)     -> return $ Out v m k
    Just (E e t)   -> return $ In t e (set n Blackhole m) (FUpdate n : k)
    Just Blackhole -> error "Infinite loop detected!"  -- XXX make a real error
step (Out v m (FUpdate n : k))             = return $ Out v (set n (V v) m) k

appConst :: Op -> Value -> Value
appConst OAdd (VPair (VNum d1 x) (VNum d2 y)) = VNum (d1 <> d2) (x + y)
-- appConst ONeg v                               = _wb
-- appConst OSqrt v                              = _wc
-- appConst OFloor v                             = _wd
-- appConst OCeil v                              = _we
-- appConst OAbs v                               = _wf
-- appConst OMul v                               = _wg
-- appConst ODiv v                               = _wh
-- appConst OExp v                               = _wi
-- appConst OMod v                               = _wj
-- appConst ODivides v                           = _wk
-- appConst OMultinom v                          = _wl
-- appConst OFact v                              = _wm
-- appConst (OEq ty) v                           = _wn
-- appConst (OLt ty) v                           = _wo
-- appConst OEnum v                              = _wp
-- appConst OCount v                             = _wq
-- appConst (OMDiv n) v                          = _wr
-- appConst (OMExp n) v                          = _ws
-- appConst (OMDivides n) v                      = _wt
-- appConst OSize v                              = _wu
-- appConst (OPower ty) v                        = _wv
-- appConst (OBagElem ty) v                      = _ww
-- appConst (OListElem ty) v                     = _wx
-- appConst OEachList v                          = _wy
-- appConst (OEachBag ty) v                      = _wz
-- appConst (OEachSet ty) v                      = _wA
-- appConst OReduceList v                        = _wB
-- appConst OReduceBag v                         = _wC
-- appConst OFilterList v                        = _wD
-- appConst OFilterBag v                         = _wE
-- appConst (OMerge ty) v                        = _wF
-- appConst OConcat v                            = _wG
-- appConst (OBagUnions ty) v                    = _wH
-- appConst (OUnions ty) v                       = _wI
-- appConst OSummary v                           = _wJ
-- appConst (OEmptyGraph ty) v                   = _wK
-- appConst (OVertex ty) v                       = _wL
-- appConst (OOverlay ty) v                      = _wM
-- appConst (OConnect ty) v                      = _wN
-- appConst OEmptyMap v                          = _wO
-- appConst OInsert v                            = _wP
-- appConst OLookup v                            = _wQ
-- appConst OForever v                           = _wR
-- appConst OUntil v                             = _wS
-- appConst OSetToList v                         = _wT
-- appConst OBagToSet v                          = _wU
-- appConst OBagToList v                         = _wV
-- appConst (OListToSet ty) v                    = _wW
-- appConst (OListToBag ty) v                    = _wX
-- appConst OBagToCounts v                       = _wY
-- appConst (OCountsToBag ty) v                  = _wZ
-- appConst (OMapToSet ty ty') v                 = _w10
-- appConst OSetToMap v                          = _w11
-- appConst OIsPrime v                           = _w12
-- appConst OFactor v                            = _w13
-- appConst OFrac v                              = _w14
-- appConst (OForall tys) v                      = _w15
-- appConst (OExists tys) v                      = _w16
-- appConst OHolds v                             = _w17
-- appConst ONotProp v                           = _w18
-- appConst (OShouldEq ty) v                     = _w19
-- appConst OMatchErr v                          = _w1a
-- appConst OCrash v                             = _w1b
-- appConst OId v                                = _w1c
-- appConst OLookupSeq v                         = _w1d
-- appConst OExtendSeq v                         = _w1e

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
  --   types and legible names of the variables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core

-}

------------------------------------------------------------
-- XXX
------------------------------------------------------------

eval :: Members '[Error EvalError] r => Core -> Sem r Value
eval c = runFresh $ runCESK (In c M.empty emptyMem [])

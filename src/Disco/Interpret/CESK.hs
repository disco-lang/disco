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

module Disco.Interpret.CESK where

import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IntMap
import qualified Data.IntMap                      as IM
import           Data.Map                         (Map, (!))
import qualified Data.Map                         as M

import           Polysemy
import           Polysemy.Error
import           Unbound.Generics.LocallyNameless (Bind, Name)

import           Disco.AST.Core
import           Disco.AST.Generic                (Side (..), selectSide)
import           Disco.Effects.Fresh
import           Disco.Error
import           Disco.Types                      (Type)

------------------------------------------------------------
-- Values
------------------------------------------------------------

-- | Different types of values which can result from the evaluation
--   process.
data Value where
  VNum     :: RationalDisplay -> Rational -> Value
  VConst   :: Op -> Value
  VInj     :: Side -> Value -> Value
  VUnit    :: Value
  VPair    :: Value -> Value -> Value
  VClo     :: Env -> Bind [Name Core] Core -> Value
  VType    :: Type -> Value
  VRef     :: Int -> Value

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
-- Environment and memory
------------------------------------------------------------

-- | An 'Env' simply maps names to values.
type Env = Map (Name Core) Value

-- | 'Mem' represents a memory, containing 'Cell'
data Mem = Mem { next :: Int, mu :: IntMap Cell }
data Cell = Blackhole | E Env Core | V Value

allocate :: Env -> Core -> Mem -> (Mem, Int)
allocate e t (Mem n m) = (Mem (n+1) (IntMap.insert n (E e t) m), n)

lkup :: Int -> Mem -> Maybe Cell
lkup n (Mem _ m) = IntMap.lookup n m

set :: Int -> Cell -> Mem -> Mem
set n c (Mem nxt m) = Mem nxt (IntMap.insert n c m)

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

isFinal :: CESK -> Maybe Value
isFinal (Out v _ []) = Just v
isFinal _            = Nothing

step :: Members '[Fresh, Error EvalError] r => CESK -> Sem r CESK
step (In (CVar x) e m k)                   = return $ Out (e!x) m k
step (In (CNum d r) e m k)                 = return $ Out (VNum d r) m k
step (In (CConst op) e m k)                = return $ Out (VConst op) m k
step (In (CInj s c) e m k)                 = return $ In c e m (FInj s : k)
step (In (CCase c b1 b2) e m k)            = return $ In c e m (FCase e b1 b2 : k)
step (In CUnit e m k)                      = return $ Out VUnit m k
step (In (CPair c1 c2) e m k)              = return $ In c1 e m (FPairR e c2 : k)
step (In (CProj s c) e m k)                = return $ In c e m (FProj s : k)
step (In (CAbs b) e m k)                   = return $ Out (VClo e b) m k
step (In (CApp c1 c2) e m k)               = return $ In c1 e m (FArg e c2 : k)
step (In (CType ty) e m k)                 = return $ Out (VType ty) m k
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
appConst = undefined

{-

  -- | A "test frame" under which a test case is run. Records the
  --   types and legible names of the variables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core

-}

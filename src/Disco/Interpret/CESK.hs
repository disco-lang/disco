{-# LANGUAGE NondecreasingIndentation #-}

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

import qualified Data.IntMap    as IntMap
import qualified Data.Map       as M

import           Polysemy
import           Polysemy.Error

import           Disco.AST.Core

data Value where
  VConst   :: Op -> Value
  VUnit    :: Value
  VInj     :: Side -> Value -> Value
  VNum     :: RationalDisplay -> Rational -> Value
  VClo     :: Env -> Bind [Name Core] Core -> Value
  VNoMatch :: Value
  VMatch   :: Env -> Value
  VType    :: Type -> Value
  VCell    :: Int -> Value

data Frame
  = FInj Side
  | FPairL Env Core
  | FPairR Value
  | FArg Env Core
  | FCase Env [CBranch]
  | FBranch Env Core
  | FMatch CPattern [(Embed Core, CPattern)]
  | FForce

type Cont = [Frame]

type Env = Map (Name Core) Value

data Mem = Mem { next :: Int, mu :: IntMap Cell }

data Cell = Blackhole | E Env Core | V Value

allocate :: Env -> Expr -> Mem -> (Mem, Int)
allocate e t (Mem n m) = (Mem (n+1) (IntMap.insert n (E e t) m), n)

data Control
  = CnTerm Core
  | CnBranch CBranch
  | CnGuards [(Embed Core, CPattern)]

data CESK = In Control Env Mem Cont | Out Value Mem Cont

isFinal :: CESK -> Maybe Value
isFinal (Out v _ []) = Just v
isFinal _            = Nothing

step :: Members '[Fresh, Error EvalError] r => CESK -> Sem r CESK
step (In (CnTerm (CVar x)) e m k)      = return $ Out (e!x) m k
step (In (CnTerm (CConst op)) e m k)   = return $ Out (VConst op) m k
step (In (CnTerm CUnit) e m k)         = return $ Out VUnit m k
step (In (CnTerm (CInj s c)) e m k)    = return $ In c e m (FInj s : k)
step (In (CnTerm (CPair c1 c2)) e m k) = return $ In c1 e m (FPairL e c2 : k)
step (In (CnTerm (CNum d r)) e m k)    = return $ Out (VNum d r) m k
step (In (CnTerm (CAbs b)) e m k)      = return $ Out (VClo e b) m k
step (In (CnTerm (CApp c1 c2)) e m k)  = return $ In c1 e m (FArg e c2 : k)
step (In (CnTerm (CCase [])) e m k)    = throw NonExhaustive
step (In (CnTerm (CCase (br:brs)) e m k)) = return $ In (CnBranch br) e m (FCase e brs : k)
step (In (CnBranch br) e m k) = do
  (gs, c) <- unbind br
  return $ In (CnGuards (fromTelescope gs)) e m (FBranch e c : k)
step (In (CnGuards []) e m k) = return $ Out (VMatch e) m k
step (In (CnGuards ((unembed -> c, p) : gs)) e m k)
  = return $ In c e m (FMatch p gs : k)
step (In (CType ty) e m k) = return $ Out (VType ty) m k
step (In (CDelay b) e m k) = do
  (x, c) <- unbind b
  let (m', loc) = allocate (M.insert x (VCell loc) e) c m
  return $ Out (VCell loc) m' k
step (In (CnTerm (EForce c)) e m k) = In (CnTerm c) e m (FForce : k)

step (Out v m (FInj s : k)) = Out (VInj s v) m k
step (


{-

  -- | A "test frame" under which a test case is run. Records the
  --   types and legible names of the variables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core

-}

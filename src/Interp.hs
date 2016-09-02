{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Interp where

import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Ratio              (Ratio)

import           Unbound.LocallyNameless (Name)

import           Types

data Value where
  VUnit :: Value
  VBool :: Bool -> Value
  VAbs  :: Value -> Value
  VPair :: Value -> Value -> Value
  VNat  :: Integer -> Value
  VInt  :: Integer -> Value
  VRat  :: Rational -> Value

data InterpError where
  UnboundError :: Name Term -> InterpError

type Env = M.Map (Name Term) Value

interpTerm :: Term -> Either InterpError Value
interpTerm = interpTerm' M.empty

interpTerm' :: Env -> Term -> Either InterpError Value
interpTerm' e (TVar x)   = maybe (Left $ UnboundError x) Right (M.lookup x e)
interpTerm' _ TUnit      = Right VUnit
interpTerm' _ (TBool b)  = Right (VBool b)
interpTerm' e (TAbs abs) = undefined
interpTerm' e (TApp f x) = undefined

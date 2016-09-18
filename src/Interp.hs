{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Interp where

import           Control.Monad           (join)
import qualified Data.Map                as M
import           Data.Ratio              ((%))

import           Unbound.LocallyNameless (Name)

import           Types

data Value where
  VUnit :: Value
  VBool :: Bool -> Value
  VAbs  :: Value -> Value
  VPair :: Value -> Value -> Value
  VInj  :: Side -> Value -> Value
  VNum  :: Rational -> Value

data InterpError where
  UnboundError :: Name Term -> InterpError
  NumError     :: Value     -> InterpError  -- ^ v should be a number, but isn't
  DivByZero    :: InterpError
  BoolError    :: Value     -> InterpError  -- ^ should be a boolean, but isn't

type Env = M.Map (Name Term) Value

interpTerm :: Term -> Either InterpError Value
interpTerm = interpTerm' M.empty

interpTerm' :: Env -> Term -> Either InterpError Value
interpTerm' e (TVar x)      = maybe (Left $ UnboundError x) Right (M.lookup x e)
interpTerm' _ TUnit         = return VUnit
interpTerm' _ (TBool b)     = return $ VBool b
interpTerm' e (TAbs abs)    = undefined
interpTerm' e (TJuxt f x)   = undefined
interpTerm' e (TPair l r)   = VPair <$> interpTerm' e l <*> interpTerm' e r
interpTerm' e (TInj s t)    = VInj s <$> interpTerm' e t
interpTerm' _ (TNat i)      = return $ VNum (i % 1)
interpTerm' e (TUn op t)    = interpTerm' e t >>= interpUOp op
interpTerm' e (TBin op l r) = join (interpBOp op <$> interpTerm' e l <*> interpTerm' e r)
interpTerm' e (TLet t)      = undefined
interpTerm' e (TCase bs)    = undefined
interpTerm' e (TAscr t _)   = interpTerm' e t

interpUOp :: UOp -> Value -> Either InterpError Value
interpUOp Neg (VNum n) = return $ VNum (-n)
interpUOp Neg v        = Left $ NumError v

interpBOp :: BOp -> Value -> Value -> Either InterpError Value
interpBOp Add    = numOp (+)
interpBOp Sub    = numOp (-)
interpBOp Mul    = numOp (*)
interpBOp Div    = divOp
interpBOp Equals = undefined
interpBOp Less   = undefined
interpBOp And    = boolOp (&&)
interpBOp Or     = boolOp (||)

numOp :: (Rational -> Rational -> Rational) -> Value -> Value -> Either InterpError Value
numOp (#) (VNum x) (VNum y) = return $ VNum (x # y)
numOp _   (VNum _) y        = Left $ NumError y
numOp _   x        _        = Left $ NumError x

divOp :: Value -> Value -> Either InterpError Value
divOp (VNum x) (VNum y)
  | y == 0    = Left DivByZero
  | otherwise = return $ VNum (x / y)
divOp (VNum _) y = Left $ NumError y
divOp x        _ = Left $ NumError x

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Either InterpError Value
boolOp (#) (VBool x) (VBool y) = return $ VBool (x # y)
boolOp _   (VBool _) y         = Left $ BoolError y
boolOp _   x         _         = Left $ BoolError x

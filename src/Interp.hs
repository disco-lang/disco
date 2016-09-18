{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Interp where

import           Control.Monad           (join)
import           Data.List               (find)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           Data.Ratio              ((%))

import           Unbound.LocallyNameless (Name)

import           Typecheck
import           Types

data Value where
  VUnit :: Value
  VBool :: Bool -> Value
  VFun  :: (Value -> Value) -> Value
  VPair :: Value -> Value -> Value
  VInj  :: Side -> Value -> Value
  VNum  :: Rational -> Value

data InterpError where
  UnboundError :: Name Term -> InterpError
  NumError     :: Value     -> InterpError  -- ^ v should be a number, but isn't
  DivByZero    :: InterpError
  BoolError    :: Value     -> InterpError  -- ^ should be a boolean, but isn't

type Env = M.Map (Name Term) Value

interpTerm :: ATerm -> Either InterpError Value
interpTerm = interpTerm' M.empty

interpTerm' :: Env -> ATerm -> Either InterpError Value
interpTerm' e (ATVar _ x)       = maybe (Left $ UnboundError x) Right (M.lookup x e)
interpTerm' _ ATUnit            = return VUnit
interpTerm' _ (ATBool b)        = return $ VBool b
interpTerm' e (ATAbs _ abs)     = undefined
interpTerm' e (ATApp _ f x)     = undefined
interpTerm' e (ATPair _ l r)    = VPair <$> interpTerm' e l <*> interpTerm' e r
interpTerm' e (ATInj _ s t)     = VInj s <$> interpTerm' e t
interpTerm' _ (ATNat i)         = return $ VNum (i % 1)
interpTerm' e (ATUn _ op t)     = interpTerm' e t >>= interpUOp op
interpTerm' e (ATBin ty op l r) = join (interpBOp ty op <$> interpTerm' e l <*> interpTerm' e r)
interpTerm' e (ATLet _ t)       = undefined
interpTerm' e (ATCase _ bs)     = undefined
interpTerm' e (ATAscr t _)      = interpTerm' e t

interpUOp :: UOp -> Value -> Either InterpError Value
interpUOp Neg (VNum n) = return $ VNum (-n)
interpUOp Neg v        = Left $ NumError v

interpBOp :: Type -> BOp -> Value -> Value -> Either InterpError Value
interpBOp _ Add     = numOp (+)
interpBOp _ Sub     = numOp (-)
interpBOp _ Mul     = numOp (*)
interpBOp _ Div     = divOp
interpBOp ty Equals = \v1 v2 -> return $ VBool (decideFor ty v1 v2)
interpBOp _ Less    = undefined
interpBOp _ And     = boolOp (&&)
interpBOp _ Or      = boolOp (||)

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

decideFor :: Type -> Value -> Value -> Bool
decideFor (TyPair ty1 ty2) (VPair x1 y1) (VPair x2 y2)
  = decideFor ty1 x1 x2 && decideFor ty2 y1 y2
decideFor (TySum ty1 _) (VInj L v1) (VInj L v2)
  = decideFor ty1 v1 v2
decideFor (TySum _ ty2) (VInj R v1) (VInj R v2)
  = decideFor ty2 v1 v2
decideFor (TyArr ty1 ty2) (VFun f1) (VFun f2)
  = and (zipWith (decideFor ty2) (map f1 ty1s) (map f2 ty1s))
  where
    ty1s = enumerate ty1
decideFor _ v1 v2 = primValEq v1 v2

primValEq :: Value -> Value -> Bool
primValEq VUnit VUnit           = True
primValEq (VBool b1) (VBool b2) = b1 == b2
primValEq (VNum n1) (VNum n2)   = n1 == n2
primValEq _ _                   = False

enumerate :: Type -> [Value]
enumerate TyVoid           = []
enumerate TyUnit           = [VUnit]
enumerate TyBool           = [VBool False, VBool True]
enumerate (TyArr ty1 ty2)  = map (mkFun vs1) (sequence (vs2 <$ vs1))
  where
    vs1   = enumerate ty1
    vs2   = enumerate ty2
    mkFun vs1 vs2 = VFun $ \v -> snd . fromJust $ find (decideFor ty1 v . fst) (zip vs1 vs2)
enumerate (TyPair ty1 ty2) = [ VPair x y | x <- enumerate ty1, y <- enumerate ty2 ]
enumerate (TySum ty1 ty2)  = map (VInj L) (enumerate ty1) ++ map (VInj R) (enumerate ty2)
enumerate _                = []  -- other cases shouldn't happen if the program type checks

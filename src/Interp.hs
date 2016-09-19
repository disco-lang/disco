{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Interp where

import           Control.Monad           (join)
import           Control.Monad.Except    (ExceptT, runExceptT, throwError)
import           Data.List               (find)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           Data.Ratio              ((%))

import           Unbound.LocallyNameless (LFreshM, Name, lunbind, runLFreshM)

import           Typecheck
import           Types

data Value where
  VUnit :: Value
  VBool :: Bool -> Value
  VClos :: Name ATerm -> ATerm -> Env -> Value    -- closure
  VPair :: Value -> Value -> Value
  VInj  :: Side -> Value -> Value
  VNum  :: Rational -> Value

data InterpError where
  UnboundError :: Name ATerm -> InterpError
  NotANum      :: Value      -> InterpError  -- ^ v should be a number, but isn't
  DivByZero    ::               InterpError
  NotABool     :: Value      -> InterpError  -- ^ should be a boolean, but isn't
  NotAFun      :: Value      -> InterpError

type Env = M.Map (Name ATerm) Value

-- | Interpreter monad.  Can throw InterpErrors, and generate fresh
-- names.
type IM = ExceptT InterpError LFreshM

runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT

interpTerm :: ATerm -> IM Value
interpTerm = interpTerm' M.empty

interpTerm' :: Env -> ATerm -> IM Value
interpTerm' e (ATVar _ x)       = maybe (throwError $ UnboundError x) return (M.lookup x e)
interpTerm' _ ATUnit            = return VUnit
interpTerm' _ (ATBool b)        = return $ VBool b
interpTerm' e (ATAbs _ abs)     = lunbind abs $ \(x,t) -> return $ VClos x t e
interpTerm' e (ATApp _ f x)     = join (interpApp <$> interpTerm' e f <*> interpTerm' e x)
interpTerm' e (ATPair _ l r)    = VPair <$> interpTerm' e l <*> interpTerm' e r
interpTerm' e (ATInj _ s t)     = VInj s <$> interpTerm' e t
interpTerm' _ (ATNat i)         = return $ VNum (i % 1)
interpTerm' e (ATUn _ op t)     = interpTerm' e t >>= interpUOp op
interpTerm' e (ATBin ty op l r) = join (interpBOp ty op <$> interpTerm' e l <*> interpTerm' e r)
interpTerm' e (ATLet _ t)       = undefined
interpTerm' e (ATCase _ bs)     = undefined
interpTerm' e (ATAscr t _)      = interpTerm' e t

interpApp :: Value -> Value -> IM Value
interpApp (VClos x body e) v = interpTerm' (M.insert x v e) body
interpApp f _                = throwError $ NotAFun f

interpUOp :: UOp -> Value -> IM Value
interpUOp Neg (VNum n) = return $ VNum (-n)
interpUOp Neg v        = throwError $ NotANum v

interpBOp :: Type -> BOp -> Value -> Value -> IM Value
interpBOp _ Add     = numOp (+)
interpBOp _ Sub     = numOp (-)
interpBOp _ Mul     = numOp (*)
interpBOp _ Div     = divOp
interpBOp ty Equals = \v1 v2 -> return $ VBool (decideFor ty v1 v2)
interpBOp _ Less    = undefined
interpBOp _ And     = boolOp (&&)
interpBOp _ Or      = boolOp (||)

numOp :: (Rational -> Rational -> Rational) -> Value -> Value -> IM Value
numOp (#) (VNum x) (VNum y) = return $ VNum (x # y)
numOp _   (VNum _) y        = throwError $ NotANum y
numOp _   x        _        = throwError $ NotANum x

divOp :: Value -> Value -> IM Value
divOp (VNum x) (VNum y)
  | y == 0    = throwError DivByZero
  | otherwise = return $ VNum (x / y)
divOp (VNum _) y = throwError $ NotANum y
divOp x        _ = throwError $ NotANum x

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> IM Value
boolOp (#) (VBool x) (VBool y) = return $ VBool (x # y)
boolOp _   (VBool _) y         = throwError $ NotABool y
boolOp _   x         _         = throwError $ NotABool x

decideFor :: Type -> Value -> Value -> Bool
decideFor (TyPair ty1 ty2) (VPair x1 y1) (VPair x2 y2)
  = decideFor ty1 x1 x2 && decideFor ty2 y1 y2
decideFor (TySum ty1 _) (VInj L v1) (VInj L v2)
  = decideFor ty1 v1 v2
decideFor (TySum _ ty2) (VInj R v1) (VInj R v2)
  = decideFor ty2 v1 v2
decideFor (TyArr ty1 ty2) c1@(VClos {}) c2@(VClos {})
  = undefined
  -- = and (zipWith (decideFor ty2) (map f1 ty1s) (map f2 ty1s))
  -- where
  --   mkFun (VClos x t e) v = interpTerm' (M.insert x v e) t
  --   f1 = mkFun c1
  --   f2 = mkFun c2
  --   ty1s = enumerate ty1
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
    mkFun vs1 vs2 = undefined
      -- VFun $ \v -> snd . fromJust $ find (decideFor ty1 v . fst) (zip vs1 vs2)
enumerate (TyPair ty1 ty2) = [ VPair x y | x <- enumerate ty1, y <- enumerate ty2 ]
enumerate (TySum ty1 ty2)  = map (VInj L) (enumerate ty1) ++ map (VInj R) (enumerate ty2)
enumerate _                = []  -- other cases shouldn't happen if the program type checks

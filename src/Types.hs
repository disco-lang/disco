{-# LANGUAGE GADTs #-}

module Types where

import           Unbound.LocallyNameless

data Side = L | R

data UOp = Neg
data BOp = Add | Sub | Mul | Div

data Term where
  TVar   :: Name Term -> Term
  TUnit  :: Term
  TBool  :: Bool -> Term
  TAbs   :: Bind (Name Term) Term -> Term
  TApp   :: Term -> Term -> Term
  TPair  :: Term -> Term -> Term
  TInj   :: Side -> Term -> Term
  TInt   :: Integer -> Term
  TUn    :: UOp -> Term -> Term
  TBin   :: BOp -> Term -> Term -> Term
  TLet   :: Bind (Rec (Name Term, Embed Term)) Term -> Term
  TCase  :: [Branch] -> Term
  TWrong :: Term

type Branch = Bind [Guard] Term

data Guard where
  GIf    :: Embed Term -> Guard
  GWhere :: Embed Term -> Pattern -> Guard

data Pattern where
  PVar  :: Name Term -> Pattern
  PWild :: Pattern
  PUnit :: Pattern
  PBool :: Bool -> Pattern
  PPair :: Pattern -> Pattern -> Pattern
  PInj  :: Side -> Pattern -> Pattern
  PInt  :: Integer -> Pattern
  PSucc :: Pattern -> Pattern

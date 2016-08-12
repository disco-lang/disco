{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}


module Types where

import           Unbound.LocallyNameless

data Side = L | R
  deriving Show

data UOp = Neg
  deriving Show
data BOp = Add | Sub | Mul | Div
  deriving Show

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
  deriving Show

type Branch = Bind [Guard] Term

data Guard where
  GIf    :: Embed Term -> Guard
  GWhere :: Embed Term -> Pattern -> Guard
  deriving Show

data Pattern where
  PVar  :: Name Term -> Pattern
  PWild :: Pattern
  PUnit :: Pattern
  PBool :: Bool -> Pattern
  PPair :: Pattern -> Pattern -> Pattern
  PInj  :: Side -> Pattern -> Pattern
  PInt  :: Integer -> Pattern
  PSucc :: Pattern -> Pattern
  deriving Show

data Type where
  TyVoid   :: Type
  TyUnit   :: Type
  TyBool   :: Type
  TyArr    :: Type -> Type -> Type
  TyPair   :: Type -> Type -> Type
  TySum    :: Type -> Type -> Type
  TyN      :: Type
  TyZ      :: Type
  TyQ      :: Type
  deriving Show

derive [''Side, ''UOp, ''BOp, ''Term, ''Guard, ''Pattern, ''Type]

instance Alpha Side
instance Alpha UOp
instance Alpha BOp
instance Alpha Term
instance Alpha Guard
instance Alpha Pattern
instance Alpha Type

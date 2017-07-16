{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module Types where

import           Data.Coerce  (coerce)
import           GHC.Generics (Generic)

import           Data.Map     (Map, (!))
import qualified Data.Map     as M
import           Data.Set     (Set)
import qualified Data.Set     as S

import           Control.Arrow ((***))
import           Control.Lens (toListOf)
import           Data.Set.Lens (setOf)

import           Unbound.Generics.LocallyNameless

import           Subst

------------------------------------------------------------
-- Atomic types
------------------------------------------------------------

data Atom where
  AVar :: Name Type -> Atom
  ANat :: Atom
  AInt :: Atom
  deriving (Show, Eq, Ord, Generic)

instance Alpha Atom

instance Subst Atom Atom where
  isvar (AVar x) = Just (SubstName (coerce x))
  isvar _        = Nothing

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

isSub :: Atom -> Atom -> Bool
isSub a1 a2 | a1 == a2 = True
isSub ANat AInt = True
isSub _ _ = False

ainf2 :: Atom -> Atom -> Maybe Atom
ainf2 a1 a2     | a1 == a2 = Just a1
ainf2 AInt ANat = Just ANat
ainf2 ANat AInt = Just ANat
ainf2 _ _       = Nothing

ainf :: [Atom] -> Maybe Atom
ainf []  = Nothing
ainf [a] = Just a
ainf (a:as) = do
  g <- ainf as
  ainf2 a g

asup2 :: Atom -> Atom -> Maybe Atom
asup2 a1 a2     | a1 == a2 = Just a1
asup2 AInt ANat = Just AInt
asup2 ANat AInt = Just AInt
asup2 _ _       = Nothing

asup :: [Atom] -> Maybe Atom
asup []  = Nothing
asup [a] = Just a
asup (a:as) = do
  g <- asup as
  asup2 a g

------------------------------------------------------------
-- Type structure
------------------------------------------------------------

data Cons where
  CArr  :: Cons
  CPair :: Cons
  deriving (Show, Eq, Ord, Generic)

instance Alpha Cons

data Variance = Co | Contra

arity :: Cons -> [Variance]
arity CArr  = [Contra, Co]
arity CPair = [Co, Co]

------------------------------------------------------------
-- Types
------------------------------------------------------------

data Type where
  TyAtom :: Atom -> Type
  TyCons :: Cons -> [Type] -> Type
  deriving (Show, Eq, Ord, Generic)

instance Alpha Type

instance Subst Type Atom
instance Subst Type Cons
instance Subst Type Type where
  isvar (TyAtom (AVar x)) = Just (SubstName x)
  isvar _                 = Nothing

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s  = S.map (substs s)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s  = M.map (substs s)

type S = S' Type

atomToTypeSubst :: S' Atom -> S' Type
atomToTypeSubst = map (coerce *** TyAtom)

------------------------------------------------------------
-- Convenience
------------------------------------------------------------

var :: String -> Type
var x = TyVar (string2Name x)

pattern TyVar :: Name Type -> Type
pattern TyVar v = TyAtom (AVar v)

pattern TyNat :: Type
pattern TyNat   = TyAtom ANat

pattern TyInt :: Type
pattern TyInt   = TyAtom AInt

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

pattern TyPair :: Type -> Type -> Type
pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

------------------------------------------------------------
-- Sigma types
------------------------------------------------------------

data Sigma where
  Forall :: Bind [Name Type] Type -> Sigma
  deriving (Show, Generic)

instance Alpha Sigma
instance Subst Type Sigma

generalize :: Type -> Sigma
generalize ty = Forall (bind (S.toList $ setOf fv ty) ty)

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Disco language types.
--
-----------------------------------------------------------------------------

module Disco.Types
       (
       -- * Disco language types
       -- ** Atomic types

         BaseTy(..), Var(..), Atom(..), UAtom
       , uatomToAtom, isVar, isBase, isSkolem

       -- ** Type constructors

       , Con(..)

       -- ** Type AST

       , Type(..)

       , pattern TyVar
       , pattern Skolem
       , pattern TyVoid
       , pattern TyUnit
       , pattern TyBool
       , pattern TyN
       , pattern TyZ
       , pattern TyF
       , pattern TyQ
       , pattern TyC
       , pattern TyFin
       , pattern TyArr
       , pattern TyPair
       , pattern TySum
       , pattern TyList
       , pattern TySet

       -- ** Quantified types

       , Sigma(..)
       , toSigma, closeSigma

       -- * Type predicates

       , isNumTy, isSubtractive, isEmptyTy

       -- * Type substitutions

       , S', atomToTypeSubst, uatomToTypeSubst

       -- * Strictness
       , Strictness(..), strictness

       -- * Utilities
       , countType
       , unpair
       , S

       -- * HasType class
       , HasType(..)
       )
       where

import           Data.Coerce
import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Control.Arrow                    ((***))
import           Control.Lens                     (toListOf)
import           Data.List                        (nub)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Void

import           Disco.Subst                      (S')

--------------------------------------------------
-- Disco types
--------------------------------------------------

----------------------------------------
-- Base types

data BaseTy where

  -- | The void type, with no inhabitants.
  Void :: BaseTy

  -- | The unit type, with one inhabitant.
  Unit :: BaseTy

  -- | Booleans.
  B    :: BaseTy

  -- | Natural numbers.
  N    :: BaseTy

  -- | Integers.
  Z    :: BaseTy

  -- | Fractionals (i.e. nonnegative rationals).
  F    :: BaseTy

  -- | Rationals.
  Q    :: BaseTy

  -- | Unicode characters.
  C    :: BaseTy


  -- | Finite types. The single argument is a natural number defining
  --   the exact number of inhabitants.
  Fin  :: Integer -> BaseTy

  deriving (Show, Eq, Ord, Generic)

instance Alpha BaseTy
instance Subst BaseTy BaseTy

----------------------------------------
-- Type variables

data Var where
  -- | Unification variable
  U :: Name Type -> Var
  -- | Skolem variable
  S :: Name Type -> Var
  deriving (Show, Eq, Ord, Generic)

instance Alpha Var

----------------------------------------
-- Atomic types

data Atom where
  AVar  :: Var -> Atom
  ABase :: BaseTy -> Atom
  deriving (Show, Eq, Ord, Generic)

instance Alpha Atom
instance Subst Atom Var
instance Subst Atom BaseTy

instance Subst Atom Atom where
  isvar (AVar (U x)) = Just (SubstName (coerce x))
  isvar _            = Nothing

type UAtom = Either BaseTy (Name Type)  -- unifiable atoms, i.e. no skolems

uatomToAtom :: UAtom -> Atom
uatomToAtom (Left b)  = ABase b
uatomToAtom (Right x) = AVar (U x)

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

isSkolem :: Atom -> Bool
isSkolem (AVar (S _)) = True
isSkolem _            = False

----------------------------------------
-- Type constructors

data Con where
  -- | Function type, T1 -> T2
  CArr  :: Con
  -- | Pair type, T1 * T2
  CPair :: Con
  -- | Sum type, T1 + T2
  CSum  :: Con
  -- | Lists
  CList :: Con
  -- | Sets
  CSet  :: Con
  deriving (Show, Eq, Ord, Generic)

instance Alpha Con

----------------------------------------
-- Types

-- | Types.
data Type where

  -- | Atomic types (variables and base types).
  TyAtom :: Atom -> Type

  -- | Application of a type constructor to type arguments.
  TyCon  :: Con -> [Type] -> Type

  -- | A user defined algrbraic datatype. In order for an ADT to be a valid
  --   type, the string representing the ADT must appear on the left hand side of
  --   a ADT declaration.
  TyDef :: String -> Type

  deriving (Show, Eq, Ord, Generic)

instance Alpha Type
instance Subst Type Rational where
  subst _ _ = id
  substs _  = id
instance Subst Type Void where
  subst _ _ = id
  substs _  = id

pattern TyVar  :: Name Type -> Type
pattern TyVar v = TyAtom (AVar (U v))

pattern Skolem :: Name Type -> Type
pattern Skolem v = TyAtom (AVar (S v))

pattern TyVoid :: Type
pattern TyVoid = TyAtom (ABase Void)

pattern TyUnit :: Type
pattern TyUnit = TyAtom (ABase Unit)

pattern TyBool :: Type
pattern TyBool = TyAtom (ABase B)

pattern TyN :: Type
pattern TyN = TyAtom (ABase N)

pattern TyZ :: Type
pattern TyZ = TyAtom (ABase Z)

pattern TyF :: Type
pattern TyF = TyAtom (ABase F)

pattern TyQ :: Type
pattern TyQ = TyAtom (ABase Q)

pattern TyC :: Type
pattern TyC = TyAtom (ABase C)

pattern TyFin :: Integer -> Type
pattern TyFin n = TyAtom (ABase (Fin n))

pattern TyArr :: Type -> Type -> Type
pattern TyArr ty1 ty2 = TyCon CArr [ty1, ty2]

pattern TyPair :: Type -> Type -> Type
pattern TyPair ty1 ty2 = TyCon CPair [ty1, ty2]

pattern TySum :: Type -> Type -> Type
pattern TySum ty1 ty2 = TyCon CSum [ty1, ty2]

pattern TyList :: Type -> Type
pattern TyList elTy = TyCon CList [elTy]

pattern TySet :: Type -> Type
pattern TySet elTy = TyCon CSet [elTy]

{-# COMPLETE
      TyDef, TyVar, Skolem, TyVoid, TyUnit, TyBool, TyN, TyZ, TyF, TyQ, TyC, TyFin,
      TyArr, TyPair, TySum, TyList, TySet #-}

instance Subst Type Var
instance Subst Type BaseTy
instance Subst Type Atom
instance Subst Type Con
instance Subst Type Type where
  isvar (TyAtom (AVar (U x))) = Just (SubstName x)
  isvar _                     = Nothing

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s  = S.map (substs s)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s  = M.map (substs s)

----------------------------------------
-- Sigma types (i.e. quanitified types)

-- | @Sigma@ represents a polymorphic type of the form
--   @forall a1 a2 ... an. ty@  (n may be 0).
newtype Sigma = Forall (Bind [Name Type] Type)
  deriving (Show, Generic)

instance Alpha Sigma
instance Subst Type Sigma

-- | Convert a monotype into a (trivially) quantified type.
toSigma :: Type -> Sigma
toSigma ty = Forall (bind [] ty)

-- | Convert a monotype into a polytype by quantifying over all its
--   free type variables.
closeSigma :: Type -> Sigma
closeSigma ty = Forall (bind (nub $ toListOf fv ty) ty)

--------------------------------------------------
-- Counting inhabitants
--------------------------------------------------

-- | Compute the number of inhabitants of a type.  @Nothing@ means the
--   type is countably infinite.
countType :: Type -> Maybe Integer
countType TyVoid            = Just 0
countType TyUnit            = Just 1
countType TyBool            = Just 2
countType (TyFin n)         = Just n
countType (TySum  ty1 ty2)  = (+) <$> countType ty1 <*> countType ty2
countType (TyPair ty1 ty2)
  | isEmptyTy ty1 = Just 0
  | isEmptyTy ty2 = Just 0
  | otherwise     = (*) <$> countType ty1 <*> countType ty2
countType (TyArr  ty1 ty2)
  | isEmptyTy ty1 = Just 1
  | isEmptyTy ty2 = Just 0
  | otherwise     = (^) <$> countType ty2 <*> countType ty1
countType (TyList ty)
  | isEmptyTy ty            = Just 1
  | otherwise               = Nothing

-- All other types are infinite. (TyN, TyZ, TyQ, TyF)
countType _                 = Nothing

--------------------------------------------------
-- Type predicates
--------------------------------------------------

-- | Check whether a type is a numeric type (N, Z, F, Q, or Zn).
isNumTy :: Type -> Bool
isNumTy (TyFin _) = True
isNumTy ty        = ty `elem` [TyN, TyZ, TyF, TyQ]

-- | Decide whether a type supports subtraction.
isSubtractive :: Type -> Bool
isSubtractive TyZ       = True
isSubtractive TyQ       = True
isSubtractive (TyFin _) = True
isSubtractive _         = False

-- | Decide whether a type is empty, /i.e./ uninhabited.
isEmptyTy :: Type -> Bool
isEmptyTy TyVoid           = True
isEmptyTy (TyFin 0)        = True
isEmptyTy (TyPair ty1 ty2) = isEmptyTy ty1 || isEmptyTy ty2
isEmptyTy (TySum ty1 ty2)  = isEmptyTy ty1 && isEmptyTy ty2
isEmptyTy _                = False

--------------------------------------------------
-- Strictness
--------------------------------------------------

-- | Strictness of a function application or let-expression.
data Strictness = Strict | Lazy
  deriving (Eq, Show, Generic)

instance Alpha Strictness

-- | Numeric types are strict, others are lazy.
strictness :: Type -> Strictness
strictness ty
  | isNumTy ty = Strict
  | otherwise  = Lazy

--------------------------------------------------
-- Utilities
--------------------------------------------------

-- | Decompose T1 * (T2 * ( ... )) into a list of types.
unpair :: Type -> [Type]
unpair (TyPair ty1 ty2) = ty1 : unpair ty2
unpair ty               = [ty]

-- | Define @S@ as a substitution on types (the most common kind)
--   for convenience.
type S = S' Type

atomToTypeSubst :: S' Atom -> S' Type
atomToTypeSubst = map (coerce *** TyAtom)

uatomToTypeSubst :: S' UAtom -> S' Type
uatomToTypeSubst = atomToTypeSubst . map (coerce *** uatomToAtom)

------------------------------------------------------------
-- HasType class
------------------------------------------------------------

class HasType t where
  getType :: t -> Type

  setType :: Type -> t -> t
  setType _ = id

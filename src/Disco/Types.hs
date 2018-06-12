{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Disco language types.
--
-----------------------------------------------------------------------------

module Disco.Types
       (
       -- * Disco language types
         Type(..)

       -- * Type predicates

       , isNumTy, isEmptyTy, isFinite
       , isSubtractive, isFractional
       , isDecidable, isOrdered

       -- * Strictness
       , Strictness(..), strictness

       -- * Utilities
       , countType
       , unpair

       )
       where

import           Data.Maybe                       (isJust)
import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Math.NumberTheory.Primes.Testing (isPrime)

--------------------------------------------------
-- Disco types
--------------------------------------------------

-- | Types.
data Type where
  -- | Type variables (for unification, not polymorphism)
  TyVar    :: Name Type -> Type

    -- TyVar is for unification variables.  Ideally Type would be parameterized by
    -- a variable type, then we could use Type' Void to represent
    -- solved types, but I can't figure out how to make that work with
    -- unbound.

  -- | The void type, with no inhabitants.
  TyVoid   :: Type

  -- | The unit type, with one inhabitant.
  TyUnit   :: Type

  -- | Booleans.
  TyBool   :: Type

  -- | Function type, T1 -> T2
  TyArr    :: Type -> Type -> Type

  -- | Pair type, T1 * T2
  TyPair   :: Type -> Type -> Type

  -- | Sum type, T1 + T2
  TySum    :: Type -> Type -> Type

  -- | Natural numbers
  TyN      :: Type

  -- | Integers
  TyZ      :: Type

  -- | Nonnegative rationals
  TyQP     :: Type

  -- | Rationals
  TyQ      :: Type

  -- | Finite type, single argument is a natural number
  --   defining the exact number of inhabitants.
  TyFin    :: Integer -> Type

  -- | Lists
  TyList   :: Type -> Type

  TySet :: Type -> Type

  deriving (Show, Eq, Generic)

instance Alpha Type

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

-- All other types are infinite. (TyN, TyZ, TyQ, TyQP)
countType _                 = Nothing

--------------------------------------------------
-- Type predicates
--------------------------------------------------

-- | Check whether a type is a numeric type (N, Z, Q+, Q, or Zn).
isNumTy :: Type -> Bool
isNumTy (TyFin _) = True
isNumTy ty        = ty `elem` [TyN, TyZ, TyQP, TyQ]

-- | Decide whether a type is empty, /i.e./ uninhabited.
isEmptyTy :: Type -> Bool
isEmptyTy TyVoid           = True
isEmptyTy (TyFin 0)        = True
isEmptyTy (TyPair ty1 ty2) = isEmptyTy ty1 || isEmptyTy ty2
isEmptyTy (TySum ty1 ty2)  = isEmptyTy ty1 && isEmptyTy ty2
isEmptyTy _                = False

-- | Decide whether a type is finite.
isFinite :: Type -> Bool
isFinite ty = isJust (countType ty)

-- | Decide whether a type supports division.
isFractional :: Type -> Bool
isFractional TyQ        = True
isFractional TyQP       = True
isFractional (TyFin n)  = isPrime n
isFractional _          = False

-- | Decide whether a type supports subtraction.
isSubtractive :: Type -> Bool
isSubtractive TyZ       = True
isSubtractive TyQ       = True
isSubtractive (TyFin _) = True
isSubtractive _         = False

-- | Decide whether a type has decidable equality.
isDecidable :: Type -> Bool
isDecidable TyVoid            = True
isDecidable TyUnit            = True
isDecidable TyBool            = True
isDecidable TyN               = True
isDecidable TyZ               = True
isDecidable TyQP              = True
isDecidable TyQ               = True
isDecidable (TyFin _)         = True
isDecidable (TySum  ty1 ty2)  = isDecidable ty1 && isDecidable ty2
isDecidable (TyPair ty1 ty2)  = isDecidable ty1 && isDecidable ty2
isDecidable (TyList ty)       = isDecidable ty
isDecidable (TyArr  ty1 ty2)  = isFinite    ty1 && isDecidable ty2

isDecidable (TyVar _)         = error "isDecidable TyVar"

-- | Check whether the given type has a decidable total order.
isOrdered :: Type -> Bool
isOrdered TyVoid            = True
isOrdered TyUnit            = True
isOrdered TyBool            = True
isOrdered TyN               = True
isOrdered TyZ               = True
isOrdered TyQP              = True
isOrdered TyQ               = True
isOrdered (TyFin _)         = True
isOrdered (TySum  ty1 ty2)  = isOrdered ty1 && isOrdered ty2
isOrdered (TyPair ty1 ty2)  = isOrdered ty1 && isOrdered ty2
isOrdered (TyList ty)       = isOrdered ty
isOrdered (TyArr  ty1 ty2)  = isFinite ty1 && isOrdered ty1 && isOrdered ty2

isOrdered (TyVar _)         = error "isOrdered TyVar"

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

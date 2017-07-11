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
       ( Type(..)

       , isNumTy, Strictness(..), strictness

       , unpair
       )
       where

import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless

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

  -- | Lists
  TyList   :: Type -> Type

  deriving (Show, Eq, Generic)

-- | Check whether a type is a numeric type (N, Z, or Q).
isNumTy :: Type -> Bool
isNumTy ty = ty `elem` [TyN, TyZ, TyQP, TyQ]

-- | Strictness of a function application or let-expression.
data Strictness = Strict | Lazy
  deriving (Eq, Show, Generic)

-- | Numeric types are strict, others are lazy.
strictness :: Type -> Strictness
strictness ty
  | isNumTy ty = Strict
  | otherwise  = Lazy

-- | Decompose T1 * (T2 * ( ... )) into a list of types.
unpair :: Type -> [Type]
unpair (TyPair ty1 ty2) = ty1 : unpair ty2
unpair ty               = [ty]

instance Alpha Type
instance Alpha Strictness


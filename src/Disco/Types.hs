{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.Types where

import           Unbound.LocallyNameless

-- | Types.
data Type where
  TyVar    :: Name Type -> Type
    -- Unification variables.  Ideally Type would be parameterized by
    -- a variable type, then we could use Type' Void to represent
    -- solved types, but I can't figure out how to make that work with
    -- unbound.

  TyVoid   :: Type                  -- ^ Void
  TyUnit   :: Type                  -- ^ Unit
  TyBool   :: Type                  -- ^ Bool
  TyArr    :: Type -> Type -> Type  -- ^ Function type,  T1 -> T2
  TyPair   :: Type -> Type -> Type  -- ^ Pair type, T1 * T2
  TySum    :: Type -> Type -> Type  -- ^ Sum type, T1 + T2
  TyN      :: Type                  -- ^ Natural numbers
  TyZ      :: Type                  -- ^ Integers
  TyQ      :: Type                  -- ^ Rationals
  deriving (Show, Eq)

derive [''Type]

instance Alpha Type

isNumTy :: Type -> Bool
isNumTy ty = ty `elem` [TyN, TyZ, TyQ]

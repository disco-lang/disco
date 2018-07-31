{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Constraints
-- Copyright   :  (c) 2018 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Constraints generated by type inference & checking.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Constraints
  ( Constraint(..)
  , cAnd
  )
  where

import qualified Data.List.NonEmpty               as NE
import           Data.Semigroup
import           GHC.Generics                     (Generic)

import           Disco.Types
import           Disco.Types.Rules
import           Unbound.Generics.LocallyNameless

-- | Constraints are generated as a result of type inference and checking.
--   These constraints are accumulated during the inference and checking phase
--   and are subsequently solved by the constraint solver.
data Constraint where
  CSub   :: Type -> Type -> Constraint
  CEq    :: Type -> Type -> Constraint
  CQual  :: Qualifier -> Type -> Constraint
  CAnd   :: [Constraint] -> Constraint
  CTrue  :: Constraint
  COr    :: [Constraint] -> Constraint
  CAll  :: Bind [Name Type] Constraint -> Constraint

  deriving (Show, Generic)

instance Alpha Constraint

instance Subst Type Constraint

-- A helper function for creating a single constraint from a list of constraints.
cAnd :: [Constraint] -> Constraint
cAnd cs = case filter nontrivial cs of
  []  -> CTrue
  [c] -> c
  cs' -> CAnd cs'
  where
    nontrivial CTrue = False
    nontrivial _     = True

instance Semigroup Constraint where
  c1 <> c2 = cAnd [c1,c2]
  sconcat  = cAnd . NE.toList
  stimes   = stimesIdempotent

instance Monoid Constraint where
  mempty  = CTrue
  mappend = (<>)
  mconcat = cAnd

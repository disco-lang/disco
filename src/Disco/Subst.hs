-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Subst
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic definition of substitutions.
--
-----------------------------------------------------------------------------

module Disco.Subst where

import           Unbound.Generics.LocallyNameless (Name, Subst, substs)

import           Data.Coerce

import           Data.Bifunctor                   (second)
import qualified Data.Map                         as M

-- This is an inefficient representation of substitutions; and ideally
-- we would implement constraint solving without using explicit
-- substitutions at all.  But this will do for now.

-- | @S' a@ is a substitution from variables representing @a@ to
--   values of type @a@.
newtype S' a = S' { getSubst :: M.Map (Name a) a }
  deriving (Eq, Ord, Show)

instance Functor S' where
  fmap f (S' m) = S' (M.mapKeys coerce . M.map f $ m)

-- | The identity substitution.
idS :: S' a
idS = S' M.empty

-- | The domain of a substitution.
dom :: S' a -> [Name a]
dom = M.keys . getSubst

-- | Construct a singleton substitution.
(|->) :: Name a -> a -> S' a
x |-> t = S' (M.singleton x t)

-- | Compose two substitutions.
(@@) :: Subst a a => S' a -> S' a -> S' a
(S' s1) @@ (S' s2) = S' ((M.map (applySubst (S' s1))) s2 `M.union` s1)

-- | Compose a whole bunch of substitutions.
compose :: (Subst a a, Foldable t) => t (S' a) -> S' a
compose = foldr (@@) idS

-- | Apply a substitution.
applySubst :: Subst b a => S' b -> a -> a
applySubst (S' s) = substs (M.assocs s)

-- | Create a substitution from an assoc-list.
fromList :: [(Name a, a)] -> S' a
fromList = S' . M.fromList

-- | Convert a substitution into an assoc-list.
toList :: S' a -> [(Name a, a)]
toList (S' m) = M.assocs m

-- | Look up the value a particular name maps to under the given
--   substitution.
lookup :: Name a -> S' a -> Maybe a
lookup x (S' m) = M.lookup x m

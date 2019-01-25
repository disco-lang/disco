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

import           Data.Bifunctor                   (second)

-- This is an inefficient representation of substitutions; and ideally
-- we would implement constraint solving without using explicit
-- substitutions at all.  But this will do for now.

-- | @S' a@ is a substitution from variables representing @a@ to
--   values of type @a@.
type S' a = [(Name a, a)]

-- | The identity substitution.
idS :: S' a
idS = []

-- | The domain of a substitution.
dom :: S' a -> [Name a]
dom = map fst

-- | Construct a singleton substitution.
(|->) :: Name a -> a -> S' a
x |-> t = [(x,t)]

-- | Compose two substitutions.
(@@) :: Subst a a => S' a -> S' a -> S' a
s1 @@ s2 = (map . second) (substs s1) s2 ++ s1

-- | Compose a whole bunch of substitutions.
compose :: (Subst a a, Foldable t) => t (S' a) -> S' a
compose = foldr (@@) idS

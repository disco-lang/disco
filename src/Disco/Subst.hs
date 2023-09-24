{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Subst
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- The "Disco.Subst" module defines a generic type of substitutions
-- that map variable names to values.
module Disco.Subst (
  -- * Substitutions
  Substitution (..),
  dom,

  -- ** Constructing/destructing substitutions
  idS,
  (|->),
  fromList,
  toList,

  -- ** Substitution operations
  (@@),
  compose,
  applySubst,
  lookup,
)
where

import Prelude hiding (lookup)

import Unbound.Generics.LocallyNameless (Name, Subst, substs)

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)

import Disco.Effects.LFresh
import Disco.Pretty
import Polysemy
import Polysemy.Reader

-- | A value of type @Substitution a@ is a substitution which maps some set of
--   names (the /domain/, see 'dom') to values of type @a@.
--   Substitutions can be /applied/ to certain terms (see
--   'applySubst'), replacing any free occurrences of names in the
--   domain with their corresponding values.  Thus, substitutions can
--   be thought of as functions of type @Term -> Term@ (for suitable
--   @Term@s that contain names and values of the right type).
--
--   Concretely, substitutions are stored using a @Map@.
--
--   See also "Disco.Types", which defines 'S' as an alias for
--   substitutions on types (the most common kind in the disco
--   codebase).
newtype Substitution a = Substitution {getSubst :: Map (Name a) a}
  deriving (Eq, Ord, Show)

instance Functor Substitution where
  fmap f (Substitution m) = Substitution (M.mapKeys coerce . M.map f $ m)

instance Pretty a => Pretty (Substitution a) where
  pretty (Substitution s) = do
    let es = map (uncurry prettyMapping) (M.assocs s)
    ds <- punctuate "," es
    braces (hsep ds)

prettyMapping :: (Pretty a, Members '[Reader PA, LFresh] r) => Name a -> a -> Sem r Doc
prettyMapping x a = pretty x <+> "->" <+> pretty a

-- | The domain of a substitution is the set of names for which the
--   substitution is defined.
dom :: Substitution a -> Set (Name a)
dom = M.keysSet . getSubst

-- | The identity substitution, /i.e./ the unique substitution with an
--   empty domain, which acts as the identity function on terms.
idS :: Substitution a
idS = Substitution M.empty

-- | Construct a singleton substitution, which maps the given name to
--   the given value.
(|->) :: Name a -> a -> Substitution a
x |-> t = Substitution (M.singleton x t)

-- | Compose two substitutions.  Applying @s1 \@\@ s2@ is the same as
--   applying first @s2@, then @s1@; that is, semantically,
--   composition of substitutions corresponds exactly to function
--   composition when they are considered as functions on terms.
--
--   As one would expect, composition is associative and has 'idS' as
--   its identity.
(@@) :: Subst a a => Substitution a -> Substitution a -> Substitution a
(Substitution s1) @@ (Substitution s2) = Substitution ((M.map (applySubst (Substitution s1))) s2 `M.union` s1)

-- | Compose a whole container of substitutions.  For example,
--   @compose [s1, s2, s3] = s1 \@\@ s2 \@\@ s3@.
compose :: (Subst a a, Foldable t) => t (Substitution a) -> Substitution a
compose = foldr (@@) idS

-- | Apply a substitution to a term, resulting in a new term in which
--   any free variables in the domain of the substitution have been
--   replaced by their corresponding values.  Note this requires a
--   @Subst b a@ constraint, which intuitively means that values of
--   type @a@ contain variables of type @b@ we can substitute for.
applySubst :: Subst b a => Substitution b -> a -> a
applySubst (Substitution s) = substs (M.assocs s)

-- | Create a substitution from an association list of names and
--   values.
fromList :: [(Name a, a)] -> Substitution a
fromList = Substitution . M.fromList

-- | Convert a substitution into an association list.
toList :: Substitution a -> [(Name a, a)]
toList (Substitution m) = M.assocs m

-- | Look up the value a particular name maps to under the given
--   substitution; or return @Nothing@ if the name being looked up is
--   not in the domain.
lookup :: Name a -> Substitution a -> Maybe a
lookup x (Substitution m) = M.lookup x m

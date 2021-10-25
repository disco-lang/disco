-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Context
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- A *context* is a mapping from names to other things (such as types
-- or values).  This module defines a generic type of contexts which
-- is used in many different places throughout the disco codebase.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Context
       (
         Ctx, names, emptyCtx, singleCtx, joinCtx, joinCtxs
       , lookup, extend, extends

       ) where

import           Prelude                          hiding (lookup)

import qualified Data.Map                         as M

import           Unbound.Generics.LocallyNameless (Name)

import           Polysemy
import           Polysemy.Reader

import           Disco.AST.Typed                  (NameProvenance, QName (..))

-- | A context maps qualified names to things.  In particular a @Ctx a
--   b@ maps qualified names for @a@s to values of type @b@.
type Ctx a b = M.Map NameProvenance (M.Map (Name a) b)

  -- Note that we implement a context as a nested map from
  -- NameProvenance to Name to b, rather than as a Map QName b.  They
  -- are isomorphic, but this way it is easier to do name resolution,
  -- because given an (unqualified) Name, we can look it up in each
  -- inner map corresponding to modules that are in scope.

-- | Return a list of the names defined by the context.
names :: Ctx a b -> [Name a]
names = concatMap M.keys . M.elems

-- | The empty context.
emptyCtx :: Ctx a b
emptyCtx = M.empty

-- | A singleton context, mapping a qualified name to a thing.
singleCtx :: QName a -> b -> Ctx a b
singleCtx (QName p n) = M.singleton p . M.singleton n

-- | Join two contexts (left-biased, /i.e./ if the same name exists in
--   both contexts, the result will use the value from the first
--   context, and throw away the value from the second.).
joinCtx :: Ctx a b -> Ctx a b -> Ctx a b
joinCtx = M.union

-- | Join a list of contexts (left-biased).
joinCtxs :: [Ctx a b] -> Ctx a b
joinCtxs = M.unions

-- | Look up a name in a context.
lookup :: Member (Reader (Ctx a b)) r => QName a -> Sem r (Maybe b)
lookup x = M.lookup x <$> ask

-- | Run a computation under a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: Member (Reader (Ctx a b)) r => Name a -> b -> Sem r c -> Sem r c
extend x b = local (M.insert x b)

-- | Run a computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: Member (Reader (Ctx a b)) r => Ctx a b -> Sem r c -> Sem r c
extends ctx = local (joinCtx ctx)

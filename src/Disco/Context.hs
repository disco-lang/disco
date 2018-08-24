{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Context
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Contexts manage mappings from names to other things (such as types
-- or values).
--
-----------------------------------------------------------------------------

module Disco.Context
       (
         Ctx, names, emptyCtx, singleCtx, joinCtx, joinCtxs
       , lookup, extend, extends

       ) where

import           Prelude                          hiding (lookup)

import           Control.Monad.Reader
import qualified Data.Map                         as M

import           Unbound.Generics.LocallyNameless

-- | A context maps names to things.
type Ctx a b = M.Map (Name a) b

-- | Return a list of the names bound in the context.
names :: Ctx a b -> [Name a]
names = M.keys

-- | The empty context.
emptyCtx :: Ctx a b
emptyCtx = M.empty

-- | A singleton context, mapping a name to a thing.
singleCtx :: Name a -> b -> Ctx a b
singleCtx = M.singleton

-- | Join two contexts (left-biased).
joinCtx :: Ctx a b -> Ctx a b -> Ctx a b
joinCtx = M.union

-- | Join a list of contexts (left-biased).
joinCtxs :: [Ctx a b] -> Ctx a b
joinCtxs = M.unions

-- | Look up a name in a context.
lookup :: MonadReader (Ctx a b) m => Name a -> m (Maybe b)
lookup x = M.lookup x <$> ask

-- | Run a computation under a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: MonadReader (Ctx a b) m => Name a -> b -> m r -> m r
extend x b = local (M.insert x b)

-- | Run a computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: MonadReader (Ctx a b) m => Ctx a b -> m r -> m r
extends ctx = local (joinCtx ctx)

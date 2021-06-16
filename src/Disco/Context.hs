{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}

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

import           Capability.Reader
import qualified Data.Map                         as M

import           Capability.Source                (await_)
import           GHC.Exts                         (Proxy#, proxy#)
import           Unbound.Generics.LocallyNameless

-- | A context maps names to things.  In particular a @Ctx a b@ maps
--   names for @a@s to values of type @b@.
type Ctx a b = M.Map (Name a) b

-- | Return a list of the names defined by the context.
names :: Ctx a b -> [Name a]
names = M.keys

-- | The empty context.
emptyCtx :: Ctx a b
emptyCtx = M.empty

-- | A singleton context, mapping a name to a thing.
singleCtx :: Name a -> b -> Ctx a b
singleCtx = M.singleton

-- | Join two contexts (left-biased, /i.e./ if the same name exists in
--   both contexts, the result will use the value from the first
--   context, and throw away the value from the second.).
joinCtx :: Ctx a b -> Ctx a b -> Ctx a b
joinCtx = M.union

-- | Join a list of contexts (left-biased).
joinCtxs :: [Ctx a b] -> Ctx a b
joinCtxs = M.unions

lookup_ :: forall k (tag :: k) m a b. HasReader tag (Ctx a b) m => Proxy# tag -> Name a -> m (Maybe b)
lookup_ tag x = M.lookup x <$> await_ tag

-- | Look up a name in a context.
lookup :: forall tag m a b. HasReader tag (Ctx a b) m => Name a -> m (Maybe b)
lookup = lookup_ (proxy# @tag)

extend_ :: forall k (tag :: k) m a b r. HasReader tag (Ctx a b) m => Proxy# tag -> Name a -> b -> m r -> m r
extend_ tag x b = local_ tag (M.insert x b)

-- | Run a computation under a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: forall tag m a b r. HasReader tag (Ctx a b) m => Name a -> b -> m r -> m r
extend = extend_ (proxy# @tag)

extends_ :: forall k (tag :: k) m a b r. HasReader tag (Ctx a b) m => Proxy# tag -> Ctx a b -> m r -> m r
extends_ tag ctx = local_ tag (joinCtx ctx)

-- | Run a computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: forall tag m a b r. HasReader tag (Ctx a b) m => Ctx a b -> m r -> m r
extends = extends_ (proxy# @tag)

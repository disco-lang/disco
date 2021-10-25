{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

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
       ( -- * Context type
         Ctx

         -- * Construction
       , emptyCtx
       , singleCtx
       , ctxForModule

       -- * Insertion
       , extend
       , extends

       -- * Query/conversion
       , lookup
       , lookupAll
       , names
       , elems

       -- * Traversal
       , coerceKeys

       -- * Combination
       , joinCtx
       , joinCtxs

       -- * Filter
       , filter

       ) where

import           Control.Monad                    ((<=<))
import qualified Data.Map                         as M
import           Prelude                          hiding (filter, lookup)

import           Unbound.Generics.LocallyNameless (Name)

import           Polysemy
import           Polysemy.Reader

import           Data.Coerce
import           Disco.AST.Typed                  (ModuleName,
                                                   NameProvenance (QualifiedName),
                                                   QName (..))

-- | A context maps qualified names to things.  In particular a @Ctx a
--   b@ maps qualified names for @a@s to values of type @b@.
newtype Ctx a b = Ctx { getCtx :: M.Map NameProvenance (M.Map (Name a) b) }
  deriving (Eq, Show, Functor, Foldable, Traversable)

  -- Note that we implement a context as a nested map from
  -- NameProvenance to Name to b, rather than as a Map QName b.  They
  -- are isomorphic, but this way it is easier to do name resolution,
  -- because given an (unqualified) Name, we can look it up in each
  -- inner map corresponding to modules that are in scope.

------------------------------------------------------------
-- Construction
------------------------------------------------------------

-- | The empty context.
emptyCtx :: Ctx a b
emptyCtx = Ctx M.empty

-- | A singleton context, mapping a qualified name to a thing.
singleCtx :: QName a -> b -> Ctx a b
singleCtx (QName p n) = Ctx . M.singleton p . M.singleton n

-- | Create a context for bindings from a single module.
ctxForModule :: ModuleName -> [(Name a, b)] -> Ctx a b
ctxForModule m = Ctx . M.singleton (QualifiedName m) . M.fromList

------------------------------------------------------------
-- Insertion
------------------------------------------------------------

-- | Run a computation under a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: Member (Reader (Ctx a b)) r => QName a -> b -> Sem r c -> Sem r c
extend (QName p n) b = local (Ctx . M.insertWith M.union p (M.singleton n b) . getCtx)

-- | Run a computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: Member (Reader (Ctx a b)) r => Ctx a b -> Sem r c -> Sem r c
extends = local . joinCtx

------------------------------------------------------------
-- Query/conversion
------------------------------------------------------------

-- | Look up a name in a context.
lookup :: Member (Reader (Ctx a b)) r => QName a -> Sem r (Maybe b)
lookup (QName p n) = (M.lookup n <=< M.lookup p) . getCtx <$> ask

-- | Look up all the non-local bindings of a name in a context.
lookupAll :: Member (Reader (Ctx a b)) r => Name a -> Sem r [(ModuleName, b)]
lookupAll n = nonLocal . M.assocs . M.mapMaybe (M.lookup n) . getCtx <$> ask
  where
    nonLocal bs = [(m,b) | (QualifiedName m, b) <- bs]

-- | Return a list of the names defined by the context.
names :: Ctx a b -> [Name a]
names = concatMap M.keys . M.elems . getCtx

-- | Return a list of all the values bound by the context.
elems :: Ctx a b -> [b]
elems = concatMap M.elems . M.elems . getCtx

------------------------------------------------------------
-- Traversal
------------------------------------------------------------

coerceKeys :: Ctx a1 b -> Ctx a2 b
coerceKeys = Ctx . M.map (M.mapKeys coerce) . getCtx

------------------------------------------------------------
-- Combination
------------------------------------------------------------

-- | Join two contexts (left-biased, /i.e./ if the same qualified name
--   exists in both contexts, the result will use the value from the
--   first context, and throw away the value from the second.).
joinCtx :: Ctx a b -> Ctx a b -> Ctx a b
joinCtx a b = joinCtxs [a,b]

-- | Join a list of contexts (left-biased).
joinCtxs :: [Ctx a b] -> Ctx a b
joinCtxs = Ctx . M.unionsWith M.union . map getCtx

------------------------------------------------------------
-- Filter
------------------------------------------------------------

-- | Filter a context using a predicate.
filter :: (b -> Bool) -> Ctx a b -> Ctx a b
filter p = Ctx . M.map (M.filter p) . getCtx

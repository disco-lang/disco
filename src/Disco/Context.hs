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
       , localCtx

       -- * Insertion
       , insert
       , extend
       , extends

       -- * Query
       , null
       , lookup, lookup'
       , lookupNonLocal, lookupNonLocal'
       , lookupAll, lookupAll'

       -- * Conversion
       , names
       , elems
       , assocs
       , keysSet

       -- * Traversal
       , coerceKeys
       , restrictKeys

       -- * Combination
       , joinCtx
       , joinCtxs

       -- * Filter
       , filter

       ) where

import           Control.Monad                    ((<=<))
import           Data.Bifunctor                   (first, second)
import           Data.Coerce
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Prelude                          hiding (filter, lookup, null)

import           Unbound.Generics.LocallyNameless (Name)

import           Polysemy
import           Polysemy.Reader

import           Disco.Names                      (ModuleName,
                                                   NameProvenance (..),
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

instance Semigroup (Ctx a b) where
  (<>) = joinCtx

instance Monoid (Ctx a b) where
  mempty = emptyCtx
  mappend = (<>)

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

-- | Create a context with local bindings.
localCtx :: [(Name a, b)] -> Ctx a b
localCtx = Ctx . M.singleton LocalName . M.fromList

------------------------------------------------------------
-- Insertion
------------------------------------------------------------

-- | Insert a new binding into a context.  The new binding shadows any
--   old binding for the same qualified name.
insert :: QName a -> b -> Ctx a b -> Ctx a b
insert (QName p n) b = Ctx . M.insertWith M.union p (M.singleton n b) . getCtx

-- | Run a computation under a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: Member (Reader (Ctx a b)) r => QName a -> b -> Sem r c -> Sem r c
extend qn b = local (insert qn b)

-- | Run a computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: Member (Reader (Ctx a b)) r => Ctx a b -> Sem r c -> Sem r c
extends = local . joinCtx

------------------------------------------------------------
-- Query
------------------------------------------------------------

-- | Check if a context is empty.
null :: Ctx a b -> Bool
null = all M.null . getCtx

-- | Look up a qualified name in an ambient context.
lookup :: Member (Reader (Ctx a b)) r => QName a -> Sem r (Maybe b)
lookup x = lookup' x <$> ask

-- | Look up a qualified name in a context.
lookup' :: QName a -> Ctx a b -> Maybe b
lookup' (QName p n) = (M.lookup n <=< M.lookup p) . getCtx

-- | Look up all the non-local bindings of a name in an ambient context.
lookupNonLocal :: Member (Reader (Ctx a b)) r => Name a -> Sem r [(ModuleName, b)]
lookupNonLocal n = lookupNonLocal' n <$> ask

-- | Look up all the non-local bindings of a name in a context.
lookupNonLocal' :: Name a -> Ctx a b -> [(ModuleName, b)]
lookupNonLocal' n = nonLocal . lookupAll' n
  where
    nonLocal bs = [(m,b) | (QName (QualifiedName m) _, b) <- bs]

-- | Look up all the bindings of an (unqualified) name in an ambient context.
lookupAll :: Member (Reader (Ctx a b)) r => Name a -> Sem r [(QName a, b)]
lookupAll n = lookupAll' n <$> ask

-- | Look up all the bindings of an (unqualified) name in a context.
lookupAll' :: Name a -> Ctx a b -> [(QName a, b)]
lookupAll' n = map (first (`QName` n)) . M.assocs . M.mapMaybe (M.lookup n) . getCtx

------------------------------------------------------------
-- Conversion
------------------------------------------------------------

-- | Return a list of the names defined by the context.
names :: Ctx a b -> [Name a]
names = concatMap M.keys . M.elems . getCtx

-- | Return a list of all the values bound by the context.
elems :: Ctx a b -> [b]
elems = concatMap M.elems . M.elems . getCtx

-- | Return a list of the qualified name-value associations in the
--   context.
assocs :: Ctx a b -> [(QName a, b)]
assocs = concatMap (uncurry modAssocs) . M.assocs . getCtx
  where
    modAssocs :: NameProvenance -> Map (Name a) b -> [(QName a, b)]
    modAssocs p = map (first (QName p)) . M.assocs

-- | Return a set of all qualified names in the context.
keysSet :: Ctx a b -> Set (QName a)
keysSet = S.unions . map (uncurry (S.map . QName) . second M.keysSet) . M.assocs . getCtx

------------------------------------------------------------
-- Traversal
------------------------------------------------------------

-- | XXX
coerceKeys :: Ctx a1 b -> Ctx a2 b
coerceKeys = Ctx . M.map (M.mapKeys coerce) . getCtx

-- | XXX
restrictKeys :: Ctx a b -> Set (QName a) -> Ctx a b
restrictKeys = undefined

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

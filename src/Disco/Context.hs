{-# LANGUAGE FlexibleContexts         #-}
-- {-# LANGUAGE FlexibleInstances        #-}
-- {-# LANGUAGE GADTs                    #-}
-- {-# LANGUAGE MultiParamTypeClasses    #-}
-- {-# LANGUAGE NondecreasingIndentation #-}
-- {-# LANGUAGE RankNTypes               #-}
-- {-# LANGUAGE TemplateHaskell          #-}
-- {-# LANGUAGE TupleSections            #-}
-- {-# LANGUAGE TypeFamilies             #-}
-- {-# LANGUAGE UndecidableInstances     #-}
-- {-# LANGUAGE ViewPatterns             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Context
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Disco.Context
       ( -- * Contexts
         Ctx, emptyCtx, singleCtx, joinCtx, joinCtxs
       , lookup, extend, extends

       ) where

import           Prelude                                 hiding (lookup)

-- import           Control.Applicative                     ((<|>))
-- import           Control.Arrow                           ((&&&))
-- import           Control.Lens                            ((%~), (&), _1, _2)
-- import           Control.Monad.Except
import           Control.Monad.Reader
-- import           Control.Monad.State
-- import           Data.Bifunctor                          (first)
-- import           Data.Coerce
-- import           Data.List                               (group, partition,
--                                                           sort)
import qualified Data.Map                                as M

import           Unbound.Generics.LocallyNameless
-- import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

-- import           Disco.AST.Surface
-- import           Disco.AST.Typed
-- import           Disco.Syntax.Operators
-- import           Disco.Types

-- import           Math.NumberTheory.Primes.Testing        (isPrime)

-- | A context maps names to things.
type Ctx a b = M.Map (Name a) b

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

-- | Look up a name in a context, XXX
lookup :: MonadReader (Ctx a b) m => Name a -> m (Maybe b)
lookup x = M.lookup x <$> ask

-- | Run a  XXX
--   The new binding shadows any old binding for the same name.
extend :: MonadReader (Ctx a b) m => Name a -> b -> m r -> m r
extend x b = local (M.insert x b)

-- | Run a @TCM@ computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: MonadReader (Ctx a b) m => Ctx a b -> m r -> m r
extends ctx = local (joinCtx ctx)

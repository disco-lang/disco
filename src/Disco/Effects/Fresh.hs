{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Fresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for fresh name generation, via the unbound-generics
-- library.
--
-----------------------------------------------------------------------------

module Disco.Effects.Fresh where

import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.State
import qualified Unbound.Generics.LocallyNameless      as U
import           Unbound.Generics.LocallyNameless.Name

-- | Fresh name generation effect, supporting raw generation of fresh
--   names, and opening binders with automatic freshening.
data Fresh m a where
  Fresh  :: Name x -> Fresh m (Name x)

makeSem ''Fresh

-- | Dispatch the fresh name generation effect.
runFresh' :: Integer -> Sem (Fresh ': r) a -> Sem r a
runFresh' i
  = evalState i
  . reinterpret \case
      Fresh x -> case x of
        Fn s _ -> do
          n <- get
          put $! n + 1
          return (Fn s n)
        nm@Bn{} -> return nm

    -- Above code copied from
    -- https://hackage.haskell.org/package/unbound-generics-0.4.1/docs/src/Unbound.Generics.LocallyNameless.Fresh.html ;
    -- see instance Monad m => Fresh (FreshMT m) .

    -- It turns out to make things much simpler to reimplement the
    -- Fresh effect ourselves in terms of a state effect, since then
    -- we can immediately dispatch it.  The alternative would be to
    -- implement it in terms of (Embed U.FreshM), but then we are
    -- stuck with that constraint.  Given the constraint-absorbing
    -- machinery below, just impementing the 'fresh' effect itself
    -- means we can then reuse other things from unbound-generics that
    -- depend on a Fresh constraint, such as the 'unbind' function
    -- below.

-- | Run a computation requiring only fresh name generation.
runFresh :: Sem (Fresh ': r) a -> Sem r a
runFresh = runFresh' 0

-- | Run a computation requiring only fresh name generation, beginning
--   with 1 instead of 0 for the initial freshly generated name.
runFresh1 :: Sem (Fresh ': r) a -> Sem r a
runFresh1 = runFresh' 1

------------------------------------------------------------
-- Other functions

unbind :: (Member Fresh r, U.Alpha p, U.Alpha t) => U.Bind p t -> Sem r (p, t)
unbind b = absorbFresh (U.unbind b)

------------------------------------------------------------
-- Machinery for absorbing MTL-style constraint.
-- See https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/Polysemy-ConstraintAbsorber.html
-- Used https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/src/Polysemy.ConstraintAbsorber.MonadState.html#absorbState as a template.

absorbFresh :: Member Fresh r => (U.Fresh (Sem r) => Sem r a) -> Sem r a
absorbFresh = absorbWithSem @U.Fresh @Action (FreshDict fresh) (Sub Dict)
{-# INLINEABLE absorbFresh #-}

newtype FreshDict m = FreshDict { fresh_ :: forall x. Name x -> m (Name x) }

-- | Wrapper for a monadic action with phantom type parameter for reflection.
--   Locally defined so that the instance we are going to build with reflection
--   must be coherent, that is there cannot be orphans.
newtype Action m s' a = Action (m a)
  deriving (Functor, Applicative, Monad)

instance ( Monad m
         , Reifies s' (FreshDict m)
         ) => U.Fresh (Action m s') where
  fresh x = Action $ fresh_ (reflect $ Proxy @s') x
  {-# INLINEABLE fresh #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Effects.Fresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for fresh name generation, compatible with the
-- unbound-generics library.
module Disco.Effects.Fresh where

import Disco.Effects.Counter
import Disco.Names (QName, localName)
import Polysemy
import Polysemy.ConstraintAbsorber
import qualified Unbound.Generics.LocallyNameless as U
import Unbound.Generics.LocallyNameless.Name

-- | Fresh name generation effect, supporting raw generation of fresh
--   names, and opening binders with automatic freshening.  Simply
--   increments a global counter every time 'fresh' is called and
--   makes a variable with that numeric suffix.
data Fresh m a where
  Fresh :: Name x -> Fresh m (Name x)

makeSem ''Fresh

-- | Dispatch the fresh name generation effect, starting at a given
--   integer.
runFresh' :: Integer -> Sem (Fresh ': r) a -> Sem r a
runFresh' i =
  runCounter' i
    . reinterpret \case
      Fresh x -> case x of
        Fn s _ -> Fn s <$> next
        nm@Bn {} -> return nm

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

-- | Run a computation requiring fresh name generation, beginning with
--   0 for the initial freshly generated name.
runFresh :: Sem (Fresh ': r) a -> Sem r a
runFresh = runFresh' 0

-- | Run a computation requiring fresh name generation, beginning with
--   1 instead of 0 for the initial freshly generated name.
runFresh1 :: Sem (Fresh ': r) a -> Sem r a
runFresh1 = runFresh' 1

------------------------------------------------------------
-- Other functions

-- | Open a binder, automatically creating fresh names for the bound
--   variables.
unbind :: (Member Fresh r, U.Alpha p, U.Alpha t) => U.Bind p t -> Sem r (p, t)
unbind b = absorbFresh (U.unbind b)

-- | Generate a fresh (local, free) qualified name based on a given
--   string.
freshQ :: (Member Fresh r) => String -> Sem r (QName a)
freshQ s = localName <$> fresh (string2Name s)

------------------------------------------------------------
-- Machinery for absorbing MTL-style constraint.
-- See https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/Polysemy-ConstraintAbsorber.html
-- Used https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/src/Polysemy.ConstraintAbsorber.MonadState.html#absorbState as a template.

-- | Run a 'Sem' computation requiring a 'U.Fresh' constraint (from
--   the @unbound-generics@ library) in terms of an available 'Fresh'
--   effect.
absorbFresh :: Member Fresh r => (U.Fresh (Sem r) => Sem r a) -> Sem r a
absorbFresh = absorbWithSem @U.Fresh @Action (FreshDict fresh) (Sub Dict)
{-# INLINEABLE absorbFresh #-}

newtype FreshDict m = FreshDict {fresh_ :: forall x. Name x -> m (Name x)}

-- | Wrapper for a monadic action with phantom type parameter for reflection.
--   Locally defined so that the instance we are going to build with reflection
--   must be coherent, that is there cannot be orphans.
newtype Action m s' a = Action (m a)
  deriving (Functor, Applicative, Monad)

instance
  ( Monad m
  , Reifies s' (FreshDict m)
  ) =>
  U.Fresh (Action m s')
  where
  fresh x = Action $ fresh_ (reflect $ Proxy @s') x
  {-# INLINEABLE fresh #-}

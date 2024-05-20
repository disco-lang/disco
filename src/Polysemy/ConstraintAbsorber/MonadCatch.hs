{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module was copied from polysemy-zoo:
-- https://hackage.haskell.org/package/polysemy-zoo-0.8.2.0/docs/src/Polysemy.ConstraintAbsorber.MonadCatch.html

module Polysemy.ConstraintAbsorber.MonadCatch (
  -- * Constraint Absorbers
  absorbMonadThrow,
  absorbMonadCatch,

  -- * run helper
  runMonadCatch,
  runMonadCatchAsText,

  -- * Re-exports
  Exception (..),
  SomeException,
)
where

import Control.Monad.Catch (
  Exception (..),
  SomeException,
  toException,
 )
import qualified Control.Monad.Catch as C

import qualified Data.Text as T
import Polysemy
import Polysemy.ConstraintAbsorber
import qualified Polysemy.Error as E

------------------------------------------------------------------------------

-- | Like 'E.runError' but applies a given function from 'SomeException'
-- to some other type, typically something less opaque.
-- e.g.:
--  @runMonadCatch C.displayException@
runMonadCatch ::
  Exception e =>
  (Maybe e -> e') ->
  Sem (E.Error C.SomeException : E.Error e' : r) a ->
  Sem r (Either e' a)
runMonadCatch f = E.runError . E.mapError (f . C.fromException)

runMonadCatchAsText ::
  Sem (E.Error C.SomeException : E.Error T.Text : r) a ->
  Sem r (Either T.Text a)
runMonadCatchAsText = E.runError . E.mapError (T.pack . C.displayException)

-- | Introduce a local 'S.MonadCatch' constraint on 'Sem' --- allowing it to
-- interop nicely with exceptions
absorbMonadCatch ::
  Member (E.Error C.SomeException) r =>
  -- | A computation that requires an instance of 'C.MonadCatch'
  -- or 'C.MonadThrow' for
  -- 'Sem'. This might be something with type @'C.MonadCatch' e m => m a@.
  (C.MonadCatch (Sem r) => Sem r a) ->
  Sem r a
absorbMonadCatch =
  absorbWithSem @C.MonadCatch @Action (CatchDict E.throw E.catch) (Sub Dict)
{-# INLINEABLE absorbMonadCatch #-}

-- | Introduce a local 'S.MonadThrow' constraint on 'Sem' --- allowing it to
-- interop nicely with exceptions
absorbMonadThrow ::
  Member (E.Error C.SomeException) r =>
  -- | A computation that requires an instance of 'C.MonadCatch'
  -- or 'C.MonadThrow' for
  -- 'Sem'. This might be something with type @'C.MonadCatch' e m => m a@.
  (C.MonadThrow (Sem r) => Sem r a) ->
  Sem r a
absorbMonadThrow main = absorbMonadCatch main
{-# INLINEABLE absorbMonadThrow #-}

------------------------------------------------------------------------------

-- | A dictionary of the functions we need to supply
-- to make an instance of Error
data CatchDict m = CatchDict
  { throwM_ :: forall a. C.SomeException -> m a
  , catch_ :: forall a. m a -> (C.SomeException -> m a) -> m a
  }

------------------------------------------------------------------------------

-- | Wrapper for a monadic action with phantom
-- type parameter for reflection.
-- Locally defined so that the instance we are going
-- to build with reflection must be coherent, that is
-- there cannot be orphans.
newtype Action m s' a = Action {action :: m a}
  deriving (Functor, Applicative, Monad)

------------------------------------------------------------------------------

-- | Given a reifiable mtl Error dictionary,
-- we can make an instance of @MonadError@ for the action
-- wrapped in @Action@.
instance
  ( Monad m
  , Reifies s' (CatchDict m)
  ) =>
  C.MonadThrow (Action m s')
  where
  throwM e = Action $ throwM_ (reflect $ Proxy @s') (C.toException e)
  {-# INLINEABLE throwM #-}

instance
  ( Monad m
  , Reifies s' (CatchDict m)
  ) =>
  C.MonadCatch (Action m s')
  where
  catch x f =
    let catchF = catch_ (reflect $ Proxy @s')
     in Action $
          (action x) `catchF` \e -> case C.fromException e of
            Just e' -> action $ f e'
            _ -> throwM_ (reflect $ Proxy @s') (C.toException e)
  {-# INLINEABLE catch #-}

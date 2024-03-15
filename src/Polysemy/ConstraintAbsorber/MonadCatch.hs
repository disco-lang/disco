-- Copied from polysemy-zoo.
{-
Copyright Sandy Maguire (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandy Maguire nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Polysemy.ConstraintAbsorber.MonadCatch (absorbMonadCatch) where

import qualified Control.Monad.Catch as C
import Polysemy
import Polysemy.ConstraintAbsorber
import qualified Polysemy.Error as E

------------------------------------------------------------------------------

-- -- | Like 'E.runError' but applies a given function from 'SomeException'
-- -- to some other type, typically something less opaque.
-- -- e.g.:
-- --  @runMonadCatch C.displayException@
-- --
-- -- @since 0.7.0.0
-- runMonadCatch ::
--   Exception e =>
--   (Maybe e -> e') ->
--   Sem (E.Error C.SomeException : E.Error e' : r) a ->
--   Sem r (Either e' a)
-- runMonadCatch f = E.runError . E.mapError (f . C.fromException)

-- runMonadCatchAsText ::
--   Sem (E.Error C.SomeException : E.Error T.Text : r) a ->
--   Sem r (Either T.Text a)
-- runMonadCatchAsText = E.runError . E.mapError (T.pack . C.displayException)

-- | Introduce a local 'S.MonadCatch' constraint on 'Sem' --- allowing it to
-- interop nicely with exceptions
--
-- @since 0.7.0.0
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

-- -- | Introduce a local 'S.MonadThrow' constraint on 'Sem' --- allowing it to
-- -- interop nicely with exceptions
-- --
-- -- @since 0.7.0.0
-- absorbMonadThrow ::
--   Member (E.Error C.SomeException) r =>
--   -- | A computation that requires an instance of 'C.MonadCatch'
--   -- or 'C.MonadThrow' for
--   -- 'Sem'. This might be something with type @'C.MonadCatch' e m => m a@.
--   (C.MonadThrow (Sem r) => Sem r a) ->
--   Sem r a
-- absorbMonadThrow main = absorbMonadCatch main
-- {-# INLINEABLE absorbMonadThrow #-}

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

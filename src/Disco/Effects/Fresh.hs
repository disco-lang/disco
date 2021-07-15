{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Unbound.Generics.LocallyNameless as U

-- | Fresh name generation effect, supporting raw generation of fresh
--   names, and opening binders with automatic freshening.
data Fresh m a where
  Fresh  :: U.Name x -> Fresh m (U.Name x)
  Unbind :: (U.Alpha p, U.Alpha t) => U.Bind p t -> Fresh m (p, t)

makeSem ''Fresh

-- | Dispatch the fresh name generation effect in an effect stack
--   containing the 'FreshM' monad from @unbound-generics@.
runFreshR :: Member (Embed U.FreshM) r => Sem (Fresh ': r) a -> Sem r a
runFreshR = interpret $ \case
  Fresh x  -> embed @U.FreshM (U.fresh x)
  Unbind b -> embed @U.FreshM (U.unbind b)

-- | Run a computation requiring only fresh name generation.
runFresh :: Sem '[Fresh, Embed U.FreshM] a -> a
runFresh = U.runFreshM . runM . runFreshR

-- | Run a computation requiring only fresh name generation, beginning
--   with 1 instead of 0 for the initial freshly generated name.
runFresh1 :: Sem '[Fresh, Embed U.FreshM] a -> a
runFresh1 = flip U.contFreshM 1 . runM . runFreshR

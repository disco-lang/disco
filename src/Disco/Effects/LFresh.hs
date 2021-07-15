{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.LFresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for local fresh name generation, via the
-- unbound-generics library.
--
-----------------------------------------------------------------------------

module Disco.Effects.LFresh where

import           Polysemy
import           Polysemy.Final
import qualified Unbound.Generics.LocallyNameless as U

-- | Local fresh name generation effect.
data LFresh m a where
  Avoid   :: [U.AnyName] -> m a -> LFresh m a
  Lunbind :: (U.Alpha p, U.Alpha t) => U.Bind p t -> ((p,t) -> m c) -> LFresh m c

makeSem ''LFresh

-- | Dispatch the local fresh name generation effect in an effect stack
--   containing the 'LFreshM' monad from @unbound-generics@.
runLFreshR :: Member (Final U.LFreshM) r => Sem (LFresh ': r) a -> Sem r a
runLFreshR = interpretFinal @U.LFreshM $ \case
  Avoid xs m  -> do
    m' <- runS m
    pure (U.avoid xs m')
  Lunbind b k -> do
    s <- getInitialStateS
    k' <- bindS k
    pure (U.lunbind b (k' . (<$ s)))

-- The above code took me a long time to figure out how to write.
-- lunbind is a higher-order effect, so we have to use more
-- complicated machinery.  See my Stack Overflow question,
-- https://stackoverflow.com/questions/68384508/how-to-incorporate-mtl-style-cps-style-higher-order-effect-into-polysemy/68397358#68397358

-- | Run a computation requiring only fresh name generation.
runLFresh :: Sem '[LFresh, Final U.LFreshM] a -> a
runLFresh = U.runLFreshM . runFinal . runLFreshR

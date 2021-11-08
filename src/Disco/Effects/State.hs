{-# LANGUAGE BlockArguments #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.State
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions for state effect.
--
-----------------------------------------------------------------------------

module Disco.Effects.State
  ( module Polysemy.State
  , zoom
  )
  where

import           Control.Lens   (Lens', view, (.~))

import           Polysemy
import           Polysemy.State

-- | Use a lens to zoom into a component of a state.
zoom :: forall s a r c. Member (State s) r => Lens' s a -> Sem (State a ': r) c -> Sem r c
zoom l = interpret \case
  Get   -> view l <$> get
  Put a -> modify (l .~ a)

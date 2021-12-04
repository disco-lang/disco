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
  , use
  ,(%=),(.=))
  where

import           Control.Lens   (Getter, Lens', view, (%~), (.~))

import           Polysemy
import           Polysemy.State

-- | Use a lens to zoom into a component of a state.
zoom :: forall s a r c. Member (State s) r => Lens' s a -> Sem (State a ': r) c -> Sem r c
zoom l = interpret \case
  Get   -> view l <$> get
  Put a -> modify (l .~ a)

use :: Member (State s) r => Getter s a -> Sem r a
use = gets . view

infix 4 .=, %=

(.=) :: Member (State s) r => Lens' s a -> a -> Sem r ()
l .= a = modify (l .~ a)

(%=) :: Member (State s) r => Lens' s a -> (a -> a) -> Sem r ()
l %= f = modify (l %~ f)

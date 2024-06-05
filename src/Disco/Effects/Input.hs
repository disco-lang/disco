-- |
-- Module      :  Disco.Effects.Input
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions for input effect.
module Disco.Effects.Input (
  module Polysemy.Input,
  inputToState,
  mapInput,
)
where

import Polysemy
import Polysemy.Input
import Polysemy.State

-- | Run an input effect in terms of an ambient state effect.
inputToState :: forall s r a. Member (State s) r => Sem (Input s ': r) a -> Sem r a
inputToState = interpret (\case Input -> get @s)

-- | Use a function to (contravariantly) transform the input value in
--   an input effect.
mapInput :: forall s t r a. Member (Input s) r => (s -> t) -> Sem (Input t ': r) a -> Sem r a
mapInput f = interpret (\case Input -> inputs @s f)

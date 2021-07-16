{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
--
-----------------------------------------------------------------------------

module Disco.Effects where

import           Disco.Effects.Counter
import           Disco.Effects.LFresh
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.State

type DiscoEffects' env m e = '[Reader env, State m, Counter, Error e, LFresh]

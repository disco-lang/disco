{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Counter
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for integer counter.
--
-----------------------------------------------------------------------------

module Disco.Effects.Counter where

import           Polysemy
import           Polysemy.State

-- | Counter effect.
data Counter m a where
  -- | Return the next available int.
  Next  :: Counter m Int

makeSem ''Counter

-- | Dispatch a counter effect, starting the counter from zero.
runCounter :: Sem (Counter ': r) a -> Sem r a
runCounter
  = evalState (0 :: Int)
  . reinterpret \case
      Next -> do
        n <- get
        put (n+1)
        return n


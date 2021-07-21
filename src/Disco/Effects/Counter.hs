{-# LANGUAGE BlockArguments  #-}
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

data Counter m a where

  -- | Return the next integer in sequence.
  Next  :: Counter m Integer

makeSem ''Counter

-- | Dispatch a counter effect, starting the counter from the given
--   Integer.
runCounter' :: Integer -> Sem (Counter ': r) a -> Sem r a
runCounter' i
  = evalState i
  . reinterpret \case
      Next -> do
        n <- get
        put (n+1)
        return n

-- | Dispatch a counter effect, starting the counter from zero.
runCounter :: Sem (Counter ': r) a -> Sem r a
runCounter = runCounter' 0

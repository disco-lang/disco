-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Random
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions for random effect.
--
-----------------------------------------------------------------------------

module Disco.Effects.Random
  ( module Polysemy.Random
  , runGen
  )
  where

import           Polysemy
import           Polysemy.Random
import qualified System.Random.SplitMix as SM
import qualified Test.QuickCheck.Gen    as QC
import qualified Test.QuickCheck.Random as QCR

import           Data.Word              (Word64)

-- | Run a QuickCheck generator using a 'Random' effect.
runGen :: Member Random r => QC.Gen a -> Sem r a
runGen g = do
  n <- random @_ @Int
  w <- random @_ @Word64
  return $ QC.unGen g (QCR.QCGen (SM.mkSMGen w)) n

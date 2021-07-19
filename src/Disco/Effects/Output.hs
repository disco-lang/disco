-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Output
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions for output effect.
--
-----------------------------------------------------------------------------

module Disco.Effects.Output
  ( module Polysemy.Output
  , outputLn
  , printout
  )
  where

import           Polysemy
import           Polysemy.Output

-- | Output a string with a trailing newline.
outputLn :: Member (Output String) r => String -> Sem r ()
outputLn s = output s >> output "\n"

-- | Output an item after calling @show@ on it.
printout :: (Show a, Member (Output String) r) => a -> Sem r ()
printout = output . show

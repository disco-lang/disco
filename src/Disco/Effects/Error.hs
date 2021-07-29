-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions for error effect.
--
-----------------------------------------------------------------------------

module Disco.Effects.Error
  ( module Polysemy.Error
  , outputErrors
  )
  where

import           Disco.Effects.Output
import           Polysemy
import           Polysemy.Error

-- | Run an error effect by simply printing out the errors, using an
--   ambient output effect.  This is a stopgap for now, eventually
--   intended to be replaced by something more sophisticated.
outputErrors :: (Show e, Member (Output String) r) => Sem (Error e ': r) () -> Sem r ()
outputErrors m = do
  e <- runError m
  either printoutLn return e

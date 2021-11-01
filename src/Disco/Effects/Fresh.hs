{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.Fresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for fresh name generation, compatible with the
-- unbound-generics library.
--
-----------------------------------------------------------------------------

module Disco.Effects.Fresh where

import           Disco.Effects.Counter
import           Polysemy
import           Polysemy.ConstraintAbsorber

-- | Fresh name generation effect, supporting raw generation of fresh
--   names, and opening binders with automatic freshening.  Simply
--   increments a global counter every time 'fresh' is called and
--   makes a variable with that numeric suffix.
data Fresh m a where
  Fresh  :: Name x -> Fresh m (Name x)

makeSem ''Fresh

-- | Dispatch the fresh name generation effect, starting at a given
--   integer.
runFresh' :: Integer -> Sem (Fresh ': r) a -> Sem r a
runFresh' i
  = runCounter' i
  . reinterpret \case
      Fresh x -> case x of
        Fn s _  -> Fn s <$> next
        nm@Bn{} -> return nm

-- | Run a computation requiring fresh name generation, beginning with
--   0 for the initial freshly generated name.
runFresh :: Sem (Fresh ': r) a -> Sem r a
runFresh = runFresh' 0

-- | Run a computation requiring fresh name generation, beginning with
--   1 instead of 0 for the initial freshly generated name.
runFresh1 :: Sem (Fresh ': r) a -> Sem r a
runFresh1 = runFresh' 1

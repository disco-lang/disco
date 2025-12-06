{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Disco.Effects.FileSystem
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for read/write filesystem access.
module Disco.Effects.FileSystem where

import Control.Exception (SomeException, handle)
import Control.Monad.IO.Class (liftIO)
import Polysemy

data FileSystem m a where
  ReadFile :: FilePath -> FileSystem m (Maybe String)

makeSem ''FileSystem

-- | Dispatch a filesystem effect using raw I/O.
runFileSystemIO :: forall r a. Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret \case
  ReadFile file ->
    liftIO $
      handle @SomeException (const (pure Nothing)) (Just <$> Prelude.readFile file)

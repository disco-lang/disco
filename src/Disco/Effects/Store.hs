{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Disco.Effects.Store
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for a memory store with integer keys.
module Disco.Effects.Store where

import Data.Proxy
import qualified Data.IntMap.Lazy as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Disco.Effects.Counter
import Polysemy
import Polysemy.State

data Store v m a where
  ClearStore :: Proxy v -> Store v m ()
  New :: v -> Store v m Int
  LookupStore :: Int -> Store v m (Maybe v)
  InsertStore :: Int -> v -> Store v m ()
  MapStore :: (v -> v) -> Store v m ()
  AssocsStore :: Store v m [(Int, v)]
  KeepKeys :: Proxy v -> IntSet -> Store v m ()

makeSem ''Store

-- | Dispatch a store effect.
runStore :: forall v r a. Sem (Store v ': r) a -> Sem r a
runStore =
  runCounter
    . evalState @(IntMap.IntMap v) IntMap.empty
    . reinterpret2 \case
      ClearStore _ -> put @(IntMap.IntMap v) IntMap.empty
      New v -> do
        loc <- fromIntegral <$> next
        modify $ IntMap.insert loc v
        return loc
      LookupStore k -> gets (IntMap.lookup k)
      InsertStore k v -> modify (IntMap.insert k v)
      MapStore f -> modify (IntMap.map f)
      AssocsStore -> gets IntMap.assocs
      KeepKeys _ ks ->
        modify @(IntMap.IntMap v) $ \m ->
          IntMap.withoutKeys m (IntMap.keysSet m `IntSet.difference` ks)

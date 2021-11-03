{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Effects.LFresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for local fresh name generation, compatible with
-- the unbound-generics library.
--
-----------------------------------------------------------------------------

module Disco.Effects.LFresh where

import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Typeable               (Typeable)
import           Polysemy
import           Polysemy.ConstraintAbsorber
import           Polysemy.Reader

import           Disco.Names


-- | Local fresh name generation effect.
data LFresh m a where
  Lfresh    :: Typeable a => Name a -> LFresh m (Name a)
  Avoid     :: [AnyName] -> m a -> LFresh m a
  GetAvoids :: LFresh m (Set AnyName)

makeSem ''LFresh

-- | Dispatch an 'LFresh' effect via a 'Reader' effect to keep track
--   of a set of in-scope names.
runLFresh :: Sem (LFresh ': r) a -> Sem r a
runLFresh = runReader S.empty . runLFresh'

runLFresh' :: Sem (LFresh ': r) a -> Sem (Reader (Set AnyName) ': r) a
runLFresh'
  = reinterpretH @_ @(Reader (Set AnyName)) \case
      Lfresh nm -> do
        let s = name2String nm
        used <- ask
        pureT $ head (filter (\x -> not (S.member (AnyName x) used))
                       (map (makeName s) [0..]))
      Avoid names m -> do
        m' <- runT m
        raise (subsume (runLFresh' (local (S.union (S.fromList names)) m')))
      GetAvoids  -> ask >>= pureT

  -- Much of the above code copied from unbound-generics
  -- (see instance Monad m => LFresh (LFreshMT m))

  -- NOTE: originally, there was a single function runLFresh which
  -- called reinterpretH and then immediately dispatched the Reader
  -- (Set AnyName) effect.  However, since runLFresh is recursive,
  -- this means that the recursive calls were running with a
  -- completely *separate* Reader effect that started over from the
  -- empty set! This meant that LFresh basically never changed any
  -- names, leading to all sorts of name clashes and crashes.
  --
  -- Instead, we need to organize things as above: runLFresh' is
  -- recursive, and keeps the Reader effect (using 'subsume' to squash
  -- the duplicated Reader effects together).  Then a top-level
  -- runLFresh function finally runs the Reader effect.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Capability
-- Copyright   :  (c) 2021 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Utilities related to the 'capability' library.
--
-----------------------------------------------------------------------------

module Disco.Capability where

import qualified Capability.Constraints           as CC
import           Capability.Error
import           Capability.Reader
import           Capability.Reflection            (interpret)
import           Capability.Source
import           Capability.State
import           Capability.Writer
import           Control.Monad.IO.Class
import           Data.IORef

import           Unbound.Generics.LocallyNameless

type Rd tag = HasReader' tag
type Wr tag = HasWriter' tag
type St tag = HasState' tag
type Sc tag = HasSource' tag
type Th tag = HasThrow' tag
type Ct tag = HasCatch' tag

type Has cs m = CC.All cs m

type MonadDisco m = Has '[Rd "env", St "mem", Sc "nextloc", Th "err", Ct "err", MonadIO, MonadFail, LFresh] m

-- | Interpret an action while providing an additional local state
--   capability, stored internally using an IORef.
withLocalState
  :: forall k (tag :: k) st (cs :: [(* -> *) -> CC.Constraint]) m a.
     (MonadIO m, Has cs m)
  => st -> (forall m'. (HasState tag st m', MonadIO m', Has cs m') => m' a) -> m (a, st)
withLocalState s m = do
  ref <- liftIO $ newIORef s
  interpret @tag @(MonadIO : cs) ReifiedState
    { _state = \f -> do
        b <- liftIO $ readIORef ref
        let (a, b') = f b
        liftIO $ writeIORef ref b'
        pure a
    , _stateSource = ReifiedSource
        { _await = liftIO $ readIORef ref }
    , _stateSink = ReifiedSink
        { _yield = liftIO . writeIORef ref }
    } $ do
      a <- m
      s' <- get @tag
      return (a,s')

withLocalReader
  :: forall k (tag :: k) r (cs :: [(* -> *) -> CC.Constraint]) m a.
     (MonadIO m, Has cs m)
  => r -> (forall m'. (HasReader tag r m', MonadIO m', Has cs m') => m' a) -> m a
withLocalReader r m = do
  ref <- liftIO $ newIORef r
  interpret @tag @(MonadIO : cs) ReifiedReader
    { _readerSource = ReifiedSource { _await = liftIO $ readIORef ref }
    , _local = \f m' -> do
        old <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (f old)
        a <- m'
        liftIO $ writeIORef ref old
        pure a
    , _reader = \f -> f <$> liftIO (readIORef ref)
    }
    m

withLocalWriter
  :: forall k (tag :: k) w (cs :: [(* -> *) -> CC.Constraint]) m a.
     (MonadIO m, Has cs m, Monoid w)
  => (forall m'. (HasWriter tag w m', MonadIO m', Has cs m') => m' a) -> m (a, w)
withLocalWriter m = do
  ref <- liftIO $ newIORef (mempty :: w)
  interpret @tag @(MonadIO : cs) ReifiedWriter
    { _writerSink = ReifiedSink
      { _yield = \a -> liftIO $ modifyIORef ref (<>a)
      }
    , _writer = \(a, w) -> liftIO (modifyIORef ref (<>w) >> return a)
    , _listen = \m' -> do
        a <- m'
        w <- liftIO $ readIORef ref
        return (a, w)
    , _pass = \m' -> do
        (a, f) <- m'
        liftIO $ modifyIORef ref f
        return a
    } (listen @tag m)

-- withLocalFresh
--   :: forall (cs :: [CC.Capability]) m a.
--      Has cs m
--   => (forall m'. (Fresh m', Has cs m') => m' a) -> m a
-- withLocalFresh =
--   runFreshMT $

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Disco.Effects.LFresh
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Polysemy effect for local fresh name generation, compatible with
-- the unbound-generics library.
module Disco.Effects.LFresh where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Disco.Util (gate, iterUntil)
import Polysemy
import Polysemy.ConstraintAbsorber
import Polysemy.Reader
import qualified Unbound.Generics.LocallyNameless as U
import Unbound.Generics.LocallyNameless.Name

-- | Local fresh name generation effect.
data LFresh m a where
  Lfresh :: Typeable a => Name a -> LFresh m (Name a)
  Avoid :: [AnyName] -> m a -> LFresh m a
  GetAvoids :: LFresh m (Set AnyName)

makeSem ''LFresh

-- | Dispatch an 'LFresh' effect via a 'Reader' effect to keep track
--   of a set of in-scope names.
runLFresh :: Sem (LFresh ': r) a -> Sem r a
runLFresh = runReader S.empty . runLFresh'

runLFresh' :: Sem (LFresh ': r) a -> Sem (Reader (Set AnyName) ': r) a
runLFresh' =
  reinterpretH @_ @(Reader (Set AnyName)) \case
    Lfresh nm -> do
      let s = name2String nm
      used <- ask
      let ok n = S.notMember (AnyName n) used
      pureT $ iterUntil (+1) (gate ok . makeName s) 0
    Avoid names m -> do
      m' <- runT m
      raise (subsume (runLFresh' (local (S.union (S.fromList names)) m')))
    GetAvoids -> ask >>= pureT

-- Much of the above code copied from
-- https://hackage.haskell.org/package/unbound-generics-0.4.1/docs/src/Unbound.Generics.LocallyNameless.LFresh.html
-- (see instance Monad m => LFresh (LFreshMT m))

-- It turns out to make things much simpler to reimplement the
-- LFresh effect ourselves in terms of a reader effect, since then
-- we can immediately dispatch it as above.  The alternative would
-- be to implement it in terms of (Final U.LFreshM) (see the
-- commented code at the bottom of this file), but then we are stuck
-- with that constraint.  Given the constraint-absorbing machinery
-- below, just impementing the 'LFresh' effect itself means we can
-- then reuse other things from unbound-generics that depend on a
-- Fresh constraint, such as the 'lunbind' function below.

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

--------------------------------------------------
-- Other functions

-- | Open a binder, automatically freshening the names of the bound
--   variables, and providing the opened pattern and term to the
--   provided continuation.  The bound variables are also added to the
--   set of in-scope variables within in the continuation.
lunbind ::
  (Member LFresh r, U.Alpha p, U.Alpha t) =>
  U.Bind p t ->
  ((p, t) -> Sem r c) ->
  Sem r c
lunbind b k = absorbLFresh (U.lunbind b k)

------------------------------------------------------------
-- Machinery for absorbing MTL-style constraint.
-- See https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/Polysemy-ConstraintAbsorber.html
-- Used https://hackage.haskell.org/package/polysemy-zoo-0.7.0.1/docs/src/Polysemy.ConstraintAbsorber.MonadState.html#absorbState as a template.

absorbLFresh :: Member LFresh r => (U.LFresh (Sem r) => Sem r a) -> Sem r a
absorbLFresh = absorbWithSem @U.LFresh @Action (LFreshDict lfresh avoid getAvoids) (Sub Dict)
{-# INLINEABLE absorbLFresh #-}

data LFreshDict m = LFreshDict
  { lfresh_ :: forall a. Typeable a => Name a -> m (Name a)
  , avoid_ :: forall a. [AnyName] -> m a -> m a
  , getAvoids_ :: m (Set AnyName)
  }

-- | Wrapper for a monadic action with phantom type parameter for reflection.
--   Locally defined so that the instance we are going to build with reflection
--   must be coherent, that is there cannot be orphans.
newtype Action m s' a = Action (m a)
  deriving (Functor, Applicative, Monad)

instance
  ( Monad m
  , Reifies s' (LFreshDict m)
  ) =>
  U.LFresh (Action m s')
  where
  lfresh x = Action $ lfresh_ (reflect $ Proxy @s') x
  {-# INLINEABLE lfresh #-}
  avoid xs (Action m) = Action $ avoid_ (reflect $ Proxy @s') xs m
  {-# INLINEABLE avoid #-}
  getAvoids = Action $ getAvoids_ (reflect $ Proxy @s')
  {-# INLINEABLE getAvoids #-}

----------------------------------------------------------------------
-- Old code I don't want to delete because I spent so much time
-- banging my head against it.  It wasn't wasted, though, since I used
-- some of my hard-earned knowledge to write runLFresh' above.

-- -- | Dispatch the local fresh name generation effect in an effect stack
-- --   containing the 'LFreshM' monad from @unbound-generics@.
-- runLFreshR :: Member (Final U.LFreshM) r => Sem (LFresh ': r) a -> Sem r a
-- runLFreshR = interpretFinal @U.LFreshM $ \case
--   Avoid xs m  -> do
--     m' <- runS m
--     pure (U.avoid xs m')
--   Lunbind b k -> do
--     s <- getInitialStateS
--     k' <- bindS k
--     pure (U.lunbind b (k' . (<$ s)))

-- -- The above code took me a long time to figure out how to write.
-- -- lunbind is a higher-order effect, so we have to use more
-- -- complicated machinery.  See my Stack Overflow question,
-- -- https://stackoverflow.com/questions/68384508/how-to-incorporate-mtl-style-cps-style-higher-order-effect-into-polysemy/68397358#68397358

-- -- | Run a computation requiring only fresh name generation.
-- runLFresh :: Sem '[LFresh, Final U.LFreshM] a -> a
-- runLFresh = U.runLFreshM . runFinal . runLFreshR

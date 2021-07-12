{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- XXX module comments etc.

module Disco.Effects.Fresh where

import           Polysemy
import qualified Unbound.Generics.LocallyNameless as U

-- | XXX
data Fresh m a where
  Fresh  :: U.Name x -> Fresh m (U.Name x)
  Unbind :: (U.Alpha p, U.Alpha t) => U.Bind p t -> Fresh m (p, t)

makeSem ''Fresh

-- | XXX
runFreshR :: Member (Embed U.FreshM) r => Sem (Fresh ': r) a -> Sem r a
runFreshR = interpret $ \case
  Fresh x  -> embed @U.FreshM (U.fresh x)
  Unbind b -> embed @U.FreshM (U.unbind b)

-- | XXX
runFresh :: Sem '[Fresh, Embed U.FreshM] a -> a
runFresh = U.runFreshM . runM . runFreshR

-- | XXX
runFresh1 :: Sem '[Fresh, Embed U.FreshM] a -> a
runFresh1 = flip U.contFreshM 1 . runM . runFreshR

-- runFresh :: Member (Embed IO) r => Sem (Fresh ': r) a -> Sem r a
-- runFresh
--   = interpret (\(Embed m) -> embed @IO (U.runFreshMT m))
--   . reinterpret (\case { Fresh x -> embed @(U.FreshMT IO) (U.fresh x) })

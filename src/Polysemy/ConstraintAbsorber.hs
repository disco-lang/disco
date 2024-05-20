{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- This module was copied from polysemy-zoo:
-- https://hackage.haskell.org/package/polysemy-zoo-0.8.2.0/docs/src/Polysemy.ConstraintAbsorber.html

module Polysemy.ConstraintAbsorber (
  -- * Absorb builder
  absorbWithSem,

  -- * Re-exports
  Reifies,
  (:-) (Sub),
  Dict (Dict),
  reflect,
  Proxy (Proxy),
) where

import Data.Constraint (Dict (Dict), (:-) (Sub), (\\))
import qualified Data.Constraint as C
import qualified Data.Constraint.Unsafe as C
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies, reflect)
import qualified Data.Reflection as R
import Polysemy

------------------------------------------------------------------------------

-- | This function can be used to locally introduce typeclass instances for
-- 'Sem'. See 'Polysemy.ConstraintAbsorber.MonadState' for an example of how to
-- use it.
absorbWithSem ::
  forall
    -- Constraint to be absorbed
    (p :: (Type -> Type) -> Constraint)
    -- Wrapper to avoid orphan instances
    (x :: (Type -> Type) -> Type -> Type -> Type)
    d
    r
    a.
  -- | Reified dictionary
  d ->
  -- | This parameter should always be @'Sub' 'Dict'@
  (forall s. R.Reifies s d :- p (x (Sem r) s)) ->
  (p (Sem r) => Sem r a) ->
  Sem r a
absorbWithSem d i m = R.reify d $ \(_ :: Proxy (s :: Type)) ->
  m
    \\ C.trans
      (C.unsafeCoerceConstraint :: (p (x m s) :- p m))
      i
{-# INLINEABLE absorbWithSem #-}

-- Copied from the polysemy-zoo package.
{-
Copyright Sandy Maguire (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandy Maguire nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Polysemy (Sem)

------------------------------------------------------------------------------

-- | This function can be used to locally introduce typeclass instances for
-- 'Sem'. See 'Polysemy.ConstraintAbsorber.MonadState' for an example of how to
-- use it.
--
-- @since 0.3.0.0
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

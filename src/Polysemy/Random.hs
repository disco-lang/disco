-- Copied from polysemy-zoo.
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
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random (
  -- * Effect
  Random (..),

  -- * Actions
  random,
  randomR,

  -- * Interpretations
  runRandom,
  runRandomIO,
) where

import Polysemy
import Polysemy.State
import qualified System.Random as R

------------------------------------------------------------------------------

-- | An effect capable of providing 'R.Random' values.
data Random m a where
  Random :: R.Random x => Random m x
  RandomR :: R.Random x => (x, x) -> Random m x

makeSem ''Random

------------------------------------------------------------------------------

-- | Run a 'Random' effect with an explicit 'R.RandomGen'.
runRandom ::
  forall q r a.
  R.RandomGen q =>
  q ->
  Sem (Random ': r) a ->
  Sem r (q, a)
runRandom q =
  runState q
    . reinterpret
      ( \case
          Random -> do
            ~(a, q') <- gets @q R.random
            put q'
            pure a
          RandomR r -> do
            ~(a, q') <- gets @q $ R.randomR r
            put q'
            pure a
      )
{-# INLINE runRandom #-}

------------------------------------------------------------------------------

-- | Run a 'Random' effect by using the 'IO' random generator.
runRandomIO :: Member (Embed IO) r => Sem (Random ': r) a -> Sem r a
runRandomIO m = do
  q <- embed @IO R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Property
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Properties of disco functions.
--
-----------------------------------------------------------------------------

module Disco.Property
       where

import qualified Data.Enumeration.Invertible as E
import qualified Test.QuickCheck             as QC

import           Disco.Effects.Random
import           Polysemy

import           Disco.Value

-- | Whether the property test resulted in a runtime error.
testIsError :: TestResult -> Bool
testIsError (TestResult _ (TestRuntimeError _) _) = True
testIsError _                                     = False

-- | Whether the property test resulted in success.
testIsOk :: TestResult -> Bool
testIsOk (TestResult b _ _) = b

-- | The reason the property test had this result.
testReason :: TestResult -> TestReason
testReason (TestResult _ r _) = r

testEnv :: TestResult -> TestEnv
testEnv (TestResult _ _ e) = e

-- | Toggles which outcome (finding or not finding the thing being
--   searched for) qualifies as success, without changing the thing
--   being searched for.
invertMotive :: SearchMotive -> SearchMotive
invertMotive (SearchMotive (a, b)) = SearchMotive (not a, b)

-- | Flips the success or failure status of a @PropResult@, leaving
--   the explanation unchanged.
invertPropResult :: TestResult -> TestResult
invertPropResult res@(TestResult b r env)
  | TestRuntimeError _ <- r = res
  | otherwise               = TestResult (not b) r env

-- | Select samples from an enumeration according to a search type. Also returns
--   a 'SearchType' describing the results, which may be 'Exhaustive' if the
--   enumeration is no larger than the number of samples requested.
generateSamples :: Member Random r => SearchType -> E.IEnumeration a -> Sem r ([a], SearchType)
generateSamples Exhaustive e           = return (E.enumerate e, Exhaustive)
generateSamples (Randomized n m) e
  | E.Finite k <- E.card e, k <= n + m = return (E.enumerate e, Exhaustive)
  | otherwise                          = do
    let small = [0 .. n]
    rs <- runGen . mapM sizedNat $ [n .. n + m]
    let samples = map (E.select e) $ small ++ rs
    return (samples, Randomized n m)
  where
    sizedNat k = QC.resize (fromIntegral k) QC.arbitrarySizedNatural

-- XXX do shrinking for randomly generated test cases?

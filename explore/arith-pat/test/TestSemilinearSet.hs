{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestSemilinearSet (runTests) where

import Control.Monad (unless)
import SemilinearSet
import Test.QuickCheck

-- Typeclasses -----------------------------------------------------------------

instance Arbitrary LS where
  arbitrary = do
    x <- chooseInt (-maxTestSize, maxTestSize)
    p <- chooseInt (-maxTestSize, maxTestSize)
    return $ LS (x, p)

instance Arbitrary SS where
  arbitrary = do
    lss <- scale (`div` 20) $ listOf arbitrary
    return $ SS lss

-- Test Helpers ----------------------------------------------------------------

-- | The absolute value limit up to which infinite lists should be tested.
maxTestSize :: Int
maxTestSize = 40

-- | Determines whether two semilinear sets consist of the same values up to
-- `maxTestSize`.
listEquivalent :: SS -> SS -> Bool
listEquivalent a b = toList' a == toList' b
  where toList' = takeWhile ((<= maxTestSize) . abs) . toListSS

-- | Checks if a set is equivalent to the set of all numbers.
isAllSS :: SS -> Bool
isAllSS = listEquivalent allSS

-- | Checks if a set covers all natural numbers.
coversNats :: SS -> Bool
coversNats ss
  = isEmptySS $ intersectSS natSS (complementSS ss)

-- | Checks if the second set is a subset of the first.
isSubsetOf :: SS -> SS -> Bool
isSubsetOf sup sub = isEmptySS $ intersectSS sub (complementSS sup)

-- | Checks if the second set is a superset of the first.
isSupersetOf :: SS -> SS -> Bool
isSupersetOf = flip isSubsetOf

-- Property Tests --------------------------------------------------------------

-- | Tests that an empty semilinear set contains no values.
testEmptySS :: Bool
testEmptySS = null $ toListSS emptySS

-- | Tests that the complement of an empty semilinear set contains all numbers.
testEmptyComplement :: Bool
testEmptyComplement
  = isAllSS (complementSS emptySS)
  && isEmptySS (complementSS allSS)

-- | Tests that taking the complement of a semilinear set twice results in the
-- same set.
testDoubleComplement :: SS -> Bool
testDoubleComplement ss = listEquivalent ss $ (complementSS . complementSS) ss

-- | Tests that intersecting a semilinear set with its complement results in the
-- empty set.
testIntersectWithComplement :: SS -> Bool
testIntersectWithComplement ss
  = isEmptySS $ intersectSS ss (complementSS ss)

-- | Tests that unioning a semilinear set with its complement contains all
-- numbers.
testUnionWithComplement :: SS -> Bool
testUnionWithComplement ss = isAllSS $ unionSS ss (complementSS ss)

-- | Tests that the union of two sets is a superset of both.
testUnionIsSuperset :: SS -> SS -> Bool
testUnionIsSuperset ssa ssb = all (isSubsetOf ssu) [ssa, ssb]
  where ssu = unionSS ssa ssb

-- | Tests that the intersection of two sets is a subset of both.
testIntersectIsSubset :: SS -> SS -> Bool
testIntersectIsSubset ssa ssb = all (isSupersetOf ssi) [ssa, ssb]
  where ssi = intersectSS ssa ssb

-- | Tests that all sets are a superset of the empty set.
testEmptyIsAlwaysSubset :: SS -> Bool
testEmptyIsAlwaysSubset = isSupersetOf emptySS

-- | Tests that the set of all numbers is a superset of all sets.
testAllIsAlwaysSuperset :: SS -> Bool
testAllIsAlwaysSuperset = isSubsetOf allSS

-- | Tests that the union of all equivalence classes for a given modulus/period
-- covers all numbers.
testSamePeriodAllOffsets :: Int -> Bool
testSamePeriodAllOffsets n = coversNats offsets
  where
    p = abs n + 1
    offsets = SS $ LS (0, p) : [LS (x, p) | x <- [1..p-1]]

-- Run Tests -------------------------------------------------------------------

tests :: [Property]
tests =
  [ once testEmptySS
  , once testEmptyComplement
  , withMaxSuccess   10 testDoubleComplement
  , withMaxSuccess  500 testIntersectWithComplement
  , withMaxSuccess  500 testUnionWithComplement
  , withMaxSuccess   50 testUnionIsSuperset
  , withMaxSuccess  500 testIntersectIsSubset
  , withMaxSuccess  500 testEmptyIsAlwaysSubset
  , withMaxSuccess  500 testAllIsAlwaysSuperset
  , withMaxSuccess   50 testSamePeriodAllOffsets
  ]

runTests :: IO ()
runTests = do
  results <- mapM quickCheckResult tests
  unless (all isSuccess results) $ fail "Not all tests passed"

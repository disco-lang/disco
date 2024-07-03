module TestPatternChecker (runTests) where

import Control.Monad (unless)
import PatternChecker
import Test.QuickCheck

-- Property Tests --------------------------------------------------------------

-- | Tests that the patterns `2n` and `2n+1` cover all natural numbers.
testEvenOddN :: Bool
testEvenOddN = null $ missingNats [evens, odds]
  where
    evens = mkNatPattern 0 2
    odds  = mkNatPattern 1 2

-- | Tests that the patterns `2x` and `2x+1` cover all the integers.
testEvenOddZ :: Bool
testEvenOddZ = null $ missingInts [evens, odds]
  where
    evens = mkIntPattern 0 2
    odds  = mkIntPattern 1 2

-- | Tests that `allNatsPattern` satisfies `coversNats`.
testNatsCover :: Bool
testNatsCover = coversNats [allNatsPattern]

-- | Tests that `allIntsPattern` satisfies `coversInts`.
testIntsCover :: Bool
testIntsCover = coversInts [allIntsPattern]

-- | Tests that an `n+k` pattern misses exactly the first k natural numbers.
testMissingN :: Int -> Bool
testMissingN x = missingNats [mkNatPattern n 1] == [0..n-1]
  where n = abs x

-- | Tests that no single natural number pattern can cover all integers.
testIntsNeedMultipleNatPatterns :: Int -> Int -> Bool
testIntsNeedMultipleNatPatterns x p = not $ coversInts [mkNatPattern x p]

-- | Tests that all integer patterns with period 1 cover all integers.
testWildcardIntPattern :: Int -> Bool
testWildcardIntPattern x = coversInts [mkIntPattern x 1]

-- | Tests that constant patterns contain the number they represent.
testConstantPatternContains :: Int -> Bool
testConstantPatternContains x = elemPattern x $ mkConstPattern x

-- | Tests that constant patterns do not contain any other number.
testConstantContainsOne :: Int -> Int -> Bool
testConstantContainsOne x y = (x == y) || not (elemPattern x (mkConstPattern y))

-- Run Tests -------------------------------------------------------------------

tests :: [Property]
tests =
  [ once testEvenOddN
  , once testEvenOddZ
  , once testNatsCover
  , once testIntsCover
  , withMaxSuccess 500 testMissingN
  , withMaxSuccess 500 testIntsNeedMultipleNatPatterns
  , withMaxSuccess 500 testWildcardIntPattern
  , withMaxSuccess 500 testConstantPatternContains
  , withMaxSuccess 500 testConstantContainsOne
  ]

runTests :: IO ()
runTests = do
  results <- mapM quickCheckResult tests
  unless (all isSuccess results) $ fail "Not all tests passed"

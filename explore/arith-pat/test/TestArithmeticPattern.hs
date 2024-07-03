module TestArithmeticPattern (runTests) where

import Control.Monad (unless)
import qualified ArithmeticPattern as P
import Test.QuickCheck

-- Property Tests --------------------------------------------------------------

-- | Tests that the patterns `2n` and `2n+1` cover all natural numbers.
testEvenOddN :: Bool
testEvenOddN = P.equal P.nats $ P.union evens odds
  where
    evens = P.mkNatPattern 0 2
    odds  = P.mkNatPattern 1 2

-- | Tests that the patterns `2x` and `2x+1` cover all the integers.
testEvenOddZ :: Bool
testEvenOddZ = P.equal P.ints $ P.union evens odds
  where
    evens = P.mkIntPattern 0 2
    odds  = P.mkIntPattern 1 2

-- | Tests that an `n+k` pattern misses exactly the first k natural numbers.
testMissingN :: Int -> Bool
testMissingN x = (== [0..k-1]) . P.toList . P.subtract P.nats $ nPlusK
  where
    k = abs x
    nPlusK = P.mkNatPattern k 1

-- | Tests that no single natural number pattern can cover all integers.
testIntsNeedMultipleNatPatterns :: Int -> Int -> Bool
testIntsNeedMultipleNatPatterns x p
  = not . null . P.toList . P.subtract P.ints $ P.mkNatPattern x p

-- | Tests that all integer patterns with period 1 cover all integers.
testWildcardIntPattern :: Int -> Bool
testWildcardIntPattern x = P.equal P.ints $ P.mkIntPattern x 1

-- | Tests that constant patterns contain the number they represent.
testConstantPatternContains :: Int -> Bool
testConstantPatternContains x = P.elem x $ P.mkConstPattern x

-- | Tests that constant patterns do not contain any other number.
testConstantContainsOne :: Int -> Int -> Bool
testConstantContainsOne x y = (x == y) || not (P.elem x (P.mkConstPattern y))

-- Run Tests -------------------------------------------------------------------

tests :: [Property]
tests =
  [ once testEvenOddN
  , once testEvenOddZ
  , withMaxSuccess 200 testMissingN
  , withMaxSuccess 200 testIntsNeedMultipleNatPatterns
  , withMaxSuccess 200 testWildcardIntPattern
  , withMaxSuccess 200 testConstantPatternContains
  , withMaxSuccess 200 testConstantContainsOne
  ]

runTests :: IO ()
runTests = do
  results <- mapM quickCheckResult tests
  unless (all isSuccess results) $ fail "Not all tests passed"

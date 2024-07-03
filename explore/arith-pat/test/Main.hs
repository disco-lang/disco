module Main (main) where

import qualified TestSemilinearSet
import qualified TestPatternChecker

main :: IO ()
main = do
  putStrLn "Running tests for SemilinearSet..."
  TestSemilinearSet.runTests
  putStrLn "Running tests for PatternChecker..."
  TestPatternChecker.runTests

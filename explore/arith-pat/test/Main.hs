module Main (main) where

import qualified TestSemilinearSet
import qualified TestArithmeticPattern

main :: IO ()
main = do
  putStrLn "Running tests for SemilinearSet..."
  TestSemilinearSet.runTests
  putStrLn "Running tests for ArithmeticPattern..."
  TestArithmeticPattern.runTests

module Main where

import           Control.Monad     (filterM)
import           System.Directory  (doesDirectoryExist, getDirectoryContents)
import           System.FilePath   ((</>))
import           System.Process    (system)

import           Test.Tasty
import           Test.Tasty.Golden

main :: IO ()
main = do
  testDirs <- getDirectoryContents "test" >>= filterM (doesDirectoryExist . ("test"</>))
  let testDirs' = filter (\f -> f /= "." && f /= "..") testDirs
  let testTree = testGroup "disco" $ map mkGolden testDirs'
  defaultMain testTree

mkGolden :: FilePath -> TestTree
mkGolden relDir =
  goldenVsFile
    relDir
    (dir </> "expected")
    (dir </> "output")
    (system ("disco -f " ++ (dir </> "input") ++ " > " ++ (dir </> "output")) >> return ())
  where
    dir = "test" </> relDir

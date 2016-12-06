module Main where

import           Control.Monad     (filterM)
import           System.Directory  (doesDirectoryExist, listDirectory)
import           System.FilePath   ((</>))
import           System.Process    (system)

import           Test.Tasty
import           Test.Tasty.Golden

main :: IO ()
main = do
  testDirs <- listDirectory "test" >>= filterM (doesDirectoryExist . ("test"</>))
  let testTree = testGroup "disco" $ map mkGolden testDirs
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

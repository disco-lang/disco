module Main where

import           Control.Monad     (filterM)
import           Data.Char         (isDigit)
import           Data.Function     (on)
import           Data.List         (groupBy, sort)
import           System.Directory  (doesDirectoryExist, getDirectoryContents)
import           System.FilePath   (isPathSeparator, (</>))
import           System.Process    (system)

import           Test.Tasty
import           Test.Tasty.Golden

main :: IO ()
main = do
  testDirs <- getDirectoryContents "test" >>= filterM (doesDirectoryExist . ("test"</>))
  let testDirs'
        = groupBy ((==) `on` extractGroup)
        . sort
        . filter (\f -> f /= "." && f /= "..")
        $ testDirs
  let testTree = testGroup "disco" $ map mkGroup testDirs'
  defaultMain testTree
  where
    mkGroup ds@(d:_) = testGroup (extractGroup d) $ map mkGolden ds

extractGroup :: FilePath -> String
extractGroup = reverse . dropWhile isDigit . takeWhile (not . isPathSeparator) . reverse

extractNum :: FilePath -> String
extractNum = reverse . takeWhile isDigit . reverse

mkGolden :: FilePath -> TestTree
mkGolden relDir =
  goldenVsFile
    (extractNum relDir)
    (dir </> "expected")
    (dir </> "output")
    (system ("disco -f " ++ (dir </> "input") ++ " > " ++ (dir </> "output")) >> return ())
  where
    dir = "test" </> relDir

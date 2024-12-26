module Main where

import Control.Monad (filterM, void)
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import System.Directory (
  doesFileExist,
  getDirectoryContents,
 )
import System.FilePath (isPathSeparator, (</>))
import System.IO (hGetContents)
import System.Process (
  StdStream (CreatePipe),
  createProcess,
  shell,
  std_out,
  system,
 )
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Text.Printf

main :: IO ()
main = do
  testDirs <-
    getDirectoryContents "test"
      >>= filterM (doesFileExist . (\d -> "test" </> d </> "input"))
  let testDirs' =
        NE.groupBy ((==) `on` extractGroup)
          . sort
          . filter (\f -> f /= "." && f /= "..")
          $ testDirs
  let testTree = testGroup "disco" $ map mkGroup testDirs'
  defaultMain testTree
 where
  mkGroup ds = testGroup (extractGroup (NE.head ds)) $ map mkGolden (NE.toList ds)

extractGroup :: FilePath -> String
extractGroup = takeWhile (/= '-')

extractName :: FilePath -> String
extractName = takeWhile (not . isPathSeparator) . drop 1 . dropWhile (/= '-')

mkGolden :: FilePath -> TestTree
mkGolden relDir =
  goldenVsFileWithDiff
    (extractName relDir)
    (dir </> "expected")
    (dir </> "output")
    (void $ system ("disco -f " ++ (dir </> "input") ++ " > " ++ (dir </> "output")))
 where
  dir = "test" </> relDir

-- | A variant of goldenVsFile that prints the result of @diff@ if
--   the files are different, so we don't have to manually call @diff@
--   every time there is a test failure.
goldenVsFileWithDiff ::
  -- | test name
  TestName ->
  -- | path to the «golden» file (the file that contains correct output)
  FilePath ->
  -- | path to the output file
  FilePath ->
  -- | action that creates the output file
  IO () ->
  -- | the test verifies that the output file contents is the same as the golden file contents
  TestTree
goldenVsFileWithDiff name ref new act =
  goldenTest
    name
    (BS.readFile ref)
    (act >> BS.readFile new)
    cmp
    upd
 where
  cmp = cmpWithDiff ref new
  upd = BS.writeFile ref

cmpWithDiff :: Eq a => FilePath -> FilePath -> a -> a -> IO (Maybe String)
cmpWithDiff f1 f2 x y = do
  if x == y
    then return Nothing
    else do
      (_, Just hout, _, _) <-
        createProcess (shell $ printf "diff %s %s" f1 f2) {std_out = CreatePipe}
      diffStr <- hGetContents hout
      return $
        Just $
          unlines
            [ printf "Files '%s' and '%s' differ:" f1 f2
            , diffStr
            ]

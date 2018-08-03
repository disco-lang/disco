module Main where

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Control.Monad

exampleDirs :: [FilePath]
exampleDirs = ["example", "docs/tutorial/example", "lib"]

main :: IO ()
main = mapM_ checkDir exampleDirs

checkDir :: FilePath -> IO ()
checkDir dir = do
  examples <- filter ((== ".disco") . takeExtension) <$> getDirectoryContents dir
  res <- and <$> mapM (checkExample . (dir </>)) examples
  when (not res) $ exitFailure

checkExample :: FilePath -> IO Bool
checkExample exampleFile = do
  ex <- system ("disco --check " ++ exampleFile)
  return (ex == ExitSuccess)

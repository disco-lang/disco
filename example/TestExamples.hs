module Main where

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Control.Monad

main :: IO ()
main = do
  examples <- filter ((== ".disco") . takeExtension) <$> getDirectoryContents "example"
  res <- and <$> mapM checkExample examples
  when (not res) $ exitFailure

checkExample :: FilePath -> IO Bool
checkExample exampleFile = do
  ex <- system ("disco --check " ++ ("example" </> exampleFile))
  return (ex == ExitSuccess)

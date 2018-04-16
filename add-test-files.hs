#!/usr/bin/env stack
-- stack runghc

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.IO
import System.Directory
import System.Process (readProcess)

testFiles :: IO [FilePath]
testFiles = lines <$> readProcess "git" ["ls-files", "test"] ""

main :: IO ()
main = do
  h <- openFile "disco.cabal" ReadMode
  (tmpName, tmpH) <- openTempFile "." "disco.cabal"
  ls <- lines <$> hGetContents h
  let (pre, (hdr:rest)) = break isTestDelim ls
      (_, post) = break isTestDelim rest

  fs <- testFiles
  hPutStr tmpH $ unlines (pre ++ [hdr] ++ map ((takeWhile isSpace hdr)++) fs ++ post)
  hClose h
  hClose tmpH

  removeFile "disco.cabal"
  renameFile tmpName "disco.cabal"

isTestDelim :: String -> Bool
isTestDelim = ("--- TEST" `isPrefixOf`) . dropWhile isSpace

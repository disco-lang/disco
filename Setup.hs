import           Distribution.PackageDescription
import           Distribution.Simple

import           System.Process                  (readProcess)

main = defaultMainWithHooks $ simpleUserHooks
  { sDistHook = discoSDist (sDistHook simpleUserHooks) }

-- Add all the files in test/ to extra-src-files.  It would be really
-- tedious to add these in the .cabal file, since cabal doesn't allow
-- recursive wildcards.
discoSDist sdist pkgDesc bi hooks flags = do
  pkgDesc' <- addTestFiles pkgDesc
  sdist pkgDesc' bi hooks flags
  where
    addTestFiles :: PackageDescription -> IO PackageDescription
    addTestFiles pkgDesc = do
      files <- testFiles
      return $ pkgDesc { extraSrcFiles = extraSrcFiles pkgDesc ++ files }
    testFiles :: IO [FilePath]
    testFiles = lines <$> readProcess "git" ["ls-files", "test"] ""

module Main (main, pFn) where

import qualified Data.Text.IO as TIO
import Text.Megaparsec (eof, many, runParser, errorBundlePretty)
import Parse

parseFile :: String -> IO [FunctionDef]
parseFile file = do
  let pf = sc *> many pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

p :: String -> IO ()
p x = parseFile x >>= print

main :: IO ()
main = p "test/test.disc"

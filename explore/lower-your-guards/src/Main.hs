module Main (main, pFn) where

import qualified Data.Text.IO as TIO
import Text.Megaparsec (eof, many, runParser, errorBundlePretty)
import Parse
import GuardTree
import Data.List.NonEmpty (fromList)
import Data.Text (Text)

parseFile :: String -> IO [FunctionDef]
parseFile file = do
  let pf = sc *> many pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

main :: IO ()
main = pfg "test/test.disc" >>= print

pfg :: String -> IO [(Text, Gdt)]
pfg file = do
  defs <- parseFile file
  return $ map (\(FunctionDef (FunctionDecl name _ _) clauses) -> (name, desugarClauses $ fromList clauses)) defs

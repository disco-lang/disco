module Main (main, pfg, pdu) where

import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GuardTree as G
import Parse
import Text.Megaparsec (eof, errorBundlePretty, many, runParser)
import qualified Uncovered as U
import Text.Pretty.Simple (pPrint)

parseFile :: String -> IO [FunctionDef]
parseFile file = do
  let pf = sc *> many pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

main :: IO ()
main = pdu "test/test.disc" >>= pPrint

pfg :: String -> IO [(Text, G.Gdt)]
pfg file = do
  defs <- parseFile file
  return $ map (\(FunctionDef (FunctionDecl name _ _) clauses) -> (name, G.desugarClauses $ fromList clauses)) defs

pdu :: String -> IO [(Text, U.RefinementType)]
pdu file = do
  defs <- parseFile file
  return $
    map
      ( \(FunctionDef (FunctionDecl name tIn _) clauses) ->
          (name, U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses)
      )
      defs

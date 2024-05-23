module Main (main, pfg, pdu) where

import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GuardTree as G
import Parse
import Text.Megaparsec (eof, errorBundlePretty, many, runParser)
import qualified Uncovered as U
import Text.Pretty.Simple (pPrint)
import qualified Inhabitants as I
import qualified Data.Set as S
import qualified Parse as P

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

pdui :: String -> IO [(Text, S.Set [P.Pattern])]
pdui file = do
  defs <- parseFile file
  return $
    map
      ( \(FunctionDef (FunctionDecl name tIn _) clauses) ->
          (name, I.genInhabitants $ U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses)
      )
      defs

pdun :: String -> IO [(Text, S.Set I.NormRefType)]
pdun file = do
  defs <- parseFile file
  return $
    map
      ( \(FunctionDef (FunctionDecl name tIn _) clauses) ->
          let (context, formula) = U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses in
          (name, I.normalize (Just (context, [])) formula)
      )
      defs

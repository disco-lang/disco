module Main (main, pfg, pdu) where

import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GuardTree as G
import Text.Megaparsec (eof, errorBundlePretty, many, runParser)
import qualified Uncovered as U
import Text.Pretty.Simple (pPrint)
import qualified Inhabitants as I
import qualified Data.Set as S
import qualified Parse as P
import qualified Types as Ty
import qualified Data.Text as T

parseFile :: String -> IO [P.FunctionDef]
parseFile file = do
  let pf = P.sc *> many P.pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

main :: IO ()
main = pdu "test/test.disc" >>= pPrint

pfg :: String -> IO [(Text, G.Gdt)]
pfg file = do
  defs <- parseFile file
  return $ map (\(P.FunctionDef (P.FunctionDecl name _ _) clauses) -> (name, G.desugarClauses $ fromList clauses)) defs

pdu :: String -> IO [(Text, U.RefinementType)]
pdu file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses)
      )
      defs

pdui :: String -> IO [(Text, S.Set [P.Pattern])]
pdui file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, I.genInhabitants $ U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses)
      )
      defs

inhabNice :: String -> IO [(Text, S.Set [String])]
inhabNice file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, S.map (map nicePattern) $ I.genInhabitants $ U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses)
      )
      defs

nicePattern :: P.Pattern -> String
nicePattern (P.PMatch k ps) = (T.unpack $ Ty.dcName k) ++ (concatMap (" "++) (map nicePattern ps))
nicePattern P.PWild = "_"
nicePattern (P.PLit i) = show i
nicePattern (P.PVar (P.Var x)) = T.unpack x

pdun :: String -> IO [(Text, S.Set I.NormRefType)]
pdun file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          let (context, formula) = U.uncovered ([("x_1", tIn)], U.T) $ G.desugarClauses $ fromList clauses in
          (name, I.normalize (Just (context, [])) formula)
      )
      defs

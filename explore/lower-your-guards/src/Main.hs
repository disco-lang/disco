module Main (main, evalGdt, pfg, pdu, pdun, pdui, inhabNice) where

import Control.Monad.State
import Data.List.NonEmpty (fromList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Fresh as F
import qualified GuardTree as G
import qualified Inhabitants as I
import qualified Parse as P
import Text.Megaparsec (eof, errorBundlePretty, many, runParser)
import Text.Pretty.Simple (pPrint)
import qualified Types as Ty
import qualified Uncovered as U
import qualified Data.List.NonEmpty as NE

parseFile :: String -> IO [P.FunctionDef]
parseFile file = do
  let pf = P.sc *> many P.pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

main :: IO ()
main = pdui "test/test.disc" >>= pPrint

desGdt :: [P.Clause] -> F.Fresh G.Gdt
desGdt clauses = do
  x1 <- F.fresh (Just "x_1")
  G.desugarClauses [x1] . fromList $ clauses

evalGdt :: [P.Clause] -> G.Gdt
evalGdt clauses = evalState (desGdt clauses) F.blank

runGdt :: [P.Clause] -> (G.Gdt, NE.NonEmpty F.Frame)
runGdt clauses = runState (desGdt clauses) F.blank

uncov :: [P.Clause] -> Ty.Type -> F.Fresh U.RefinementType
uncov clauses tIn = do
  x1 <- F.fresh (Just "x_1")
  gdt <- G.desugarClauses [x1] . fromList $ clauses
  let argsCtx = [(x1, tIn)]
  return $ U.uncovered (argsCtx, U.T) gdt

evalUncov :: [P.Clause] -> Ty.Type -> U.RefinementType
evalUncov clauses tIn = evalState (uncov clauses tIn) F.blank

inhab :: [P.Clause] -> Ty.Type -> F.Fresh [[P.Pattern]]
inhab clauses tIn = do
  u <- uncov clauses tIn
  I.genInhabitants u

evalInhab :: [P.Clause] -> Ty.Type -> [[P.Pattern]]
evalInhab clauses tIn = evalState (inhab clauses tIn) F.blank

norm :: [P.Clause] -> Ty.Type -> F.Fresh (S.Set I.NormRefType)
norm clauses tIn = do
  (context, formula) <- uncov clauses tIn
  I.normalize (context, []) formula

evalNorm :: [P.Clause] -> Ty.Type -> S.Set I.NormRefType
evalNorm clauses tIn = evalState (norm clauses tIn) F.blank

pfg :: String -> IO [(Text, (G.Gdt, NE.NonEmpty F.Frame))]
pfg file = do
  defs <- parseFile file
  return $ map (\(P.FunctionDef (P.FunctionDecl name _ _) clauses) -> (name, runGdt clauses)) defs

pdu :: String -> IO [(Text, U.RefinementType)]
pdu file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, evalUncov clauses tIn)
      )
      defs

pdui :: String -> IO [(Text, [[P.Pattern]])]
pdui file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, evalInhab clauses tIn)
      )
      defs

inhabNice :: String -> IO ()
inhabNice file = do
  defs <- parseFile file
  pPrint $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, map (map nicePattern) $ evalInhab clauses tIn)
      )
      defs

nicePattern :: P.Pattern -> String
nicePattern (P.PMatch k ps) = T.unpack (Ty.dcName k) ++ concatMap ((" " ++) . nicePattern) ps
nicePattern P.PWild = "_"
nicePattern (P.PLit i) = show i
nicePattern (P.PVar x) = T.unpack x

pdun :: String -> IO [(Text, S.Set I.NormRefType)]
pdun file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, evalNorm clauses tIn)
      )
      defs

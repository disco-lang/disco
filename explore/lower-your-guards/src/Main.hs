module Main (main, evalGdt, pfg, pdu, pdun, pduip, inhabNicePos) where

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
main = inhabNicePos "test/test.disc"

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

inhabPos :: [P.Clause] -> Ty.Type -> F.Fresh [I.InhabPat]
inhabPos clauses tIn = do
  u <- uncov clauses tIn
  s <- get
  return $ I.genInhabPos s u

evalInhabPos :: [P.Clause] -> Ty.Type -> [I.InhabPat]
evalInhabPos clauses tIn = evalState (inhabPos clauses tIn) F.blank

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

pduip :: String -> IO [(Text, [I.InhabPat])]
pduip file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, evalInhabPos clauses tIn)
      )
      defs

inhabNicePos :: String -> IO ()
inhabNicePos file = do
  defs <- parseFile file
  pPrint $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, map niceInhabPattern $ evalInhabPos clauses tIn)
      )
      defs

nicePattern :: P.Pattern -> String
nicePattern (P.PMatch k ps) = T.unpack (Ty.dcName k) ++ concatMap ((" " ++) . nicePattern) ps
nicePattern P.PWild = "_"
nicePattern (P.PLit i) = show i
nicePattern (P.PVar x) = T.unpack x

niceInhabPattern :: I.InhabPat -> String
niceInhabPattern (I.IPMatch k ps) = T.unpack (Ty.dcName k) ++ concatMap ((" " ++) . niceInhabPattern) ps
niceInhabPattern I.IPWild = "_"
niceInhabPattern (I.IPIntLit i) = show i
niceInhabPattern (I.IPNotIntLit i) = "(Not " ++ show i ++ ")"
niceInhabPattern (I.IPNotIntLits i) = "(Not " ++ show i ++ ")"
niceInhabPattern I.IPPlaceholderInt = "placehold"

pdun :: String -> IO [(Text, S.Set I.NormRefType)]
pdun file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, evalNorm clauses tIn)
      )
      defs

module Main (main, pUA, pFullUA, nicePattern, evalGdt, pfg, pdu, pda, pdun, pduip, inhabNice) where

import qualified Annotated as A
import Control.Monad.State
import Data.List (sort)
import Data.List.NonEmpty (fromList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Fresh as F
import qualified GuardTree as G
import qualified Inhabitants as I
import qualified Parse as P
import System.Directory (listDirectory)
import Text.Megaparsec (eof, errorBundlePretty, many, runParser)
import Text.Pretty.Simple (pPrint)
import qualified Types as Ty
import qualified UA
import qualified Uncovered as U
import qualified Possibilities as Poss

parseFile :: String -> IO [P.FunctionDef]
parseFile file = do
  let pf = P.sc *> many P.pFn <* eof
  contents <- TIO.readFile file
  case runParser pf file contents of
    Left e -> error $ errorBundlePretty e
    Right defs -> return defs

handleFunctions :: ([P.Clause] -> Ty.Type -> a) -> String -> IO [(Text, a)]
handleFunctions handler file = do
  defs <- parseFile file
  return $
    map
      ( \(P.FunctionDef (P.FunctionDecl name tIn _) clauses) ->
          (name, handler clauses tIn)
      )
      defs

main :: IO ()
main = do
  files <- listDirectory "./test"
  let fileNames = map ("test/" ++) . sort $ files
  sequence_ $ concatMap (\f -> [pPrint f, pFullUA f >>= pPrint]) fileNames

desGdt :: [P.Clause] -> F.Fresh G.Gdt
desGdt clauses = do
  x1 <- F.fresh (Just "x_1")
  G.desugarClauses [x1] . fromList $ clauses

evalGdt :: [P.Clause] -> G.Gdt
evalGdt clauses = evalState (desGdt clauses) F.blank

runGdt :: [P.Clause] -> (G.Gdt, NE.NonEmpty F.Frame)
runGdt clauses = runState (desGdt clauses) F.blank

uncov :: [P.Clause] -> Ty.Type -> F.Fresh (U.RefinementType, U.Context)
uncov clauses tIn = do
  x1 <- F.fresh (Just "x_1")
  gdt <- G.desugarClauses [x1] . fromList $ clauses
  let argsCtx = [(x1, tIn)]
  return (U.uncovered (argsCtx, U.Literal U.T) gdt, argsCtx)

evalUncov :: [P.Clause] -> Ty.Type -> U.RefinementType
evalUncov clauses tIn = fst $ evalState (uncov clauses tIn) F.blank

inhab :: [P.Clause] -> Ty.Type -> F.Fresh [I.InhabPat]
inhab clauses tIn = do
  (u, args) <- uncov clauses tIn
  Poss.getPossibilities <$> I.genInhab u args

evalInhab :: [P.Clause] -> Ty.Type -> [I.InhabPat]
evalInhab clauses tIn = evalState (inhab clauses tIn) F.blank

norm :: [P.Clause] -> Ty.Type -> F.Fresh (S.Set I.NormRefType)
norm clauses tIn = do
  ((context, formula), _args) <- uncov clauses tIn
  I.normalize (context, []) formula

evalNorm :: [P.Clause] -> Ty.Type -> S.Set I.NormRefType
evalNorm clauses tIn = evalState (norm clauses tIn) F.blank

pfg :: String -> IO [(Text, (G.Gdt, NE.NonEmpty F.Frame))]
pfg = handleFunctions (\c _ -> runGdt c)

pdu :: String -> IO [(Text, U.RefinementType)]
pdu = handleFunctions evalUncov

evalAnt :: [P.Clause] -> Ty.Type -> ([Int], [Int])
evalAnt clauses tIn = evalState (ant clauses tIn) F.blank

ant :: [P.Clause] -> Ty.Type -> F.Fresh ([Int], [Int])
ant clauses tIn = do
  x1 <- F.fresh (Just "x_1")
  gdt <- G.desugarClauses [x1] . fromList $ clauses
  let argsCtx = [(x1, tIn)]
  let a = A.annotated (argsCtx, U.Literal U.T) gdt
  I.accessableRedundant a argsCtx

pda :: String -> IO [(Text, ([Int], [Int]))]
pda = handleFunctions evalAnt

evalUA :: [P.Clause] -> Ty.Type -> ([I.NormRefType], UA.NAnt)
evalUA clauses tIn = flip evalState F.blank $ do
  x1 <- F.fresh (Just "x_1")
  gdt <- G.desugarClauses [x1] . fromList $ clauses
  let argsCtx = [(x1, tIn)]
  UA.ua [(argsCtx, [])] gdt

pUA :: String -> IO [(Text, ([I.NormRefType], UA.NAnt))]
pUA = handleFunctions evalUA

evalFullUA :: [P.Clause] -> Ty.Type -> ([String], [Int])
evalFullUA clauses tIn = flip evalState F.blank $ do
  x1 <- F.fresh (Just "x_1")
  gdt <- G.desugarClauses [x1] . fromList $ clauses
  let argsCtx = [(x1, tIn)]
  (nref, nant) <- UA.ua [(argsCtx, [])] gdt
  ipats <- I.genInhabNorm nref argsCtx
  redundant <- UA.redundantNorm nant argsCtx

  return (map niceInhabPattern (Poss.getPossibilities ipats), redundant)

pFullUA :: String -> IO [(Text, ([String], [Int]))]
pFullUA = handleFunctions evalFullUA

pduip :: String -> IO [(Text, [I.InhabPat])]
pduip = handleFunctions evalInhab

pdun :: String -> IO [(Text, S.Set I.NormRefType)]
pdun = handleFunctions evalNorm

inhabNice :: String -> IO [(Text, [String])]
inhabNice = handleFunctions (\clauses ty -> map niceInhabPattern $ evalInhab clauses ty)

nicePattern :: P.Pattern -> String
nicePattern (P.PMatch k ps) = T.unpack (Ty.dcName k) ++ concatMap ((" " ++) . nicePattern) ps
nicePattern P.PWild = "_"
nicePattern (P.PLit i) = show i
nicePattern (P.PVar x) = T.unpack x

niceInhabPattern :: I.InhabPat -> String
niceInhabPattern (I.IPIs k ps) = T.unpack (Ty.dcName k) ++ concatMap ((" " ++) . niceInhabPattern) ps
niceInhabPattern (I.IPNot []) = "_"
niceInhabPattern (I.IPNot nots) = "(Not " ++ show nots ++ ")"
-- niceInhabPattern I.IPWild = "_"
-- niceInhabPattern (I.IPIntLit i) = show i
-- niceInhabPattern (I.IPNotIntLits i) = "(Not " ++ show i ++ ")"

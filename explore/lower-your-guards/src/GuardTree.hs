{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module GuardTree where

import Control.Monad (replicateM, zipWithM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Fresh as F
import MatchInfo
import qualified Parse as P
import qualified Types as Ty

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: (TypedVar, Guard) -> Gdt -> Gdt
  deriving (Show, Eq)

data Guard where
  GMatch :: Ty.DataConstructor -> [TypedVar] -> Guard
  GWas :: TypedVar -> Guard
  deriving (Show, Eq)

enumerate :: NonEmpty a -> NonEmpty (Int, a)
enumerate = NE.zip (1 :| [2 ..])

desugarClauses :: [F.VarID] -> NonEmpty P.Clause -> F.Fresh Gdt
desugarClauses args clauses = do
  cl <- mapM (desugarClause args) (enumerate clauses)
  return $ foldr1 Branch cl

desugarClause :: [F.VarID] -> (Int, P.Clause) -> F.Fresh Gdt
desugarClause args (i, P.Clause pat typeIn _) = do
  let x1 = head args -- we only suport 1 arg for this toy lyg
  guards <- desugarMatch (x1, typeIn) pat
  return $ foldr Guarded (Grhs i) guards

desugarMatch :: TypedVar -> P.Pattern -> F.Fresh [(TypedVar, Guard)]
desugarMatch var@(_, ty) pat = do
  case pat of
    P.PWild -> return []
    P.PVar name -> do
      x <- F.fresh (Just name)
      let xTy = (x, ty)
      return [(xTy, GWas var)]
    P.PMatch dataCon subPats -> do
      ys <- replicateM (length subPats) (F.fresh Nothing)
      let typedYs = zip ys (Ty.dcTypes dataCon)
      guards <- zipWithM desugarMatch typedYs subPats
      return $ (var, GMatch dataCon typedYs) : concat guards

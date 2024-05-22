{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module GuardTree where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Parse as P

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

data Guard where
  Match :: DataCon -> [Var] -> Var -> Guard
  MatchLit :: Int -> Var -> Guard
  Let :: Var -> Var -> Guard
  deriving (Show, Eq)

type DataCon = Text

type Var = Text

enumerate :: NonEmpty a -> NonEmpty (Int, a)
enumerate = NE.zip (1 :| [2 ..])

desugarClauses :: NonEmpty P.Clause -> Gdt
desugarClauses clauses = foldr1 Branch $ NE.map desugarClause $ enumerate clauses

desugarClause :: (Int, P.Clause) -> Gdt
desugarClause (i, P.Clause pat _) = foldr Guarded (Grhs i) guards
  where
    guards = desugarMatch "x_1" pat

desugarMatch :: Text -> P.Pattern -> [Guard]
desugarMatch var pat = case pat of
  P.PWild -> []
  P.PLit i -> [MatchLit i var]
  P.PVar (P.Var a) -> [Let a var]
  P.PMatch dataCon subPats -> Match dataCon ys var : concat (zipWith desugarMatch ys subPats)
    where
      ys = getYs (length subPats)

getYs :: Int -> [Text]
getYs n = map (T.pack . ("y" ++) . show) [1 .. n]

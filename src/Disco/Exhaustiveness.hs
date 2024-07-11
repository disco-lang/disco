module Disco.Exhaustiveness where

import Control.Monad (replicateM, zipWithM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Disco.AST.Generic (Pattern_ (..), X_PVar)
import Disco.AST.Typed (APattern, ATerm)
import Disco.Effects.Fresh (Fresh, fresh)
import Disco.Types (Type)
import Polysemy
import Text.Show.Pretty (pPrint)
import Unbound.Generics.LocallyNameless (Name, s2n, unembed)

checkClauses :: (Members '[Fresh, Embed IO] r) => NonEmpty [APattern] -> Sem r ()
checkClauses pats = do
  cl <- zipWithM desugarClause [1 ..] (NonEmpty.toList pats)
  let gdt = foldr1 Branch cl

  embed $ pPrint gdt

-- TODO: should these really just be blank names?
newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

desugarClause :: (Member Fresh r) => Int -> [APattern] -> Sem r Gdt
desugarClause clauseIdx args = do
  names <- newNames (length args)
  guards <- zipWithM desugarMatch names args
  return $ foldr Guarded (Grhs clauseIdx) $ concat guards

desugarMatch :: (Member Fresh r) => Name ATerm -> APattern -> Sem r [Guard]
desugarMatch var pat = do
  case pat of
    (PWild_ _) -> return []
    (PVar_ ty name) -> do
      return $ [(var, Let name (unembed ty))]
    (PNat_ _ nat) -> return [(var, Nat nat)]
    (PUnit_ _) -> return [(var, DataCon Unit)]
    (PBool_ _ b) -> return [(var, DataCon $ Bool b)]
    (PTup_ _ subs) -> do
      names <- newNames (length subs)
      guards <- sequence $ zipWith desugarMatch names subs
      return $ (var, (DataCon (Tuple names))) : concat guards
    (PList_ _ subs) -> do
      names <- newNames (length subs)
      guards <- sequence $ zipWith desugarMatch names subs
      return $ (var, (DataCon (List names))) : concat guards
    (PCons_ _ subHead subTail) -> do
      nameHead <- newName
      nameTail <- newName
      guardsHead <- desugarMatch nameHead subHead
      guardsTail <- desugarMatch nameTail subTail
      return $ (var, (DataCon (Cons nameHead nameTail))) : guardsHead ++ guardsTail
    e -> return []

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

type Guard = (Name ATerm, GuardConstraint)

data GuardConstraint where
  DataCon :: DataCon -> GuardConstraint
  Nat :: Integer -> GuardConstraint
  Let :: Name ATerm -> Type -> GuardConstraint
  deriving (Show, Eq)

data DataCon where
  Unit :: DataCon
  Bool :: Bool -> DataCon
  Tuple :: [Name ATerm] -> DataCon
  List :: [Name ATerm] -> DataCon
  Cons :: Name ATerm -> Name ATerm -> DataCon
  deriving (Show, Eq)

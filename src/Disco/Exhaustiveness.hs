{-# LANGUAGE PatternSynonyms #-}

module Disco.Exhaustiveness where

import Control.Monad (forM, when, zipWithM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Disco.AST.Typed
  ( APattern,
    pattern APBool,
    pattern APCons,
    pattern APList,
    pattern APNat,
    pattern APTup,
    pattern APUnit,
    pattern APVar,
    pattern APWild,
  )
import Disco.Effects.Fresh (Fresh)
import qualified Disco.Exhaustiveness.Constraint as C
import qualified Disco.Exhaustiveness.Possibilities as Poss
import qualified Disco.Exhaustiveness.TypeInfo as TI
import qualified Disco.Types as Ty
import Polysemy
import Text.Show.Pretty (pPrint)

checkClauses :: (Members '[Fresh, Embed IO] r) => [Ty.Type] -> NonEmpty [APattern] -> Sem r ()
checkClauses types pats = do
  args <- TI.newVars (map TI.extractRelevant types)
  cl <- zipWithM (desugarClause args) [1 ..] (NonEmpty.toList pats)
  let gdt = foldr1 Branch cl

  let argsNref = (C.Context args, [])
  (normalizedNrefs, annotated) <- ua [argsNref] gdt

  inhab <- Poss.getPossibilities <$> findInhabitants normalizedNrefs args

  redundant <- findRedundant annotated args

  when False $ do
    embed $ putStrLn "=====DEBUG=============================="
    embed $ putStrLn "GDT:"
    embed $ pPrint gdt
    embed $ putStrLn "UNCOVERED:"
    embed $ pPrint inhab
    embed $ putStrLn "REDUNDANT:"
    embed $ pPrint redundant
    embed $ putStrLn "=====GUBED=============================="

  let jspace = foldr (\a b -> a ++ " " ++ b) ""

  when (not . null $ inhab) $ do
    embed $ putStrLn "Warning: You haven't matched against:"
    embed $ mapM_ (putStrLn . jspace . map prettyInhab) inhab
  -- embed $ mapM_ (putStrLn . show) inhab

  when (not . null $ redundant) $ do
    embed $ putStrLn "Warning: These clause numbers (1-indexed) are redundant"
    embed $ putStrLn $ show redundant

  return ()

desugarClause :: (Members '[Fresh, Embed IO] r) => [TI.TypedVar] -> Int -> [APattern] -> Sem r Gdt
desugarClause args clauseIdx argsPats = do
  guards <- zipWithM desugarMatch args argsPats
  return $ foldr Guarded (Grhs clauseIdx) $ concat guards

-- borrowed from `extra`
allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (x ==) xs

-- TODO: explain. this was a pain
desugarTuplePats :: [APattern] -> APattern
desugarTuplePats [] = error "Found empty tuple, what happened?"
desugarTuplePats [p] = p
desugarTuplePats (pfst : rest) = APTup (Ty.getType pfst Ty.:*: Ty.getType psnd) [pfst, psnd]
  where psnd = desugarTuplePats rest

desugarMatch :: (Members '[Fresh, Embed IO] r) => TI.TypedVar -> APattern -> Sem r [Guard]
desugarMatch var pat = do
  case pat of
    (APWild _) -> return []
    (APVar ty name) -> do
      return $ [(var, GHerebyBe $ TI.TypedVar (name, TI.extractRelevant ty))]
    (APNat _ nat) -> return [(var, GMatch (TI.natural nat) [])]
    (APUnit) -> return [(var, GMatch TI.unit [])]
    (APBool b) -> return [(var, GMatch (TI.bool b) [])]
    (APTup (ta Ty.:*: tb) [pfst, psnd]) -> do
      let tia = TI.extractRelevant ta
      let tib = TI.extractRelevant tb

      varFst <- TI.newVar tia
      varSnd <- TI.newVar tib

      guardsFst <- desugarMatch varFst pfst
      guardsSnd <- desugarMatch varSnd psnd

      let guardPair = (var, GMatch (TI.pair tia tib) [varFst, varSnd])
      return $ [guardPair] ++ guardsFst ++ guardsSnd
    (APTup ty [_, _]) -> error $ "Tuple type that wasn't a pair???: " ++ show ty
    (APTup _ sugary) -> desugarMatch var (desugarTuplePats sugary)
    (APList _ subs) -> do
      -- TODO: review, will this be a problem like tuples?
      let types = map (TI.extractRelevant . Ty.getType) subs
      when
        (not . allSame $ types)
        (embed . putStrLn $ "WARNING, mismatched types in list!: " ++ show types)
      vars <- TI.newVars types
      guards <- sequence $ zipWith desugarMatch vars subs
      return $ (var, (GMatch (TI.list types) vars)) : concat guards
    (APCons _ subHead subTail) -> do
      -- TODO: review, will this be a problem like tuples?
      let typeHead = (TI.extractRelevant . Ty.getType) subHead
      let typeTail = (TI.extractRelevant . Ty.getType) subTail
      varHead <- TI.newVar typeHead
      varTail <- TI.newVar typeTail
      guardsHead <- desugarMatch varHead subHead
      guardsTail <- desugarMatch varTail subTail
      let guardCons = (var, (GMatch (TI.cons typeHead typeTail) [varHead, varTail]))
      return $ [guardCons] ++ guardsHead ++ guardsTail
    -- TODO: consider the rest of the patterns
    -- (APAdd)
    e -> return []

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

type Guard = (TI.TypedVar, GuardConstraint)

data GuardConstraint where
  GMatch :: TI.DataCon -> [TI.TypedVar] -> GuardConstraint
  GHerebyBe :: TI.TypedVar -> GuardConstraint
  deriving (Show, Eq)

data Literal where
  T :: Literal
  F :: Literal
  LitCond :: (TI.TypedVar, LitCond) -> Literal
  deriving (Show, Eq, Ord)

data LitCond where
  LitMatch :: TI.DataCon -> [TI.TypedVar] -> LitCond
  LitNot :: TI.DataCon -> LitCond
  LitHerebyBe :: TI.TypedVar -> LitCond
  deriving (Show, Eq, Ord)

data Ant where
  AGrhs :: [C.NormRefType] -> Int -> Ant
  ABranch :: Ant -> Ant -> Ant
  deriving (Show)

ua :: (Member Fresh r) => [C.NormRefType] -> Gdt -> Sem r ([C.NormRefType], Ant)
ua nrefs gdt = case gdt of
  Grhs k -> return ([], AGrhs nrefs k)
  Branch t1 t2 -> do
    (n1, u1) <- ua nrefs t1
    (n2, u2) <- ua n1 t2
    return (n2, ABranch u1 u2)
  Guarded (x, GHerebyBe z) t -> do
    n <- addLitMulti nrefs $ LitCond (x, LitHerebyBe z)
    ua n t
  Guarded (x, (GMatch dc args)) t -> do
    n <- addLitMulti nrefs $ LitCond (x, LitMatch dc args)
    (n', u) <- ua n t
    n'' <- addLitMulti nrefs $ LitCond (x, LitNot dc)
    return (n'' ++ n', u)

addLitMulti :: (Members '[Fresh] r) => [C.NormRefType] -> Literal -> Sem r [C.NormRefType]
addLitMulti [] _ = return []
addLitMulti (n : ns) lit = do
  r <- runMaybeT $ addLiteral n lit
  case r of
    Nothing -> addLitMulti ns lit
    Just (ctx, cfs) -> do
      ns' <- addLitMulti ns lit
      return $ (ctx, cfs) : ns'

addLiteral :: (Members '[Fresh] r) => C.NormRefType -> Literal -> MaybeT (Sem r) C.NormRefType
addLiteral (context, constraints) flit = case flit of
  F -> MaybeT $ pure Nothing
  T -> return (context, constraints)
  LitCond (x, c) -> case c of
    LitHerebyBe z ->
      (context `C.addVars` [z], constraints) `C.addConstraint` (x, C.CHerebyBe z)
    LitMatch dc args ->
      (context `C.addVars` args, constraints) `C.addConstraint` (x, C.CMatch dc args)
    LitNot dc ->
      (context, constraints) `C.addConstraint` (x, C.CNot dc)

data InhabPat where
  IPIs :: TI.DataCon -> [InhabPat] -> InhabPat
  IPNot :: [TI.DataCon] -> InhabPat
  deriving (Show, Eq, Ord)

join :: [a] -> [a] -> [a] -> [a]
join j a b = a ++ j ++ b

joinComma :: [String] -> String
joinComma = foldr1 (join ", ")

joinSpace :: [String] -> String
joinSpace = foldr1 (join " ")

-- TODO: maybe fully print out tuples even if they have wildcars in the middle?
-- e.g. (1,_,_) instead of just (1,_)
prettyInhab :: InhabPat -> String
prettyInhab (IPNot []) = "_"
prettyInhab (IPNot nots) = "Not{" ++ joinComma (map dcToString nots) ++ "}"
prettyInhab (IPIs TI.DataCon {TI.dcIdent = TI.KPair, TI.dcTypes = _} [ifst, isnd]) =
  "(" ++ prettyInhab ifst ++ ", " ++ prettyInhab isnd ++ ")"
prettyInhab (IPIs dc []) = dcToString dc
prettyInhab (IPIs dc args) = dcToString dc ++ " " ++ joinSpace (map prettyInhab args)

dcToString :: TI.DataCon -> String
dcToString TI.DataCon {TI.dcIdent = TI.KNat n} = show n
dcToString TI.DataCon {TI.dcIdent = TI.KBool b} = show b
dcToString TI.DataCon {TI.dcIdent = TI.KUnit} = "Unit"
dcToString TI.DataCon {TI.dcIdent = TI.KPair} = "Pair?"
dcToString TI.DataCon {TI.dcIdent = _} = "AAAAA"

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
mkIPMatch :: TI.DataCon -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPIs k pats

findInhabitants :: (Members '[Fresh] r) => [C.NormRefType] -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findInhabitants nrefs args = do
  a <- forM nrefs (`findAllForNref` args)
  return $ Poss.anyOf a

findAllForNref :: (Member Fresh r) => C.NormRefType -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findAllForNref nref args = do
  argPats <- forM args (`findVarInhabitants` nref)
  return $ Poss.allCombinations argPats

findVarInhabitants :: (Members '[Fresh] r) => TI.TypedVar -> C.NormRefType -> Sem r (Poss.Possibilities InhabPat)
findVarInhabitants var nref@(_, cns) =
  case posMatch of
    Just (k, args) -> do
      argPossibilities <- findAllForNref nref args
      return (mkIPMatch k <$> argPossibilities)
    Nothing -> case nub negMatches of
      [] -> Poss.retSingle $ IPNot []
      neg ->
        case TI.tyDataCons . TI.getType $ var of
          Nothing -> Poss.retSingle $ IPNot neg
          Just dcs ->
            do
              let tryAddDc dc = do
                    vars <- TI.newVars (TI.dcTypes dc)
                    runMaybeT (nref `C.addConstraint` (var, C.CMatch dc vars))

              -- Try to add a positive constraint for each data constructor
              -- to the current nref
              -- If any of these additions succeed, save that nref,
              -- it now has positive information
              posNrefs <- catMaybes <$> forM dcs tryAddDc

              if null posNrefs
                then Poss.retSingle $ IPNot []
                else Poss.anyOf <$> forM posNrefs (findVarInhabitants var)
  where
    constraintsOnX = C.onVar var cns
    posMatch = C.posMatch constraintsOnX
    negMatches = C.negMatches constraintsOnX

findRedundant :: (Member Fresh r) => Ant -> [TI.TypedVar] -> Sem r [Int]
findRedundant ant args = case ant of
  AGrhs ref i -> do
    uninhabited <- Poss.none <$> findInhabitants ref args
    return [i | uninhabited]
  ABranch a1 a2 -> mappend <$> findRedundant a1 args <*> findRedundant a2 args

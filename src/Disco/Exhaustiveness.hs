{-# LANGUAGE PatternSynonyms #-}

module Disco.Exhaustiveness where

import Control.Monad (replicateM, when, zipWithM, forM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Disco.AST.Typed
  ( APattern,
    ATerm,
    pattern APBool,
    pattern APCons,
    pattern APList,
    pattern APNat,
    pattern APTup,
    pattern APUnit,
    pattern APVar,
    pattern APWild,
  )
import Disco.Effects.Fresh (Fresh, fresh)
import qualified Disco.Exhaustiveness.Possibilities as Poss
import qualified Disco.Exhaustiveness.TypeInfo as TI
import qualified Disco.Types as Ty
import Polysemy
import Text.Show.Pretty (pPrint)
import Unbound.Generics.LocallyNameless (Name, s2n)
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)

checkClauses :: (Members '[Fresh, Embed IO] r) => [Ty.Type] -> NonEmpty [APattern] -> Sem r ()
checkClauses types pats = do
  let relTypes = map TI.extractRelevant types
  cl <- zipWithM (desugarClause relTypes) [1 ..] (NonEmpty.toList pats)
  let gdt = foldr1 Branch cl

  embed $ pPrint gdt
  return ()

-- TODO: should these really just be blank names?
newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

newVars :: Member Fresh r => [b] -> Sem r [(Name ATerm, b)]
newVars types = do
  names <- newNames (length types)
  return $ zip names types

desugarClause :: (Members '[Fresh, Embed IO] r) => [TI.Type] -> Int -> [APattern] -> Sem r Gdt
desugarClause types clauseIdx args = do
  vars <- newVars types
  -- embed $ putStr "YO: "
  -- embed $ pPrint names
  guards <- zipWithM desugarMatch vars args
  return $ foldr Guarded (Grhs clauseIdx) $ concat guards

type TypedVar = (Name ATerm, TI.Type)

-- maybe `varsBound` will be useful later?

-- borrowed from `extra`
allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (x ==) xs

desugarMatch :: (Members '[Fresh, Embed IO] r) => TypedVar -> APattern -> Sem r [Guard]
desugarMatch var pat = do
  case pat of
    (APWild _) -> return []
    (APVar ty name) -> do
      return $ [(var, GHerebyBe (name, TI.extractRelevant ty))]
    (APNat _ nat) -> return [(var, GMatch (TI.natural nat) [])]
    (APUnit) -> return [(var, GMatch TI.unit [])]
    (APBool b) -> return [(var, GMatch (TI.bool b) [])]
    (APTup _ subs) -> do
      let types = map (TI.extractRelevant . Ty.getType) subs
      vars <- newVars types
      guards <- sequence $ zipWith desugarMatch vars subs
      return $ (var, (GMatch (TI.tuple types) vars)) : concat guards
    (APList _ subs) -> do
      let types = map (TI.extractRelevant . Ty.getType) subs
      when
        (not . allSame $ types)
        (embed . putStrLn $ "WARNING, mismatched types in list!: " ++ show types)
      vars <- newVars types
      guards <- sequence $ zipWith desugarMatch vars subs
      return $ (var, (GMatch (TI.list types) vars)) : concat guards
    (APCons _ subHead subTail) -> do
      embed $ putStr "Cons: "
      embed $ pPrint (subHead, subTail)
      nameHead <- newName
      nameTail <- newName
      let typeHead = (TI.extractRelevant . Ty.getType) subHead
      let typeTail = (TI.extractRelevant . Ty.getType) subTail
      let varHead = (nameHead, typeHead)
      let varTail = (nameTail, typeTail)
      guardsHead <- desugarMatch varHead subHead
      guardsTail <- desugarMatch varTail subTail
      return $ (var, (GMatch (TI.cons typeHead typeTail) [varHead, varTail])) : guardsHead ++ guardsTail
    -- (APAdd)
    e -> return []

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

type Guard = (TypedVar, GuardConstraint)

data GuardConstraint where
  GMatch :: TI.DataCon -> [TypedVar] -> GuardConstraint
  GHerebyBe :: TypedVar -> GuardConstraint
  deriving (Show, Eq)

data DataCon where
  KUnit :: DataCon
  Bool :: Bool -> DataCon
  Tuple :: [TypedVar] -> DataCon
  List :: [TypedVar] -> DataCon
  Cons :: TypedVar -> TypedVar -> DataCon
  deriving (Show, Eq, Ord)

data DataConName = NUnit | NBool Bool | NTuple | NList | NCons
  deriving (Show, Eq, Ord)

getName :: DataCon -> DataConName
getName KUnit = NUnit
getName (Bool b) = NBool b
getName (Tuple _) = NTuple
getName (List _) = NList
getName (Cons _ _) = NCons

data Literal where
  T :: Literal
  F :: Literal
  LitCond :: (TypedVar, LitCond) -> Literal
  deriving (Show, Eq, Ord)

data LitCond where
  LitMatch :: TI.DataCon -> [TypedVar] -> LitCond
  LitNot :: TI.DataCon -> LitCond
  LitHerebyBe :: TypedVar -> LitCond
  deriving (Show, Eq, Ord)

type NormRefType = (Context, [ConstraintFor])

type ConstraintFor = (TypedVar, Constraint)

type Context = [TypedVar]

data Constraint where
  CMatch :: TI.DataCon -> [TypedVar] -> Constraint
  CNot :: TI.DataCon -> Constraint
  CHerebyBe :: TypedVar -> Constraint
  deriving (Show, Eq, Ord)

data Ant where
  AGrhs :: [NormRefType] -> Int -> Ant
  ABranch :: Ant -> Ant -> Ant
  deriving (Show)

ua :: (Member Fresh r) => [NormRefType] -> Gdt -> Sem r ([NormRefType], Ant)
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

addLitMulti :: (Members '[Fresh] r) => [NormRefType] -> Literal -> Sem r [NormRefType]
addLitMulti [] _ = return []
addLitMulti (n : ns) lit = do
  r <- runMaybeT $ addLiteral n lit
  case r of
    Nothing -> addLitMulti ns lit
    Just (ctx, cfs) -> do
      ns' <- addLitMulti ns lit
      return $ (ctx, cfs) : ns'

redundantNorm :: (Member Fresh r) => Ant -> Context -> Sem r [Int]
redundantNorm ant args = case ant of
  AGrhs ref i -> do
    inhabited <- null <$> genInhab ref args
    return [i | inhabited]
  ABranch a1 a2 -> mappend <$> redundantNorm a1 args <*> redundantNorm a2 args

addLiteral :: (Members '[Fresh] r) => NormRefType -> Literal -> MaybeT (Sem r) NormRefType
addLiteral n@(context, constraints) flit = case flit of
  F -> MaybeT $ pure Nothing
  T -> return n
  LitCond (x, c) -> case c of
    LitHerebyBe z -> (n <> ([z], [])) `addConstraint` (x, CHerebyBe z)
    LitMatch dc args -> (n <> (args, [])) `addConstraint` (x, CMatch dc args)
    LitNot dc -> n `addConstraint` (x, CNot dc)

-- Tuple types names -> (context ++ zip names types, constraints) `addConstraint` (x, CIs $ Tuple names)
-- List :: [Name ATerm] -> DataCon
-- Cons :: Name ATerm -> Name ATerm -> DataCon

-- MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, MatchDataCon k ys)
-- NotDataCon k x -> n `addConstraint` (x, NotDataCon k)

data InhabPat where
  IPIs :: TI.DataCon -> [InhabPat] -> InhabPat
  IPNot :: [TI.DataCon] -> InhabPat
  deriving (Show, Eq, Ord)

genInhab :: (Members '[Fresh] r) => [NormRefType] -> Context -> Sem r (Poss.Possibilities InhabPat)
genInhab nrefs args = do
  n <- sequence [findVarInhabitants arg nref | nref <- nrefs, arg <- args]
  return $ Poss.anyOf n

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
mkIPMatch :: TI.DataCon -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPIs k pats

findVarInhabitants :: (Members '[Fresh] r) => TypedVar -> NormRefType -> Sem r (Poss.Possibilities InhabPat)
findVarInhabitants var nref@(_, cns) =
  case posMatch of
    Just (k, args) -> do
      argPats <- forM args (`findVarInhabitants` nref)
      let argPossibilities = Poss.allCombinations argPats

      return (mkIPMatch k <$> argPossibilities)
    Nothing -> case nub negMatch of
      [] -> Poss.retSingle $ IPNot []
      neg ->
        case TI.tyDataCons (snd var) of
          Nothing -> Poss.retSingle $ IPNot neg
          Just dcs ->
            do
              let tryAddDc dc = do
                    vars <- newVars (TI.dcTypes dc)
                    runMaybeT (nref `addConstraint` (var, CMatch dc vars))

              -- Try to add a positive constraint for each data constructor
              -- to the current nref
              -- If any of these additions succeed, save that nref,
              -- it now has positive information
              posNrefs <- catMaybes <$> forM dcs tryAddDc

              if null posNrefs
                then Poss.retSingle $ IPNot []
                else Poss.anyOf <$> forM posNrefs (findVarInhabitants var)
  where
    constraintsOnX = onVar var cns
    posMatch = listToMaybe $ mapMaybe (\case (CMatch k ys) -> Just (k, ys); _ -> Nothing) constraintsOnX
    negMatch = mapMaybe (\case (CNot k) -> Just k; _ -> Nothing) constraintsOnX

,,,TODO I recommend breaking things up into a "Constraint.hs" file, which imports a "Context.hs" file, in
        which we make Context a newtype instead of a alias for a pair, and expose the correct functions
        adding constraints shouldn't depend on how the context is implemented
        the only weird thing is going to be the substituting all the variables for the herebys

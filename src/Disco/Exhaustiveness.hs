{-# LANGUAGE PatternSynonyms #-}

module Disco.Exhaustiveness where

import Control.Monad (replicateM, zipWithM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Disco.AST.Generic (Pattern_ (..), X_PVar)
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
import qualified Disco.Exhaustiveness.TypeInfo as TI
import qualified Disco.Types as Ty
import Polysemy
import Text.Show.Pretty (pPrint)
import Unbound.Generics.LocallyNameless (Name, s2n, unembed)

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

desugarClause :: (Members '[Fresh, Embed IO] r) => [TI.Type] -> Int -> [APattern] -> Sem r Gdt
desugarClause types clauseIdx args = do
  names <- newNames (length args)
  -- embed $ putStr "YO: "
  -- embed $ pPrint names
  guards <- zipWithM desugarMatch (zip names types) args
  return $ foldr Guarded (Grhs clauseIdx) $ concat guards

type TypedVar = (Name ATerm, TI.Type)

-- maybe `varsBound` will be useful later?

desugarMatch :: (Members '[Fresh, Embed IO] r) => TypedVar -> APattern -> Sem r [Guard]
desugarMatch var pat = do
  case pat of
    (APWild _) -> return []
    (APVar ty name) -> do
      return $ [(var, MakeAlias (name, TI.extractRelevant ty))]
    (APNat _ nat) -> return [(var, Nat nat)]
    (APUnit) -> return [(var, DataCon KUnit)]
    (APBool b) -> return [(var, DataCon $ Bool b)]
    (APTup _ subs) -> do
      embed $ putStr "TUP: "
      embed $ pPrint subs
      let types = map (TI.extractRelevant . Ty.getType) subs
      names <- newNames (length subs)
      let vars = zip names types
      guards <- sequence $ zipWith desugarMatch vars subs
      return $ (var, (DataCon (Tuple vars))) : concat guards
    (APList _ subs) -> do
      embed $ putStr "List: "
      embed $ pPrint subs
      let types = map (TI.extractRelevant . Ty.getType) subs
      names <- newNames (length subs)
      let vars = zip names types
      guards <- sequence $ zipWith desugarMatch vars subs
      return $ (var, (DataCon (List vars))) : concat guards
    (APCons _ subHead subTail) -> do
      embed $ putStr "Cons: "
      embed $ pPrint (subHead, subTail)
      nameHead <- newName
      let varHead = (nameHead, (TI.extractRelevant . Ty.getType) subHead)
      nameTail <- newName
      let varTail = (nameTail, (TI.extractRelevant . Ty.getType) subTail)
      guardsHead <- desugarMatch varHead subHead
      guardsTail <- desugarMatch varTail subTail
      return $ (var, (DataCon (Cons varHead varTail))) : guardsHead ++ guardsTail
    -- (APAdd)
    e -> return []

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

type Guard = (TypedVar, GuardConstraint)

data GuardConstraint where
  DataCon :: DataCon -> GuardConstraint
  Nat :: Integer -> GuardConstraint
  MakeAlias :: TypedVar -> GuardConstraint
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
  Is :: DataCon -> LitCond
  Not :: DataConName -> LitCond
  IsInt :: Integer -> LitCond
  NotInt :: Integer -> LitCond
  LitMakeAlias :: TypedVar -> LitCond
  deriving (Show, Eq, Ord)

type NormRefType = (Context, [ConstraintFor])

type ConstraintFor = (Name ATerm, Constraint)

type Context = [TypedVar]

data Constraint where
  CIs :: DataCon -> Constraint
  CNot :: DataConName -> Constraint
  CIsInt :: Integer -> Constraint
  CNotInt :: Integer -> Constraint
  CMakeAlias :: TypedVar -> Constraint
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
  Guarded (x, MakeAlias z) t -> do
    n <- addLitMulti nrefs $ LitCond (x, LitMakeAlias z)
    ua n t
  Guarded (x, (DataCon dc)) t -> do
    n <- addLitMulti nrefs $ LitCond (x, Is dc)
    (n', u) <- ua n t
    n'' <- addLitMulti nrefs $ LitCond (x, Not $ getName dc)
    return (n'' ++ n', u)
  Guarded (x, (Nat i)) t -> do
    n <- addLitMulti nrefs $ LitCond (x, IsInt i)
    (n', u) <- ua n t
    n'' <- addLitMulti nrefs $ LitCond (x, NotInt i)
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
    inhabited <- null <$> genInhabNorm ref args
    return [i | inhabited]
  ABranch a1 a2 -> mappend <$> redundantNorm a1 args <*> redundantNorm a2 args

addLiteral :: (Members '[Fresh] r) => NormRefType -> Literal -> MaybeT (Sem r) NormRefType
addLiteral n@(context, constraints) flit = case flit of
  F -> MaybeT $ pure Nothing
  T -> return n
  LitCond (x, c) -> case c of
    LitMakeAlias z -> (n <> ([z], [])) `addConstraint` (x, CMakeAlias z)
    NotInt i -> n `addConstraint` (x, CNotInt i)
    IsInt i -> n `addConstraint` (x, CIsInt i)
    Not dc -> n `addConstraint` (x, CNot dc)
    Is dc -> case dc of
      KUnit -> n `addConstraint` (x, CIs dc)
      Bool b -> n `addConstraint` (x, CIs $ Bool b)

-- Tuple types names -> (context ++ zip names types, constraints) `addConstraint` (x, CIs $ Tuple names)
-- List :: [Name ATerm] -> DataCon
-- Cons :: Name ATerm -> Name ATerm -> DataCon

-- MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, MatchDataCon k ys)
-- NotDataCon k x -> n `addConstraint` (x, NotDataCon k)

{-
genInhabNorm :: Members '[Fresh] r => [NormRefType] -> Context -> Sem r (Poss.Possibilities InhabPat)
genInhabNorm nrefs args = do
  n <- sequence [findVarInhabitants arg nref | nref <- nrefs, arg <- args]
  return $ Poss.anyOf n

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
mkIPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (Ty.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPIs k pats

findVarInhabitants :: TypedVar -> NormRefType -> F.Fresh (Poss.Possibilities InhabPat)
findVarInhabitants var nref@(_, cns) =
  case posMatch of
    Just (k, args) -> do
      argPats <- forM args (`findVarInhabitants` nref)
      let argPossibilities = Poss.allCombinations argPats

      return (mkIPMatch k <$> argPossibilities)
    Nothing -> case nub negMatch of
      [] -> Poss.retSingle $ IPNot []
      neg ->
        case getDataCons var of
          Nothing -> Poss.retSingle $ IPNot neg
          Just dcs ->
            do
              let tryAddDc dc = do
                    newVars <- mkNewVars (Ty.dcTypes dc)
                    runMaybeT (nref `addConstraint` (var, MatchInfo $ Match dc newVars))

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
    posMatch = listToMaybe $ mapMaybe (\case MatchInfo (Match k ys) -> Just (k, ys); _ -> Nothing) constraintsOnX
    negMatch = mapMaybe (\case MatchInfo (Not k) -> Just k; _ -> Nothing) constraintsOnX

normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (`normalize` f2) n1
  return $ S.unions rest
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize nref (U.Literal fl) = maybe S.empty S.singleton <$> runMaybeT (nref `addLiteral` fl)
-}

addConstraint a b = undefined

genInhabNorm a b = undefined

genInhabNorm :: p1 -> p2 -> Sem r [Int]

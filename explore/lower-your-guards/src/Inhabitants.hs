{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import qualified Annotated as A
import Control.Applicative
import Control.Monad (foldM, forM, guard, replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (nub, partition)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Possibilities as Poss
import qualified Types as Ty
import qualified Uncovered as U

-- type VarMatchInfo = (F.VarID, MatchInfo)
-- data MatchInfo where
--   MatchI :: Ty.DataConstructor -> [F.VarID] -> MatchInfo
--   NotI :: [Ty.DataConstructor] -> MatchInfo

data InhabPat where
  IPIs :: Ty.DataConstructor -> [InhabPat] -> InhabPat
  IPNot :: [Ty.DataConstructor] -> InhabPat
  deriving (Show, Eq, Ord)

type NormRefType = (U.Context, [ConstraintFor])

type ConstraintFor = (F.VarID, Constraint)

data Constraint where
  Match :: Ty.DataConstructor -> [F.VarID] -> Constraint
  Not :: Ty.DataConstructor -> Constraint
  TermEquality :: F.VarID -> Constraint
  deriving (Show, Eq, Ord)

-- old, use UA
accessableRedundant :: A.Ant -> U.Context -> F.Fresh ([Int], [Int])
accessableRedundant ant args = case ant of
  A.Grhs ref i -> do
    nothing <- Poss.none <$> genInhab ref args
    return $
      if nothing
        then ([], [i])
        else ([i], [])
  A.Branch a1 a2 -> mappend <$> accessableRedundant a1 args <*> accessableRedundant a2 args

-- Resolves term equalities, finding the leftmost id for a variable
-- I believe I3 of section 3.4 allows us to
-- do a linear scan from right to left
lookupVar :: F.VarID -> [ConstraintFor] -> F.VarID
lookupVar x = foldr getNextId x
  where
    getNextId (x', TermEquality y) | x' == x = const y
    getNextId _ = id

alistLookup :: (Eq a) => a -> [(a, b)] -> [b]
alistLookup a = map snd . filter ((== a) . fst)

lookupType :: F.VarID -> U.Context -> Ty.Type
lookupType x ctx = fromMaybe (error errMsg) (listToMaybe $ alistLookup x ctx)
  where
    errMsg = "Var not found in context: " ++ show x ++ " " ++ show ctx

onVar :: F.VarID -> [ConstraintFor] -> [Constraint]
onVar x cs = alistLookup (lookupVar x cs) cs

genInhab :: U.RefinementType -> U.Context -> F.Fresh (Poss.Possibilities InhabPat)
genInhab (ctx, formula) args = do
  nrefs <- S.toList <$> normalize (ctx, []) formula
  genInhabNorm nrefs args

genInhabNorm :: [NormRefType] -> U.Context -> F.Fresh (Poss.Possibilities InhabPat)
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

findVarInhabitants :: (F.VarID, Ty.Type) -> NormRefType -> F.Fresh (Poss.Possibilities InhabPat)
findVarInhabitants var@(x, xType) nref@(_, cns) =
  case posMatch of
    Just (k, ys) -> do
      let args = zip ys (Ty.dcTypes k)

      argPats <- forM args (`findVarInhabitants` nref)
      let argPossibilities = Poss.allCombinations argPats

      return (mkIPMatch k <$> argPossibilities)
    Nothing -> case nub negMatch of
      [] -> Poss.retSingle $ IPNot []
      neg ->
        case Ty.dataCons xType of
          Nothing -> Poss.retSingle $ IPNot neg
          Just dcs ->
            do
              let tryAddDc dc = do
                    frs <- replicateM (length . Ty.dcTypes $ dc) (F.fresh Nothing)
                    runMaybeT (nref `addConstraint` (x, Match dc frs))

              -- Try to add a positive constraint for each data constructor
              -- to the current nref
              -- If any of these additions succeed, save that nref,
              -- it now has positive information
              posNrefs <- catMaybes <$> forM dcs tryAddDc

              if null posNrefs
                then Poss.retSingle $ IPNot []
                else Poss.anyOf <$> forM posNrefs (findVarInhabitants var)
  where
    constraintsOnX = onVar x cns
    posMatch = listToMaybe $ mapMaybe (\case Match k ys -> Just (k, ys); _ -> Nothing) constraintsOnX
    negMatch = mapMaybe (\case Not k -> Just k; _ -> Nothing) constraintsOnX

normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (`normalize` f2) n1
  return $ S.unions rest
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize nref (U.Literal fl) = maybe S.empty S.singleton <$> runMaybeT (nref `addLiteral` fl)

addLiteral :: NormRefType -> U.Literal -> MaybeT F.Fresh NormRefType
addLiteral n@(context, constraints) flit = case flit of
  U.F -> MaybeT $ pure Nothing
  U.T -> return n
  U.Let x xType y -> (context ++ [(x, xType)], constraints) `addConstraint` (x, TermEquality y)
  U.MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, Match k ys)
  U.NotDataCon k x -> n `addConstraint` (x, Not k)

addConstraints :: NormRefType -> [ConstraintFor] -> MaybeT F.Fresh NormRefType
addConstraints = foldM addConstraint

addConstraint :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraint nref@(_, cns) (x, c) = do
  breakIf $ any (conflictsWith c) (onVar x cns)
  addConstraintHelper nref (lookupVar x cns, c)

addConstraintHelper :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraintHelper nref@(ctx, cns) cf@(origX, c) = case c of
  --- Equation (10)
  Match k args -> do
    case getConstructorArgs k (onVar origX cns) of
      -- 10c -- TODO(colin): Still need to add type constraints!
      Just args' -> addConstraints nref (zipWith (\a b -> (a, TermEquality b)) args args')
      Nothing -> return added
  --- Equation (11)
  Not _ -> do
    inh <- lift (inhabited added origX (lookupType origX ctx))
    guard inh -- ensure that origX is still inhabited, as per I2
    return added
  -- Equation (14)
  TermEquality y -> do
    let origY = lookupVar y cns
    if origX == origY
      then return nref
      else do
        let (noX', withX') = partition ((/= origX) . fst) cns
        addConstraints (ctx, noX' ++ [cf]) (substituteVarIDs origY origX withX')
  where
    added = (ctx, cns ++ [cf])

-----
----- Helper functions for adding constraints:
-----

breakIf :: (Alternative f) => Bool -> f ()
breakIf = guard . not

-- Returns a predicate that returns true if another
-- constraint conflicts with the one given.
-- This alone is not sufficient to test
-- if a constraint can be added, but it
-- filters out the easy negatives early on
conflictsWith :: Constraint -> (Constraint -> Bool)
conflictsWith c = case c of
  Match k _ -> \case
    Match k' _ | k /= k' -> True -- 10a
    Not k' | k == k' -> True -- 10b
    _ -> False
  Not k -> \case
    Match k' _ | k == k' -> True -- 11a
    _ -> False
  TermEquality _ -> const False

-- Search for a MatchDataCon that is matching on k specifically
-- (there should be at most one, see I4 in section 3.4)
-- and if it exists, return the variable ids of its arguments
getConstructorArgs :: Ty.DataConstructor -> [Constraint] -> Maybe [F.VarID]
getConstructorArgs k cfs =
  listToMaybe $
    mapMaybe (\case Match k' v | k' == k -> Just v; _ -> Nothing) cfs

-- substituting y *for* x
-- ie replace the second with the first, replace x with y
substituteVarIDs :: F.VarID -> F.VarID -> [ConstraintFor] -> [ConstraintFor]
substituteVarIDs y x = map (\case (x', c) | x' == x -> (y, c); cf -> cf)

-- Deals with I2 form section 3.4
-- if a variable in the context has a resolvable type, there must be at least one constructor
-- which can be instantiated without contradiction of the refinement type
-- This function tests if this is true
-- NOTE: we may eventually have type constraints
-- and we would need to worry pulling them from nref here
inhabited :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inhabited n x tau = case Ty.dataCons tau of
  Nothing -> return True -- assume opaque types are inhabited
  Just constructors -> do
    or <$> mapM (instantiate n x) constructors

-- Attempts to "instantiate" a match of the dataconstructor k on x
-- If we can add the MatchDataCon constraint to the normalized refinement
-- type without contradiction (a Nothing value),
-- then x is inhabited by k and we return true
instantiate :: NormRefType -> F.VarID -> Ty.DataConstructor -> F.Fresh Bool
instantiate (ctx, cns) x k = do
  newVarIDs <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  let attempt = (ctx ++ newVars, cns) `addConstraint` (x, Match k newVarIDs)
  isJust <$> runMaybeT attempt

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import Control.Applicative
import Control.Monad (foldM, forM, guard, join, replicateM)
import Control.Monad.State (runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (nub, partition)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Types as Ty
import qualified Uncovered as U

type NormRefType = (U.Context, [ConstraintFor])

type ConstraintFor = (F.VarID, Constraint)

data Constraint where
  MatchDataCon :: Ty.DataConstructor -> [F.VarID] -> Constraint
  NotDataCon :: Ty.DataConstructor -> Constraint
  MatchIntLit :: Int -> Constraint
  NotIntLit :: Int -> Constraint
  TermEquality :: F.VarID -> Constraint
  deriving (Show, Eq, Ord)

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

data InhabPat where
  IPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
  IPWild :: InhabPat
  IPIntLit :: Int -> InhabPat
  IPNotIntLit :: Int -> InhabPat
  IPNotIntLits :: [Int] -> InhabPat
  IPPlaceholderInt :: InhabPat
  deriving (Show, Eq, Ord)

genInhab :: NE.NonEmpty F.Frame -> U.RefinementType -> [InhabPat]
genInhab frames (context, formula) = do
  let mNrefs = S.toList <$> normalize (context, []) formula
  let (nrefs, frames') = runState mNrefs frames
  nref <- nrefs
  join $ expandVars frames' nref context

expandVars :: NE.NonEmpty F.Frame -> NormRefType -> U.Context -> [[InhabPat]]
expandVars frame nset vars = do
  traverse (expandVar frame nset) vars

expandVar :: NE.NonEmpty F.Frame -> NormRefType -> (F.VarID, Ty.Type) -> [InhabPat]
expandVar frames nref@(_, cns) (x, xType) = case matchOnX of
  Nothing | xType == Ty.int -> case isIntX of
    Nothing -> case isNotIntX of
      [] -> return IPWild
      is -> return $ IPNotIntLits (nub is)
    Just i -> return $ IPIntLit i
  Nothing -> case cantMatchOnX of
    [] -> return IPWild
    _ ->
      do
        let (matchers, frames') =
              runState
                ( catMaybes
                    <$> forM
                      (Ty.dataCons xType)
                      ( \dc -> do
                          frs <- replicateM (length . Ty.dcTypes $ dc) (F.fresh Nothing)
                          runMaybeT (nref `addConstraint` (x, MatchDataCon dc frs))
                      )
                )
                frames
        if null matchers
          then return IPWild
          else do
            m <- matchers
            expandVar frames' m (x, xType)
  Just (k, ys) -> do
    l <- expandVars frames nref (zip ys (Ty.dcTypes k))
    return $ IPMatch k l
  where
    constraintsOnX = onVar x cns
    matchOnX = listToMaybe $ mapMaybe (\case MatchDataCon k ys -> Just (k, ys); _ -> Nothing) constraintsOnX
    cantMatchOnX = mapMaybe (\case NotDataCon k -> Just k; _ -> Nothing) constraintsOnX
    isIntX = listToMaybe $ mapMaybe (\case MatchIntLit i -> Just i; _ -> Nothing) constraintsOnX
    isNotIntX = mapMaybe (\case NotIntLit i -> Just i; _ -> Nothing) constraintsOnX

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
  U.MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, MatchDataCon k ys)
  U.NotDataCon k x -> n `addConstraint` (x, NotDataCon k)
  U.MatchIntLit i x -> n `addConstraint` (x, MatchIntLit i)
  U.NotIntLit i x -> n `addConstraint` (x, NotIntLit i)

addConstraints :: NormRefType -> [ConstraintFor] -> MaybeT F.Fresh NormRefType
addConstraints = foldM addConstraint

addConstraint :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraint nref@(_, cns) (x, c) = do
  breakIf $ any (conflictsWith c) (onVar x cns)
  addConstraintHelper nref (lookupVar x cns, c)

addConstraintHelper :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraintHelper nref@(ctx, cns) cf@(origX, c) = case c of
  --- Equation (10)
  MatchDataCon k args -> do
    case getConstructorArgs k (onVar origX cns) of
      -- 10c -- TODO(colin): Still need to add type constraints!
      Just args' -> addConstraints nref (zipWith (\a b -> (a, TermEquality b)) args args')
      Nothing -> return added
  --- Equation (11)
  NotDataCon _ -> do
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
  _ ->
    -- MatchIntLit and NotIntLit are handled here
    --   MatchIntLit does not require special treatment,
    --   because integer data constructors have no arguments.
    -- NotIntLit does not require special treatment,
    --   because we assume no one will write out every integer
    --   data constructor. So therefore we can assume inhabitation
    return added
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
  MatchDataCon k _ -> \case
    MatchDataCon k' _ | k /= k' -> True -- 10a
    NotDataCon k' | k == k' -> True -- 10b
    _ -> False
  NotDataCon k -> \case
    MatchDataCon k' _ | k == k' -> True -- 11a
    _ -> False
  MatchIntLit i -> (== NotIntLit i)
  NotIntLit i -> (== MatchIntLit i)
  TermEquality _ -> const False

-- Search for a MatchDataCon that is matching on k specifically
-- (there should be at most one, see I4 in section 3.4)
-- and if it exists, return the variable ids of its arguments
getConstructorArgs :: Ty.DataConstructor -> [Constraint] -> Maybe [F.VarID]
getConstructorArgs k cfs =
  listToMaybe $
    mapMaybe (\case MatchDataCon k' v | k' == k -> Just v; _ -> Nothing) cfs

-- substituting y *for* x
-- ie replace the second with the first, replace x with y
substituteVarIDs :: F.VarID -> F.VarID -> [ConstraintFor] -> [ConstraintFor]
substituteVarIDs y x = map (\case (x', c) | x' == x -> (y, c); cf -> cf)

-- Deals with I2 form section 3.4
-- if a variable in the context has a resolvable type, there must be at least one constructor
-- which can be instantiated without contradiction of the refinement type
-- This function tests if this is true
inhabited :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inhabited n x tau = or <$> mapM (instantiate n x) (constructors n tau)

-- the NormRefType is taken in because we may eventually have type constraints
-- and we would need to worry about that here
constructors :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
constructors _ Ty.Type {Ty.dataCons = ks} = ks

-- Attempts to "instantiate" a match of the dataconstructor k on x
-- If we can add the MatchDataCon constraint to the normalized refinement
-- type without contradiction (a Nothing value),
-- then x is inhabited by k and we return true
instantiate :: NormRefType -> F.VarID -> Ty.DataConstructor -> F.Fresh Bool
instantiate (ctx, cns) x k = do
  newVarIDs <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  let attempt = (ctx ++ newVars, cns) `addConstraint` (x, MatchDataCon k newVarIDs)
  isJust <$> runMaybeT attempt

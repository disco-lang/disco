-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import Control.Applicative
import Control.Monad (foldM, forM, guard, join, replicateM)
import Control.Monad.State (runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (nub, partition)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Types as Ty
import qualified Uncovered as U

type ConstraintFor = (F.VarID, Constraint)
type NormRefType = (U.Context, [ConstraintFor])

lookupVar :: F.VarID -> [ConstraintFor] -> F.VarID
lookupVar x [] = x
lookupVar x ((x', TermEquality y) : cs) | x' == x = lookupVar y cs
lookupVar x (_ : cs) = lookupVar x cs

lookupType :: F.VarID -> U.Context -> Ty.Type
lookupType var [] = error ("var not found in context: " ++ show var ++ " ")
lookupType x ((x', tau) : cs)
  | x' == x = tau
  | otherwise = lookupType x cs

-- doesn't respect Term Equalities, which is important. Use matching instead
matchingShallow :: F.VarID -> [ConstraintFor] -> [Constraint]
matchingShallow x = map snd . filter ((== x) . fst)

matching :: F.VarID -> [ConstraintFor] -> [Constraint]
matching x cs = matchingShallow (lookupVar x cs) cs

data Constraint where
  MatchDataCon :: Ty.DataConstructor -> [F.VarID] -> Constraint
  NotDataCon :: Ty.DataConstructor -> Constraint
  MatchIntLit :: Int -> Constraint
  NotIntLit :: Int -> Constraint
  TermEquality :: F.VarID -> Constraint
  deriving (Show, Eq, Ord)

data InhabPat where
  IPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
  IPWild :: InhabPat
  IPIntLit :: Int -> InhabPat
  IPNotIntLit :: Int -> InhabPat
  IPNotIntLits :: [Int] -> InhabPat
  IPPlaceholderInt :: InhabPat
  deriving (Show, Eq, Ord)

genInhabPos :: NE.NonEmpty F.Frame -> U.RefinementType -> [InhabPat]
genInhabPos frames (context, formula) = do
  let mNrefs = S.toList <$> normalize (context, []) formula
  let (nrefs, frames') = runState mNrefs frames
  nref <- nrefs
  join $ expandVarsPos frames' nref context

expandVarsPos :: NE.NonEmpty F.Frame -> NormRefType -> U.Context -> [[InhabPat]]
expandVarsPos frame nset vars = do
  traverse (expandVarPos frame nset) vars

expandVarPos :: NE.NonEmpty F.Frame -> NormRefType -> (F.VarID, Ty.Type) -> [InhabPat]
expandVarPos frames nref@(_, cns) (x, xType) = case matchOnX of
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
                          runMaybeT (nref <+| (x, MatchDataCon dc frs))
                      )
                )
                frames
        if null matchers
          then return IPWild
          else do
            m <- matchers
            expandVarPos frames' m (x, xType)
  Just (k, ys) -> do
    l <- expandVarsPos frames nref (zip ys (Ty.dcTypes k))
    return $ IPMatch k l
  where
    constraintsOnX = matching x cns
    matchOnX = listToMaybe $ mapMaybe (\case MatchDataCon k ys -> Just (k,ys); _ -> Nothing) constraintsOnX
    cantMatchOnX = mapMaybe (\case NotDataCon k -> Just k; _ -> Nothing) constraintsOnX
    isIntX = listToMaybe $ mapMaybe (\case MatchIntLit i -> Just i; _ -> Nothing) constraintsOnX
    isNotIntX = mapMaybe (\case NotIntLit i -> Just i; _ -> Nothing) constraintsOnX

normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (`normalize` f2) n1
  return $ S.unions rest
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize nref (U.Literal fl) = maybe S.empty S.singleton <$> runMaybeT (nref <+> fl)

(<+>) :: NormRefType -> U.Literal -> MaybeT F.Fresh NormRefType
_ <+> U.F = MaybeT . pure $ Nothing
n <+> U.T = return n
(context, constraints) <+> U.MatchDataCon k ys x = (context ++ zip ys (Ty.dcTypes k), constraints) <+| (x, MatchDataCon k ys)
n <+> U.NotDataCon k x = n <+| (x, NotDataCon k)
(context, constraints) <+> U.Let x xType y = (context ++ [(x, xType)], constraints) <+| (x, TermEquality y)
n <+> U.MatchIntLit i x = n <+| (x, MatchIntLit i)
n <+> U.NotIntLit i x = n <+| (x, NotIntLit i)

breakIf :: (Alternative f) => Bool -> f ()
breakIf = guard . not

-- substituting y *for* x
-- ie replace the second with the first, replace x with y
substituteVarIDs :: F.VarID -> F.VarID -> [ConstraintFor] -> [ConstraintFor]
substituteVarIDs y x = map (\case (x', c) | x' == x -> (y, c); cf -> cf)

addConstraints :: NormRefType -> [ConstraintFor] -> MaybeT F.Fresh NormRefType
addConstraints = foldM (<+|)

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

(<+|) :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
nref@(_, cns) <+| (x, c) = do
  breakIf $ any (conflictsWith c) (matching x cns)
  addConstraintHelper nref (lookupVar x cns, c)

addConstraintHelper :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
----- Equation (10) -----
addConstraintHelper (ctx, cns) c@(origX, MatchDataCon k vars) = do
  -- length of vars' should only ever be 0 or 1, see I4 in section 3.4
  let matches = mapMaybe (\case MatchDataCon k' v | k' == k -> Just v; _ -> Nothing) (matching origX cns)
  case matches of
    -- 10c -- TODO(colin): Still need to add type constraints!
    (vars' : _) -> addConstraints (ctx, cns) (zipWith (\a b -> (a, TermEquality b)) vars vars')
    -- 10d
    [] -> return (ctx, cns ++ [c])

----- Equation (11) -----
addConstraintHelper (ctx, cns) c@(origX, NotDataCon k) = do

  -- redundancy of NotDataCon constraint?
  -- also, should the second to origXs actually be the original x?
  -- this is a confusing bit
  inhabited <- lift (inh (ctx, cns ++ [(origX, NotDataCon k)]) origX (lookupType origX ctx))
  guard inhabited

  return (ctx, cns ++ [c])

----- Equation (14) -----
addConstraintHelper (ctx, cns) c@(origX, TermEquality y) = do
  -- I may have broken this
  let origY = lookupVar y cns
  if origX == origY
    then return (ctx, cns)
    else do
      let (noX', withX') =  partition ((/= origX) . fst) cns
      addConstraints (ctx, noX' ++ [c]) (substituteVarIDs origY origX withX')

-- This covers both MatchIntLit and NotIntLit
addConstraintHelper (ctx, cns) c =
  return (ctx, cns ++ [c])

-- we may need to add a special case for integers eventually?
-- inh _ _ tau | tau == Ty.int = pure True
inh :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inh n x tau = any isJust <$> mapM (runMaybeT . inst n x) (cons n tau)

-- the NormRefType is taken in because we may eventually have type constraints
-- and we would need to worry about that here
cons :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
cons _ Ty.Type {Ty.dataCons = ks} = ks

inst :: NormRefType -> F.VarID -> Ty.DataConstructor -> MaybeT F.Fresh NormRefType
inst (ctx, cns) x k = do
  newVarIDs <- lift $ replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  (ctx ++ newVars, cns) <+| (x, MatchDataCon k newVarIDs)

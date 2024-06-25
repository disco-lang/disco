-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import Control.Applicative
import Control.Monad (foldM, forM, guard, join, replicateM)
import Control.Monad.State (runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Types as Ty
import qualified Uncovered as U

type NormRefType = (U.Context, [Constraint])

data Constraint where
  MatchDataCon :: Ty.DataConstructor -> [F.VarID] -> F.VarID -> Constraint
  NotDataCon :: Ty.DataConstructor -> F.VarID -> Constraint
  MatchIntLit :: Int -> F.VarID -> Constraint
  NotIntLit :: Int -> F.VarID -> Constraint
  TermEquality :: F.VarID -> F.VarID -> Constraint
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
                          runMaybeT (nref <+| MatchDataCon dc frs x)
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
    origX = lookupVar x cns
    matchOnX = getMatchOnVar origX cns
    cantMatchOnX = getNotMatchOnVar origX cns
    isIntX = getIsInt origX cns
    isNotIntX = getIsNotInt origX cns

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
(context, constraints) <+> U.MatchDataCon k ys x = (context ++ zip ys (Ty.dcTypes k), constraints) <+| MatchDataCon k ys x
n <+> U.NotDataCon k x = n <+| NotDataCon k x
(context, constraints) <+> U.Let x xType y = (context ++ [(x, xType)], constraints) <+| TermEquality x y
n <+> U.MatchIntLit i x = n <+| MatchIntLit i x
n <+> U.NotIntLit i x = n <+| NotIntLit i x

breakIf :: (Alternative f) => Bool -> f ()
breakIf = guard . not

splitConstraintsOn :: F.VarID -> [Constraint] -> ([Constraint], [Constraint])
splitConstraintsOn x cns = (filter (not . containsVarID x) cns, filter (containsVarID x) cns)

containsVarID :: F.VarID -> Constraint -> Bool
containsVarID x (MatchDataCon _k _vars x') | x' == x = True
containsVarID x (NotDataCon _k x') | x' == x = True
containsVarID x (MatchIntLit _i x') | x' == x = True
containsVarID x (NotIntLit _i x') | x' == x = True
containsVarID _ _ = False

-- substituting y *for* x
-- ie replace the second with the first, replace x with y
substituteVarIDs :: F.VarID -> F.VarID -> [Constraint] -> [Constraint]
substituteVarIDs y x = map (substituteVarID y x)

substituteVarID :: F.VarID -> F.VarID -> Constraint -> Constraint
substituteVarID y x (MatchDataCon k vars x') | x' == x = MatchDataCon k vars y
substituteVarID y x (NotDataCon k x') | x' == x = NotDataCon k y
substituteVarID y x (MatchIntLit i x') | x' == x = MatchIntLit i y
substituteVarID y x (NotIntLit i x') | x' == x = NotIntLit i y
substituteVarID _ _ cn = cn

addConstraints :: NormRefType -> [Constraint] -> MaybeT F.Fresh NormRefType
addConstraints = foldM (<+|)

(<+|) :: NormRefType -> Constraint -> MaybeT F.Fresh NormRefType
----- Equation (10) -----
(ctx, cns) <+| MatchDataCon k vars x = do
  let origX = lookupVar x cns
  let matchingX = allMatchesOnVar origX cns
  -- 10a
  breakIf $ any (\(k', _, _) -> k' /= k) matchingX
  -- 10b
  breakIf $ any (origX `isNotDataCon` k) cns

  -- length of kMatchingX should only ever be 0 or 1, see I4 in section 3.4
  let kMatchingX = filter (\(k', _, _) -> k' == k) matchingX
  case kMatchingX of
    -- 10c -- TODO(colin): Still need to add type constraints!
    ((_, vars', _) : _) -> addConstraints (ctx, cns) (zipWith TermEquality vars vars')
    -- 10d
    [] -> return (ctx, cns ++ [MatchDataCon k vars origX])

----- Equation (11) -----
(ctx, cns) <+| NotDataCon k x = do
  let origX = lookupVar x cns
  breakIf $ any (origX `isMatchDataCon` k) cns

  -- redundancy of NotDataCon constraint?
  inhabited <- lift (inh (ctx, cns ++ [NotDataCon k origX]) x (lookupType x ctx))
  guard inhabited

  return (ctx, cns ++ [NotDataCon k origX])

----- Equation (14) -----
(ctx, cns) <+| TermEquality x y = do
  let x' = lookupVar x cns
  let y' = lookupVar y cns
  if x' == y'
    then return (ctx, cns)
    else do
      let (noX', withX') = splitConstraintsOn x' cns
      addConstraints (ctx, noX' ++ [TermEquality x' y']) (substituteVarIDs y' x' withX')

----- Modified Equation (10) -----
(ctx, cns) <+| MatchIntLit i x = do
  let origX = lookupVar x cns
  breakIf $ any (origX `isNotTheInt` i) cns
  return (ctx, cns ++ [MatchIntLit i origX])

----- Modified Equation (11) -----
(ctx, cns) <+| NotIntLit i x = do
  let origX = lookupVar x cns
  breakIf $ any (origX `isTheInt` i) cns
  -- breakIf $ any (origX `isNotTheInt` i) cns
  return (ctx, cns ++ [NotIntLit i origX])

isNotDataCon :: F.VarID -> Ty.DataConstructor -> (Constraint -> Bool)
isNotDataCon origX k = \case
  NotDataCon k' x' | x' == origX && k' == k -> True
  _ -> False

isMatchDataCon :: F.VarID -> Ty.DataConstructor -> (Constraint -> Bool)
isMatchDataCon origX k = \case
  MatchDataCon k' _ x' | x' == origX && k' == k -> True
  _ -> False

testMatchDataCon :: (Ty.DataConstructor -> Bool) -> (Constraint -> Bool)
testMatchDataCon kf = \case
  MatchDataCon k' _ _ -> kf k'
  _ -> False

isTheInt :: F.VarID -> Int -> (Constraint -> Bool)
isTheInt origX i = \case
  MatchIntLit i' x' | x' == origX && i' == i -> True
  _ -> False

isNotTheInt :: F.VarID -> Int -> (Constraint -> Bool)
isNotTheInt origX i = \case
  NotIntLit i' x' | x' == origX && i' == i -> True
  _ -> False

lookupVar :: F.VarID -> [Constraint] -> F.VarID
lookupVar x [] = x
lookupVar x (TermEquality x' y : cs) | x' == x = lookupVar y cs
lookupVar x (_ : cs) = lookupVar x cs

lookupType :: F.VarID -> U.Context -> Ty.Type
lookupType var ctx@[] = error ("var not found in context: " ++ show var ++ " " ++ show ctx)
lookupType x ((x', tau) : cs)
  | x' == x = tau
  | otherwise = lookupType x cs

allMatchesOnVar :: F.VarID -> [Constraint] -> [(Ty.DataConstructor, [F.VarID], F.VarID)]
allMatchesOnVar _ [] = []
allMatchesOnVar x (MatchDataCon k ys x' : cns) | x' == x = (k, ys, x) : allMatchesOnVar x cns
allMatchesOnVar x (_ : cns) = allMatchesOnVar x cns

getMatchOnVar :: F.VarID -> [Constraint] -> Maybe (Ty.DataConstructor, [F.VarID])
getMatchOnVar _ [] = Nothing
getMatchOnVar x (MatchDataCon k ys x' : _) | x' == x = Just (k, ys)
getMatchOnVar x (_ : cns) = getMatchOnVar x cns

getNotMatchOnVar :: F.VarID -> [Constraint] -> [Ty.DataConstructor]
getNotMatchOnVar _ [] = []
getNotMatchOnVar x (NotDataCon k x' : cns) | x' == x = k : getNotMatchOnVar x cns
getNotMatchOnVar x (_ : cns) = getNotMatchOnVar x cns

getIsInt :: F.VarID -> [Constraint] -> Maybe Int
getIsInt _ [] = Nothing
getIsInt x (MatchIntLit i x' : _) | x' == x = Just i
getIsInt x (_ : cns) = getIsInt x cns

getIsNotInt :: F.VarID -> [Constraint] -> [Int]
getIsNotInt _ [] = []
getIsNotInt x (NotIntLit i x' : cns) | x' == x = i : getIsNotInt x cns
getIsNotInt x (_ : cns) = getIsNotInt x cns

--
--
--

inh :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inh n x tau = any isJust <$> mapM (runMaybeT . inst n x) (cons n tau)

-- we may need to add a special case for integers eventually?
-- inh _ _ tau | tau == Ty.int = pure True

cons :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
cons _ Ty.Type {Ty.dataCons = ks} = ks

-- the NormRefType is taken in because we may eventually have type constraints
-- and we would need to worry about that here

inst :: NormRefType -> F.VarID -> Ty.DataConstructor -> MaybeT F.Fresh NormRefType
inst (ctx, cns) x k = do
  newVarIDs <- lift $ replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  (ctx ++ newVars, cns) <+| MatchDataCon k newVarIDs x

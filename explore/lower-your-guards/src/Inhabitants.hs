{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import Control.Applicative
import Control.Monad (foldM, guard, replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Parse as P
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

genInhabitants :: U.RefinementType -> F.Fresh [[P.Pattern]]
genInhabitants (context, formula) = do
  nrefs <- S.toList <$> normalize (context, []) formula
  mapM (`expandVars` context) nrefs

expandVars :: NormRefType -> U.Context -> F.Fresh [P.Pattern]
expandVars nset = mapM (expandVar nset)

expandVar :: NormRefType -> (F.VarID, Ty.Type) -> F.Fresh P.Pattern
expandVar (ctx, cns) (x, xType) = case matchingCons of
  [] -> return P.PWild
  (k : _) -> do
    freshVars <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
    P.PMatch k <$> expandVars (ctx, cns) (zip freshVars (Ty.dcTypes k))
  where
    origX = lookupVar x cns
    matchingCons = [k | k <- Ty.dataCons xType, any (origX `isMatchDataCon` k) cns]

normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (`normalize` f2) n1
  return $ S.unions rest
-- S.unions $ S.map (\r -> normalize (Just r) f2) (normalize nref f1)
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize nref fl = maybe S.empty S.singleton <$> runMaybeT (nref <+> fl)

(<+>) :: NormRefType -> U.Formula -> MaybeT F.Fresh NormRefType
_ <+> U.F = MaybeT . pure $ Nothing
n <+> U.T = return n
(context, constraints) <+> U.MatchDataCon k ys x = (context ++ zip ys (Ty.dcTypes k), constraints) <+| MatchDataCon k ys x
n <+> U.NotDataCon k x = n <+| NotDataCon k x
(context, constraints) <+> U.Let x xType y = (context ++ [(x, xType)], constraints) <+| TermEquality x y
(context, constraints) <+> U.MatchIntLit i x = (context, constraints) <+| MatchIntLit i x
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

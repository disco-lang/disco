{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import Control.Monad (replicateM)
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
  nrefs <- S.toList <$> normalize (Just (context, [])) formula
  mapM (`expandVars` context) nrefs
-- S.map (`expandVars` context) <$> toList <$> normalize (Just (context, [])) formula

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
    matchingCons = [k | k <- Ty.dataCons xType, any (origX `isType` k) cns]

normalize :: Maybe NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (\r -> normalize (Just r) f2) n1
  return $ S.unions rest
-- S.unions $ S.map (\r -> normalize (Just r) f2) (normalize nref f1)
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize Nothing _ = return S.empty
normalize (Just nref) fl = maybe S.empty S.singleton <$> (nref <+> fl)

(<+>) :: NormRefType -> U.Formula -> F.Fresh (Maybe NormRefType)
_ <+> U.F = return Nothing
n <+> U.T = return $ Just n
(context, constraints) <+> U.MatchDataCon k ys x = Just (context ++ zip ys (Ty.dcTypes k), constraints) <+| MatchDataCon k ys x
n <+> U.NotDataCon k x = Just n <+| NotDataCon k x
(context, constraints) <+> U.Let x xType y = Just (context ++ [(x, xType)], constraints) <+| TermEquality x y
(context, constraints) <+> U.MatchIntLit i x = Just (context, constraints) <+| MatchIntLit i x
n <+> U.NotIntLit i x = Just n <+| NotIntLit i x

(<+|) :: Maybe NormRefType -> Constraint -> F.Fresh (Maybe NormRefType)
Nothing <+| _ = return Nothing
-- TODO(colin): 10a,10c:
Just (ctx, cns) <+| MatchDataCon k vars x
  | any (origX `isNotType` k) cns = return Nothing
  | otherwise = return $ Just (ctx, cns ++ [MatchDataCon k vars origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| NotDataCon k x = do
  let origX = lookupVar x cns
  if any (origX `isType` k) cns
    then return Nothing
    else do
      b <- not <$> inh (ctx, cns ++ [NotDataCon k origX]) x (lookupType x ctx)
      if b
        then return Nothing
        else return $ Just (ctx, cns ++ [NotDataCon k origX])
-- \| any (origX `isType` k) cns = return Nothing
-- \| not $ inh (ctx, cns ++ [NotDataCon k origX]) x (lookupType x ctx) = return Nothing
-- \| otherwise = return $ Just (ctx, cns ++ [NotDataCon k origX])
-- where
--   origX = lookupVar x cns
Just (ctx, cns) <+| TermEquality x y
  | x' == y' = return $ Just (ctx, cns)
  | otherwise = return $ Just (ctx, cns ++ [TermEquality x y])
  where
    -- error "TODO(colin)"

    x' = lookupVar x cns
    y' = lookupVar y cns
-- colin test
Just (ctx, cns) <+| MatchIntLit i x
  | not $ any (origX `isNotTheInt` i) cns = return Nothing
  | otherwise = return $ Just (ctx, cns ++ [MatchIntLit i origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| NotIntLit i x
  | any (origX `isTheInt` i) cns = return Nothing
  | otherwise = return $ Just (ctx, cns ++ [NotIntLit i origX])
  where
    origX = lookupVar x cns

isNotType :: F.VarID -> Ty.DataConstructor -> (Constraint -> Bool)
isNotType origX k = \case
  NotDataCon k' x' | x' == origX && k' == k -> True
  _ -> False

isType :: F.VarID -> Ty.DataConstructor -> (Constraint -> Bool)
isType origX k = \case
  MatchDataCon k' _ x' | x' == origX && k' == k -> True
  _ -> False

isTheInt :: F.VarID -> Int -> (Constraint -> Bool)
isTheInt origX i = \case
  NotIntLit i' x' | x' == origX && i' == i -> True
  _ -> False

isNotTheInt :: F.VarID -> Int -> (Constraint -> Bool)
isNotTheInt origX i = \case
  MatchIntLit i' x' | x' == origX && i' == i -> True
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

--
--
--

inh :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inh n x tau = any isJust <$> mapM (inst n x) (cons n tau)

cons :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
cons _ Ty.Type {Ty.dataCons = ks} = ks

-- cons _ _ = error "TODO(colin) no match on Cons"

inst :: NormRefType -> F.VarID -> Ty.DataConstructor -> F.Fresh (Maybe NormRefType)
inst (ctx, cns) x k = do
  newVarIDs <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  Just (ctx ++ newVars, cns) <+| MatchDataCon k newVarIDs x

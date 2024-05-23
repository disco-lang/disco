{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inhabitants where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Uncovered as U
import qualified Types as Ty
import Data.Maybe (isJust)
import qualified Parse as P
import qualified GuardTree as G

type NormRefType = (U.Context, [Constraint])

data Constraint where
  MatchDataCon :: Ty.DataConstructor -> [Text] -> Text -> Constraint
  NotDataCon :: Ty.DataConstructor -> Text -> Constraint
  MatchIntLit :: Int -> Text -> Constraint
  NotIntLit :: Int -> Text -> Constraint
  TermEquality :: Text -> Text -> Constraint
  deriving (Show, Eq, Ord)

genInhabitants :: U.RefinementType -> S.Set [P.Pattern]
genInhabitants (context, formula) = S.map (`expandVars` context) $ normalize (Just (context, [])) formula

expandVars :: NormRefType -> U.Context -> [P.Pattern]
expandVars nset = map (expandVar nset)

expandVar :: NormRefType -> (Text, Ty.Type) -> P.Pattern
expandVar (ctx, cns) (x, xType) = case matchingCons of
  [] -> P.PWild
  (k : _) -> P.PMatch k (expandVars (ctx, cns) (zip (G.getYs 100) (Ty.dcTypes k)))
  where
    origX = lookupVar x cns
    matchingCons = [k | k <- Ty.dataCons xType, any (origX `isType` k) cns]

normalize :: Maybe NormRefType -> U.Formula -> S.Set NormRefType
normalize nref (f1 `U.And` f2) = S.unions $ S.map (\r -> normalize (Just r) f2) (normalize nref f1)
normalize nref (f1 `U.Or` f2) = normalize nref f1 `S.union` normalize nref f2
normalize Nothing _ = S.empty
normalize (Just nref) fl = maybe S.empty S.singleton (nref <+> fl)

(<+>) :: NormRefType -> U.Formula -> Maybe NormRefType
_ <+> U.F = Nothing
n <+> U.T = Just n
(context, constraints) <+> U.MatchDataCon k ys x = Just (context ++ zip ys (Ty.dcTypes k), constraints) <+| MatchDataCon k ys x
n <+> U.NotDataCon k x = Just n <+| NotDataCon k x
(context, constraints) <+> U.Let x xType y = Just (context ++ [(x,xType)], constraints) <+| TermEquality x y
(context, constraints) <+> U.MatchIntLit i x = Just (context, constraints) <+| MatchIntLit i x
n <+> U.NotIntLit i x = Just n <+| NotIntLit i x

(<+|) :: Maybe NormRefType -> Constraint -> Maybe NormRefType
Nothing <+| _ = Nothing
-- TODO(colin): 10a,10c:
Just (ctx, cns) <+| MatchDataCon k vars x
  | any (origX `isNotType` k) cns = Nothing
  | otherwise = Just (ctx, cns ++ [MatchDataCon k vars origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| NotDataCon k x
  | any (origX `isType` k) cns = Nothing
  | not $ inh (ctx, cns ++ [NotDataCon k origX]) x (lookupType x ctx) = Nothing
  | otherwise = Just (ctx, cns ++ [NotDataCon k origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| TermEquality x y
  | x' == y' = Just (ctx, cns)
  | otherwise = Just (ctx, cns ++ [TermEquality x y])
  -- error "TODO(colin)"
  where
    x' = lookupVar x cns
    y' = lookupVar y cns
-- colin test
Just (ctx, cns) <+| MatchIntLit i x
  | not $ any (origX `isNotTheInt` i) cns = Nothing
  | otherwise = Just (ctx, cns ++ [MatchIntLit i origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| NotIntLit i x
  | any (origX `isTheInt` i) cns = Nothing
  | otherwise = Just (ctx, cns ++ [NotIntLit i origX])
  where
    origX = lookupVar x cns


isNotType :: Text -> Ty.DataConstructor -> (Constraint -> Bool)
isNotType origX k = \case
  NotDataCon k' x' | x' == origX && k' == k -> True
  _ -> False

isType :: Text -> Ty.DataConstructor -> (Constraint -> Bool)
isType origX k = \case
  MatchDataCon k' _ x' | x' == origX && k' == k -> True
  _ -> False

isTheInt :: Text -> Int -> (Constraint -> Bool)
isTheInt origX i = \case
  NotIntLit i' x' | x' == origX && i' == i -> True
  _ -> False

isNotTheInt :: Text -> Int -> (Constraint -> Bool)
isNotTheInt origX i = \case
  MatchIntLit i' x' | x' == origX && i' == i -> True
  _ -> False

lookupVar :: Text -> [Constraint] -> Text
lookupVar x [] = x
lookupVar x (TermEquality x' y : cs) | x' == x = lookupVar y cs
lookupVar x (_ : cs) = lookupVar x cs

lookupType :: Text -> U.Context -> Ty.Type
lookupType _ [] = error "var not found in context"
lookupType x ((x',tau):cs)
  | x' == x = tau
  | otherwise = lookupType x cs

--
--
--

inh :: NormRefType -> Text -> Ty.Type -> Bool
inh n x tau = any (isJust . inst n x) (cons n tau)

cons :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
cons _ Ty.Type { Ty.dataCons = ks } = ks
-- cons _ _ = error "TODO(colin) no match on Cons"

inst :: NormRefType -> Text -> Ty.DataConstructor -> Maybe NormRefType
inst (ctx, cns) x k = Just (ctx ++ map ("FRESH",) (Ty.dcTypes k), cns) <+| MatchDataCon k (map (const "FRESH") (Ty.dcTypes k)) x

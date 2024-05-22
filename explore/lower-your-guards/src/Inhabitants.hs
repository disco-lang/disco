{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inhabitants where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Parse as P
import qualified Uncovered as U

type NormRefType = (U.Context, [Constraint])

data Constraint where
  MatchDataCon :: Text -> [Text] -> Text -> Constraint
  NotDataCon :: Text -> Text -> Constraint
  MatchIntLit :: Int -> Text -> Constraint
  NotIntLit :: Int -> Text -> Constraint
  TypeEquality :: Text -> Text -> Constraint
  deriving (Show, Eq, Ord)

genInhabitants :: U.RefinementType -> S.Set NormRefType
genInhabitants (context, formula) = normalize (Just (context, [])) formula

normalize :: Maybe NormRefType -> U.Formula -> S.Set NormRefType
normalize nref (f1 `U.And` f2) = S.unions $ S.map (\r -> normalize (Just r) f2) (normalize nref f1)
normalize nref (f1 `U.Or` f2) = normalize nref f1 `S.union` normalize nref f2
normalize Nothing _ = S.empty
normalize (Just nref) fl = maybe S.empty S.singleton (nref <+> fl)

(<+>) :: NormRefType -> U.Formula -> Maybe NormRefType
_ <+> U.F = Nothing
n <+> U.T = Just n
(context, constraints) <+> U.MatchDataCon dc vars x = Just (context ++ map (,P.ColinMistake) vars, constraints) <+| MatchDataCon dc vars x
n <+> U.NotDataCon k x = Just n <+| NotDataCon k x
(context, constraints) <+> U.Let x y = Just (context ++ [(x,P.ColinMistake)], constraints) <+| TypeEquality x y
(context, constraints) <+> U.MatchIntLit i x = Just (context, constraints) <+| MatchIntLit i x
n <+> U.NotIntLit i x = Just n <+| NotIntLit i x

(<+|) :: Maybe NormRefType -> Constraint -> Maybe NormRefType
Nothing <+| _ = Nothing
-- TODO(colin): 10a,10c:
Just (ctx, cns) <+| MatchDataCon k vars x
  | not $ any (origX `isNotType` k) cns = Nothing
  | otherwise = Just (ctx, cns ++ [MatchDataCon k vars origX])
  where
    origX = lookupVar x cns
-- TODO(colin): 11b:
Just (ctx, cns) <+| NotDataCon k x
  | any (origX `isType` k) cns = Nothing
  | otherwise = Just (ctx, cns ++ [NotDataCon k origX])
  where
    origX = lookupVar x cns
Just (ctx, cns) <+| TypeEquality x y
  | x' == y' = Just (ctx, cns)
  | otherwise = Just (ctx, cns ++ [TypeEquality x y])
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
-- TODO(colin): 11b:
Just (ctx, cns) <+| NotIntLit i x
  | any (origX `isTheInt` i) cns = Nothing
  | otherwise = Just (ctx, cns ++ [NotIntLit i origX])
  where
    origX = lookupVar x cns


isNotType :: Text -> Text -> (Constraint -> Bool)
isNotType origX k = \case
  NotDataCon k' x' | x' == origX && k' == k -> True
  _ -> False

isType :: Text -> Text -> (Constraint -> Bool)
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
lookupVar x (TypeEquality x' y : cs) | x' == x = lookupVar y cs
lookupVar x (_ : cs) = lookupVar x cs

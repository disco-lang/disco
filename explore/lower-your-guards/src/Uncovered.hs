{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Uncovered where

import qualified Fresh as F
import qualified GuardTree as G
import qualified Types as Ty

type RefinementType = (Context, Formula)

type Context = [(F.VarID, Ty.Type)]

data Formula where
  And :: Formula -> Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Literal :: Literal -> Formula
  deriving (Show, Eq)

data Literal where
  T :: Literal
  F :: Literal
  NotDataCon :: Ty.DataConstructor -> F.VarID -> Literal
  MatchDataCon :: Ty.DataConstructor -> [F.VarID] -> F.VarID -> Literal
  NotIntLit :: Int -> F.VarID -> Literal
  MatchIntLit :: Int -> F.VarID -> Literal
  Let :: F.VarID -> Ty.Type -> F.VarID -> Literal
  deriving (Show, Eq)

uncovered :: RefinementType -> G.Gdt -> RefinementType
uncovered r g = case g of
  G.Grhs _ ->
    let (context, _) = r in (context, Literal F)
  G.Branch t1 t2 ->
    uncovered (uncovered r t1) t2
  G.Guarded (G.Match dataCon terms var) t ->
    (r `liftAndLit` NotDataCon dataCon var) `union` uncovered (r `liftAndLit` MatchDataCon dataCon terms var) t
  G.Guarded (G.MatchLit i v) t ->
    (r `liftAndLit` NotIntLit i v) `union` uncovered (r `liftAndLit` MatchIntLit i v) t
  G.Guarded (G.Let lhs lType rhs) t ->
    uncovered (r `liftAndLit` Let lhs lType rhs) t

liftAndLit :: RefinementType -> Literal -> RefinementType
liftAndLit (cont, form) f = (cont, form `And` Literal f)

-- Note: I think this should only be called in situations
-- where cont1 == cont2. The paper doesn't specify.
-- I am just using the first
union :: RefinementType -> RefinementType -> RefinementType
union (cont1, f1) (cont2, f2) = (cont1, f1 `Or` f2)

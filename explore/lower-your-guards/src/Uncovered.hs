{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Uncovered where

import Data.Text (Text)
import qualified GuardTree as G
import qualified Types as Ty

type RefinementType = (Context, Formula)

type Context = [(G.Var, Ty.Type)]

data Formula where
  And :: Formula -> Formula -> Formula
  Or :: Formula -> Formula -> Formula
  T :: Formula
  F :: Formula
  NotDataCon :: Ty.DataConstructor -> G.Var -> Formula
  MatchDataCon :: Ty.DataConstructor -> [G.Var] -> G.Var -> Formula
  NotIntLit :: Int -> G.Var -> Formula
  MatchIntLit :: Int -> G.Var -> Formula
  Let :: G.Var -> Ty.Type -> G.Var -> Formula
  deriving (Show, Eq)

uncovered :: RefinementType -> G.Gdt -> RefinementType
uncovered r g = case g of
  G.Grhs _ -> let (context, _) = r in (context, F)
  G.Branch t1 t2 -> uncovered (uncovered r t1) t2
  (G.Guarded (G.Match dataCon terms var) t) ->  (r `liftAnd` NotDataCon dataCon var) `union` uncovered (r `liftAnd` MatchDataCon dataCon terms var) t
  (G.Guarded (G.MatchLit i v) t) -> (r `liftAnd` NotIntLit i v) `union` uncovered (r `liftAnd` MatchIntLit i v) t
  (G.Guarded (G.Let lhs lType rhs) t) -> uncovered (r `liftAnd` Let lhs lType rhs) t

liftAnd :: RefinementType -> Formula -> RefinementType
liftAnd (cont, form) f = (cont, form `And` f)

-- Note: I think this should only be called in situations
-- where cont1 == cont2. The paper doesn't specify.
-- I am just using the first
union :: RefinementType -> RefinementType -> RefinementType
union (cont1, f1) (cont2, f2) = (cont1, f1 `Or` f2)

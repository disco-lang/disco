{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Annotated where

import qualified GuardTree as G
import qualified Uncovered as U

data Ant where
  Grhs :: U.RefinementType -> Int -> Ant
  Branch :: Ant -> Ant -> Ant

annotated :: U.RefinementType -> G.Gdt -> Ant
annotated ref gdt = case gdt of
  G.Grhs i -> Grhs ref i
  G.Branch t1 t2 -> Branch (annotated ref t1) (annotated (U.uncovered ref t1) t2)
  G.Guarded g t -> case g of
    G.Match k args x -> annotated (ref `U.liftAndLit` U.MatchDataCon k args x) t
    G.MatchLit i x -> annotated (ref `U.liftAndLit` U.MatchIntLit i x) t
    G.Let lhs lType rhs -> annotated (ref `U.liftAndLit` U.Let lhs lType rhs) t

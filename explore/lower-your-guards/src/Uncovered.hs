{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Uncovered where

import qualified GuardTree as G
import MatchInfo

type RefinementType = (Context, Formula)

type Context = [TypedVar]

data Formula where
  And :: Formula -> Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Literal :: Literal -> Formula
  deriving (Show, Eq)

data Literal where
  T :: Literal
  F :: Literal
  Info :: TypedVar -> MatchInfo -> Literal
  deriving (Show, Eq)

uncovered :: RefinementType -> G.Gdt -> RefinementType
uncovered r g = case g of
  G.Grhs _ ->
    let (context, _) = r in (context, Literal F)
  G.Branch t1 t2 ->
    uncovered (uncovered r t1) t2
  G.Guarded (var, guard) t -> case guard of
    G.GMatch dataCon ys -> noMatch `union` matchedPath
     where
      noMatch = r `liftAndLit` varInfo (Not dataCon)
      matchedPath = uncovered (r `liftAndLit` varInfo (Match dataCon ys)) t
    G.GWas old -> uncovered (r `liftAndLit` varInfo (WasOriginally old)) t
   where
    varInfo = Info var

liftAndLit :: RefinementType -> Literal -> RefinementType
liftAndLit (cont, form) f = (cont, form `And` Literal f)

-- Note: I think this should only be called in situations
-- where cont1 == cont2. The paper doesn't specify.
-- I am just using the first
union :: RefinementType -> RefinementType -> RefinementType
union (cont1, f1) (_cont2, f2) = (cont1, f1 `Or` f2)

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Uncovered where

import Data.Text (Text)
import GuardTree
import qualified Parse as P

type RefinementType = (Context, Formula)

type Context = [(Var, P.Type)]

data Formula where
  And :: Formula -> Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Lit :: FLiteral -> Formula
  deriving (Show, Eq)

data FLiteral where
  T :: FLiteral
  F :: FLiteral
  NotDataCon :: Var -> DataCon -> FLiteral
  NotIntLit :: Int -> Var -> FLiteral
  MatchInt :: Int -> Var -> FLiteral
  FLet :: Var -> Text -> FLiteral
  deriving (Show, Eq)

uncovered :: RefinementType -> Gdt -> RefinementType
uncovered r g = case g of
  Grhs _ -> let (context, _) = r in (context, Lit F)
  Branch t1 t2 -> uncovered (uncovered r t1) t2
  (Guarded (Match {}) _) -> error "TODO(colin)"
  (Guarded (MatchLit i v) t) -> (r `liftAnd` NotIntLit i v) `union` uncovered (r `liftAnd` MatchInt i v) t
  (Guarded (Let lhs rhs) t) -> uncovered (r `liftAnd` FLet lhs rhs) t

liftAnd :: RefinementType -> FLiteral -> RefinementType
liftAnd (cont, form) f = (cont, And form (Lit f))

-- Note: I think this should only be called in situations
-- where cont1 == cont2. The paper doesn't specify.
-- I am just using the first
union :: RefinementType -> RefinementType -> RefinementType
union (cont1, f1) (cont2, f2) = (cont1, Or f1 f2)

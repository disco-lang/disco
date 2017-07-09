module Listcomp where

import qualified Data.Map as M
import Data.Maybe (fromJust)

type Var = String

data Term
  = V Var
  | I Integer
  | B Op Term Term
  | L [Term]
  | R Term
  | T Integer Term
  | A Term Term
  | Concat Term
  | MS Term
  | C Term [Qual]
  deriving Show

data Qual
  = Bind Var Term
  deriving Show

data Op
  = Plus
  | Times
  deriving Show

data Value
  = VI Integer
  | VNil
  | VCons Value Value
  | VThunk Env Term
  | VTake Integer Value
  | VFoldr (Value -> Value -> Value) Value Value

vappend :: Value -> Value -> Value
vappend v1 v2 = VFoldr VCons v2 v1

vconcat :: Value -> Value
vconcat = VFoldr vappend VNil

vmap :: (Value -> Value) -> Value -> Value
vmap f = VFoldr (VCons . f) VNil

vsucc :: Env -> Value -> Value
vsucc e v = case rnfV e v of
  VI i -> VI (i+1)

type Env = M.Map String Value

whnf :: Env -> Term -> Value
whnf e (V x)        = whnfV e (fromJust (M.lookup x e))
whnf _ (I i)        = VI i
whnf e (B op t1 t2) = interpOp op (whnf e t1) (whnf e t2)
whnf _ (L [])       = VNil
whnf e (L (t:ts))   = VCons (VThunk e t) (VThunk e (L ts))
whnf e (R t)        = VCons (VThunk e t) (VThunk e (R t))
whnf e (T n t)      = whnfV e (VTake n (VThunk e t))
whnf e (A t1 t2)    = whnfV e (vappend (VThunk e t1) (VThunk e t2))
whnf e (Concat t)   = whnfV e (vconcat (VThunk e t))
whnf e (MS t)       = whnfV e (vmap (vsucc e) (VThunk e t))
whnf e (C t qs)     = whnfV e (desugarComp e t qs)

desugarComp :: Env -> Term -> [Qual] -> Value
desugarComp e t [] = VCons (VThunk e t) VNil
desugarComp e t (Bind x l : qs)
  = vconcat (vmap (\v -> desugarComp (M.insert x v e) t qs) (VThunk e l))

whnfV :: Env -> Value -> Value
whnfV _ (VThunk e t) = whnf e t
whnfV _ (VTake 0 _) = VNil
whnfV e (VTake n v) =
  case whnfV e v of
    VNil        -> VNil
    VCons vh vt -> VCons vh (VTake (n-1) vt)
whnfV e (VFoldr c n v) =
  case whnfV e v of
    VNil        -> whnfV e n
    VCons vh vt -> whnfV e (c vh (VFoldr c n vt))
whnfV _ v            = v

rnf :: Env -> Term -> Value
rnf e t = rnfV e (whnf e t)

rnfV :: Env -> Value -> Value
rnfV _ v@(VI {}) = v
rnfV _ v@VNil    = v
rnfV e (VCons vh vt) = VCons (rnfV e vh) (rnfV e vt)
rnfV e v = rnfV e (whnfV e v)

interpOp :: Op -> Value -> Value -> Value
interpOp Plus  (VI a) (VI b) = VI (a + b)
interpOp Times (VI a) (VI b) = VI (a * b)

pretty :: Value -> String
pretty (VI i) = show i
pretty (VNil) = "[]"
pretty (VCons vh vt) = pretty vh ++ " : " ++ pretty vt

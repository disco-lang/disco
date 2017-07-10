module Listcomp2 where

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
--  | MS Term
  | C Term [Qual]
  deriving Show

data Qual
  = Bind Var Term
  deriving Show

data Op
  = Plus
  | Times
  deriving Show

example :: Term
example = T 3 (C (B Times (V "x") (V "x")) [Bind "y" (L [I 1, I 2, I 3]), Bind "x" (T 2 (R (V "y")))])

data Value
  = VI Integer
  | VNil
  | VCons Value Value
  | VThunk Env Term
  | VTake Integer Value
  | VDelay (IO Value)

vfoldr :: Env -> (Value -> Value -> Value) -> Value -> Value -> Value
vfoldr e f z xs = VDelay $ do
  xs' <- whnfV e xs
  case xs' of
    VNil        -> return z
    VCons vh vt -> return $ f vh (vfoldr e f z vt)

vappend :: Env -> Value -> Value -> Value
vappend e v1 v2 = vfoldr e VCons v2 v1

vconcat :: Env -> Value -> Value
vconcat e = vfoldr e (vappend e) VNil

vmap :: Env -> (Value -> Value) -> Value -> Value
vmap e f = vfoldr e (VCons . f) VNil

-- vsucc :: Env -> Value -> Value
-- vsucc e v = case rnfV e v of
--   VI i -> VI (i+1)

type Env = M.Map String Value

whnf :: Env -> Term -> IO Value
whnf e (V x)        = whnfV e (fromJust (M.lookup x e))
whnf _ (I i)        = putStrLn ("Evaluated " ++ show i) >> return (VI i)
whnf e (B op t1 t2) = do
  v1 <- whnf e t1
  v2 <- whnf e t2
  return $ interpOp op v1 v2
whnf _ (L [])     = return VNil
whnf e (L (t:ts)) = return $ VCons (VThunk e t) (VThunk e (L ts))
whnf e (R t)      = return $ VCons (VThunk e t) (VThunk e (R t))
whnf e (T n t)    = whnfV e (VTake n (VThunk e t))
whnf e (A t1 t2)  = whnfV e (vappend e (VThunk e t1) (VThunk e t2))
whnf e (Concat t) = whnfV e $ vconcat e (VThunk e t)
-- whnf e (MS t)     = whnfV e (vmap (vsucc e) (VThunk e t))
whnf e (C t qs)     = whnfV e (desugarComp e t qs)

desugarComp :: Env -> Term -> [Qual] -> Value
desugarComp e t [] = VCons (VThunk e t) VNil
desugarComp e t (Bind x l : qs)
  = vconcat e (vmap e (\v -> desugarComp (M.insert x v e) t qs) (VThunk e l))

whnfV :: Env -> Value -> IO Value
whnfV _ (VThunk e t) = whnf e t
whnfV _ (VTake 0 _) = return VNil
whnfV e (VTake n v) = do
  v' <- whnfV e v
  case v' of
    VNil        -> return VNil
    VCons vh vt -> return $ VCons vh (VTake (n-1) vt)
whnfV e (VDelay iov) = iov >>= whnfV e

-- whnfV e (VFoldr c n v) =
--   case whnfV e v of
--     VNil        -> whnfV e n
--     VCons vh vt -> whnfV e (c vh (VFoldr c n vt))
whnfV _ v            = return v

rnf :: Env -> Term -> IO Value
rnf e t = whnf e t >>= rnfV e

rnfV :: Env -> Value -> IO Value
rnfV _ v@(VI {}) = return v
rnfV _ v@VNil    = return v
rnfV e (VCons vh vt) = do
  h <- rnfV e vh
  t <- rnfV e vt
  return $ VCons h t
rnfV e v = whnfV e v >>= rnfV e

interpOp :: Op -> Value -> Value -> Value
interpOp Plus  (VI a) (VI b) = VI (a + b)
interpOp Times (VI a) (VI b) = VI (a * b)

pretty :: Value -> String
pretty (VI i) = show i
pretty (VNil) = "[]"
pretty (VCons vh vt) = pretty vh ++ " : " ++ pretty vt

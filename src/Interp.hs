{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Interp where

import           Control.Monad           (join)
import           Control.Monad.Except    (ExceptT, runExceptT, throwError)
import           Data.Char               (toLower)
import           Data.List               (find)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           Data.Ratio              ((%), numerator, denominator)

import           Unbound.LocallyNameless (Bind, LFreshM, Name, lunbind, translate,
                                          runLFreshM, unembed)

import           Parser
import           Typecheck hiding (ok)
import           Types

data Value where
  VUnit :: Value
  VBool :: Bool -> Value
  VClos :: Bind (Name ATerm) ATerm -> Env -> Value    -- closure
  VPair :: Value -> Value -> Value
  VInj  :: Side -> Value -> Value
  VNum  :: Rational -> Value
  deriving Show

data InterpError where
  UnboundError  :: Name ATerm -> InterpError
  NotANum       :: Value      -> InterpError  -- ^ v should be a number, but isn't
  DivByZero     ::               InterpError
  NotABool      :: Value      -> InterpError  -- ^ should be a boolean, but isn't
  NotAFun       :: Value      -> InterpError
  NonExhaustive ::               InterpError
  deriving Show

type Env = M.Map (Name ATerm) Value

-- | Interpreter monad.  Can throw InterpErrors, and generate fresh
-- names.
type IM = ExceptT InterpError LFreshM

runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT

interpTermTop :: ATerm -> IM Value
interpTermTop = interpTerm M.empty

interpTerm :: Env -> ATerm -> IM Value
interpTerm e (ATVar _ x)       = maybe (throwError $ UnboundError x) return (M.lookup x e)
interpTerm _ ATUnit            = return VUnit
interpTerm _ (ATBool b)        = return $ VBool b
interpTerm e (ATAbs _ c)       = return $ VClos c e

  -- XXX FIX ME: lazy evaluation
interpTerm e (ATApp _ f x)     = join (interpApp <$> interpTerm e f <*> interpTerm e x)
interpTerm e (ATPair _ l r)    = VPair <$> interpTerm e l <*> interpTerm e r
interpTerm e (ATInj _ s t)     = VInj s <$> interpTerm e t
interpTerm _ (ATNat i)         = return $ VNum (i % 1)
interpTerm e (ATUn _ op t)     = interpTerm e t >>= interpUOp op
interpTerm e (ATBin ty op l r) = join (interpBOp ty op <$> interpTerm e l <*> interpTerm e r)

  -- XXX FIX ME: lazy evaluation
interpTerm e (ATLet _ t)       =
  lunbind t $ \((x, unembed -> t1), t2) -> do
  v1 <- interpTerm e t1
  interpTerm (M.insert x v1 e) t2

interpTerm e (ATCase _ bs)     = interpCase e bs
interpTerm e (ATAscr t _)      = interpTerm e t
interpTerm e (ATSub _ t)       = interpTerm e t

interpApp :: Value -> Value -> IM Value
interpApp (VClos c e) v   = lunbind c $ \(x,t) -> interpTerm (M.insert x v e) t
interpApp f _             = throwError $ NotAFun f

interpUOp :: UOp -> Value -> IM Value
interpUOp Neg (VNum n) = return $ VNum (-n)
interpUOp Neg v        = throwError $ NotANum v

interpBOp :: Type -> BOp -> Value -> Value -> IM Value
interpBOp _ Add     = numOp' (+)
interpBOp _ Sub     = numOp' (-)
interpBOp _ Mul     = numOp' (*)
interpBOp _ Div     = divOp
interpBOp _ Exp     = expOp
interpBOp ty Eq     = \v1 v2 -> return $ VBool (decideFor ty v1 v2)
interpBOp ty Neq    = \v1 v2 -> return $ VBool (not (decideFor ty v1 v2))
interpBOp _ Lt      = undefined
interpBOp _ Gt      = undefined
interpBOp _ Leq     = undefined
interpBOp _ Geq     = undefined
interpBOp _ And     = boolOp (&&)
interpBOp _ Or      = boolOp (||)
interpBOp _ Mod     = modOp
interpBOp _ Divides = numOp divides
interpBOp _ RelPm   = numOp relPm

numOp :: (Rational -> Rational -> Value) -> Value -> Value -> IM Value
numOp (#) (VNum x) (VNum y) = return $ x # y
numOp _   (VNum _) y        = throwError $ NotANum y
numOp _   x        _        = throwError $ NotANum x

numOp' :: (Rational -> Rational -> Rational) -> Value -> Value -> IM Value
numOp' f = numOp (\x y -> VNum (f x y))

divOp :: Value -> Value -> IM Value
divOp (VNum x) (VNum y)
  | y == 0    = throwError DivByZero
  | otherwise = return $ VNum (x / y)
divOp (VNum _) y = throwError $ NotANum y
divOp x        _ = throwError $ NotANum x

expOp :: Value -> Value -> IM Value
expOp (VNum x) (VNum y) = return $ VNum (x ^^ (numerator y))
  -- if the program typechecks, y will be an integer
expOp (VNum _) y = throwError $ NotANum y
expOp x        _ = throwError $ NotANum x

modOp :: Value -> Value -> IM Value
modOp (VNum x) (VNum y)
  | y == 0    = throwError DivByZero
  | otherwise = return $ VNum ((numerator x `mod` numerator y) % 1)
                -- This is safe since if the program typechecks, mod will only ever be
                -- called on integral things.
modOp (VNum _) y = throwError $ NotANum y
modOp x        _ = throwError $ NotANum x

divides :: Rational -> Rational -> Value
divides 0 0 = VBool True
divides 0 _ = VBool False
divides x y = VBool $ denominator (y / x) == 1

relPm :: Rational -> Rational -> Value
relPm (numerator -> x) (numerator -> y) = VBool $ gcd x y == 1

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> IM Value
boolOp (#) (VBool x) (VBool y) = return $ VBool (x # y)
boolOp _   (VBool _) y         = throwError $ NotABool y
boolOp _   x         _         = throwError $ NotABool x

decideFor :: Type -> Value -> Value -> Bool
decideFor (TyPair ty1 ty2) (VPair x1 y1) (VPair x2 y2)
  = decideFor ty1 x1 x2 && decideFor ty2 y1 y2
decideFor (TySum ty1 _) (VInj L v1) (VInj L v2)
  = decideFor ty1 v1 v2
decideFor (TySum _ ty2) (VInj R v1) (VInj R v2)
  = decideFor ty2 v1 v2
decideFor (TyArr ty1 ty2) c1@(VClos {}) c2@(VClos {})
  = undefined
  -- = and (zipWith (decideFor ty2) (map f1 ty1s) (map f2 ty1s))
  -- where
  --   mkFun (VClos x t e) v = interpTerm (M.insert x v e) t
  --   f1 = mkFun c1
  --   f2 = mkFun c2
  --   ty1s = enumerate ty1
decideFor _ v1 v2 = primValEq v1 v2

primValEq :: Value -> Value -> Bool
primValEq VUnit VUnit           = True
primValEq (VBool b1) (VBool b2) = b1 == b2
primValEq (VNum n1) (VNum n2)   = n1 == n2
primValEq _ _                   = False

enumerate :: Type -> [Value]
enumerate TyVoid           = []
enumerate TyUnit           = [VUnit]
enumerate TyBool           = [VBool False, VBool True]
enumerate (TyArr ty1 ty2)  = map (mkFun vs1) (sequence (vs2 <$ vs1))
  where
    vs1   = enumerate ty1
    vs2   = enumerate ty2
    mkFun vs1 vs2 = undefined
      -- VFun $ \v -> snd . fromJust $ find (decideFor ty1 v . fst) (zip vs1 vs2)
enumerate (TyPair ty1 ty2) = [ VPair x y | x <- enumerate ty1, y <- enumerate ty2 ]
enumerate (TySum ty1 ty2)  = map (VInj L) (enumerate ty1) ++ map (VInj R) (enumerate ty2)
enumerate _                = []  -- other cases shouldn't happen if the program type checks

interpCase :: Env -> [ABranch] -> IM Value
interpCase _ []     = throwError NonExhaustive
interpCase e (b:bs) = do
  lunbind b $ \(gs, t) -> do
  res <- checkGuards e gs
  case res of
    Nothing -> interpCase e bs
    Just e' -> interpTerm e' t

checkGuards :: Env -> [AGuard] -> IM (Maybe Env)
checkGuards e []     = return $ Just e
checkGuards e (g:gs) = do
  res <- checkGuard e g
  case res of
    Nothing -> return Nothing
    Just e' -> checkGuards e' gs

checkGuard :: Env -> AGuard -> IM (Maybe Env)
checkGuard e (AGIf (unembed -> t)) = do
  v <- interpTerm e t
  case v of
    VBool True -> return (Just e)
    _          -> return Nothing
checkGuard e (AGWhen (unembed -> t) p) = do
  -- XXX FIX ME!  Should be lazy, i.e. t should be evaluated only as
  -- much as demanded by the pattern.  Perhaps the easiest way is to
  -- switch to a small-step interpreter.
  v <- interpTerm e t
  matchPattern p v

matchPattern :: Pattern -> Value -> IM (Maybe Env)
matchPattern (PVar x) v = return $ Just (M.singleton (translate x) v)
matchPattern PWild     _ = ok
matchPattern PUnit     _ = ok
matchPattern (PBool b1) (VBool b2)
  | b1 == b2  = ok
matchPattern (PPair p1 p2) (VPair v1 v2) = do
  e1 <- matchPattern p1 v1
  e2 <- matchPattern p2 v2
  return $ M.union <$> e1 <*> e2
matchPattern (PInj s1 p) (VInj s2 v)
  | s1 == s2 = matchPattern p v
matchPattern (PNat n) (VNum r)
  | r == (n % 1) = ok
matchPattern (PSucc p) (VNum r) = matchPattern p (VNum (r-1))

matchPattern _ _ = return Nothing

ok :: IM (Maybe Env)
ok   = return $ Just M.empty

------------------------------------------------------------

prettyValue :: Value -> String
prettyValue VUnit     = "()"
prettyValue (VBool f) = map toLower (show f)
prettyValue (VClos _ _) = "<closure>"
prettyValue (VPair v1 v2) = "(" ++ prettyValue v1 ++ ", " ++ prettyValue v2 ++ ")"
prettyValue (VInj s v) = (case s of { L -> "inl "; R -> "inr " }) ++ prettyValue v
prettyValue (VNum r)
  | denominator r == 1 = show (numerator r)
  | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)

eval :: String -> IO ()
eval = putStrLn . either show (either show prettyValue . runIM . interpTermTop) . evalTCM . infer . parseTermStr

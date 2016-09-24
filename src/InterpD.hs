{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module InterpD where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                as M
import           Data.Ratio
import           Unbound.LocallyNameless

import           Desugar

data Value where
  VNum   :: Rational -> Value
  VCons  :: Int -> [Value] -> Value
  VClos  :: Bind AnyName Core -> Env -> Value
  VThunk :: Core -> Env -> Value
  deriving Show

type Env = M.Map AnyName Value

data InterpError where
  UnboundError  :: AnyName -> InterpError
  NotANum       :: Value   -> InterpError  -- ^ v should be a number, but isn't
  DivByZero     ::            InterpError
  NotABool      :: Value   -> InterpError  -- ^ should be a boolean, but isn't
  NotAFun       :: Value   -> InterpError
  NonExhaustive ::            InterpError
  deriving Show

-- | Interpreter monad.  Can throw InterpErrors, and generate fresh
--   names.
type IM = ReaderT Env (ExceptT InterpError LFreshM)

runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT . flip runReaderT M.empty

extend :: AnyName -> Value -> IM a -> IM a
extend x v = local (M.insert x v)

interpTerm :: Core -> IM Value
interpTerm (CVar x)     = do
  e <- ask
  maybe (throwError $ UnboundError x) return (M.lookup x e)
interpTerm (CCons i cs) = VCons i <$> mapM interpTerm cs
interpTerm (CNat n)     = return $ VNum (n % 1)
interpTerm (CAbs b)     = VClos b <$> ask
interpTerm (CApp str c1 c2) = do
  v1 <- interpTerm c1
  v2 <- case str of
    Strict -> interpTerm c2
    Lazy   -> VThunk c2 <$> ask
  interpApp v1 v2
interpTerm (COp op cs) = do
  vs <- mapM interpTerm cs
  interpOp op vs
interpTerm (CLet str b) =
  lunbind b $ \((x, unembed -> t1), t2) -> do
  v1 <- case str of
    Strict -> interpTerm t1
    Lazy   -> VThunk t1 <$> ask
  extend x v1 $ do
  interpTerm t2
interpTerm (CCase bs) = undefined

interpApp :: Value -> Value -> IM Value
interpApp (VClos c e) v   =
  lunbind c $ \(x,t) -> do
  local (const e)     $ do
  extend x v          $ do
  interpTerm  t
interpApp f _             = throwError $ NotAFun f

interpOp :: Op -> [Value] -> IM Value
interpOp = undefined

-- interpUOp :: UOp -> Value -> IM Value
-- interpUOp Neg (VNum n) = return $ VNum (-n)
-- interpUOp Neg v        = throwError $ NotANum v

-- interpBOp :: Type -> BOp -> Value -> Value -> IM Value
-- interpBOp _ Add     = numOp' (+)
-- interpBOp _ Sub     = numOp' (-)
-- interpBOp _ Mul     = numOp' (*)
-- interpBOp _ Div     = divOp
-- interpBOp _ Exp     = expOp
-- interpBOp ty Eq     = \v1 v2 -> return $ VBool (decideFor ty v1 v2)
-- interpBOp ty Neq    = \v1 v2 -> return $ VBool (not (decideFor ty v1 v2))
-- interpBOp _ Lt      = undefined
-- interpBOp _ Gt      = undefined
-- interpBOp _ Leq     = undefined
-- interpBOp _ Geq     = undefined
-- interpBOp _ And     = boolOp (&&)
-- interpBOp _ Or      = boolOp (||)
-- interpBOp _ Mod     = modOp
-- interpBOp _ Divides = numOp divides
-- interpBOp _ RelPm   = numOp relPm

-- numOp :: (Rational -> Rational -> Value) -> Value -> Value -> IM Value
-- numOp (#) (VNum x) (VNum y) = return $ x # y
-- numOp _   (VNum _) y        = throwError $ NotANum y
-- numOp _   x        _        = throwError $ NotANum x

-- numOp' :: (Rational -> Rational -> Rational) -> Value -> Value -> IM Value
-- numOp' f = numOp (\x y -> VNum (f x y))

-- divOp :: Value -> Value -> IM Value
-- divOp (VNum x) (VNum y)
--   | y == 0    = throwError DivByZero
--   | otherwise = return $ VNum (x / y)
-- divOp (VNum _) y = throwError $ NotANum y
-- divOp x        _ = throwError $ NotANum x

-- expOp :: Value -> Value -> IM Value
-- expOp (VNum x) (VNum y) = return $ VNum (x ^^ (numerator y))
--   -- if the program typechecks, y will be an integer
-- expOp (VNum _) y = throwError $ NotANum y
-- expOp x        _ = throwError $ NotANum x

-- modOp :: Value -> Value -> IM Value
-- modOp (VNum x) (VNum y)
--   | y == 0    = throwError DivByZero
--   | otherwise = return $ VNum ((numerator x `mod` numerator y) % 1)
--                 -- This is safe since if the program typechecks, mod will only ever be
--                 -- called on integral things.
-- modOp (VNum _) y = throwError $ NotANum y
-- modOp x        _ = throwError $ NotANum x

-- divides :: Rational -> Rational -> Value
-- divides 0 0 = VBool True
-- divides 0 _ = VBool False
-- divides x y = VBool $ denominator (y / x) == 1

-- relPm :: Rational -> Rational -> Value
-- relPm (numerator -> x) (numerator -> y) = VBool $ gcd x y == 1

-- boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> IM Value
-- boolOp (#) (VBool x) (VBool y) = return $ VBool (x # y)
-- boolOp _   (VBool _) y         = throwError $ NotABool y
-- boolOp _   x         _         = throwError $ NotABool x

-- decideFor :: Type -> Value -> Value -> Bool
-- decideFor (TyPair ty1 ty2) (VPair x1 y1) (VPair x2 y2)
--   = decideFor ty1 x1 x2 && decideFor ty2 y1 y2
-- decideFor (TySum ty1 _) (VInj L v1) (VInj L v2)
--   = decideFor ty1 v1 v2
-- decideFor (TySum _ ty2) (VInj R v1) (VInj R v2)
--   = decideFor ty2 v1 v2
-- decideFor (TyArr _ty1 _ty2) _c1@(VClos {}) _c2@(VClos {})
--   = undefined
--   -- = and (zipWith (decideFor ty2) (map f1 ty1s) (map f2 ty1s))
--   -- where
--   --   mkFun (VClos x t e) v = interpTerm (M.insert x v e) t
--   --   f1 = mkFun c1
--   --   f2 = mkFun c2
--   --   ty1s = enumerate ty1
-- decideFor _ v1 v2 = primValEq v1 v2

-- primValEq :: Value -> Value -> Bool
-- primValEq VUnit VUnit           = True
-- primValEq (VBool b1) (VBool b2) = b1 == b2
-- primValEq (VNum n1) (VNum n2)   = n1 == n2
-- primValEq _ _                   = False

-- enumerate :: Type -> [Value]
-- enumerate TyVoid           = []
-- enumerate TyUnit           = [VUnit]
-- enumerate TyBool           = [VBool False, VBool True]
-- enumerate (TyArr ty1 ty2)  = map (mkFun vs1) (sequence (vs2 <$ vs1))
--   where
--     vs1   = enumerate ty1
--     vs2   = enumerate ty2
--     mkFun _vs1 _vs2 = undefined
--       -- VFun $ \v -> snd . fromJust $ find (decideFor ty1 v . fst) (zip vs1 vs2)
-- enumerate (TyPair ty1 ty2) = [ VPair x y | x <- enumerate ty1, y <- enumerate ty2 ]
-- enumerate (TySum ty1 ty2)  = map (VInj L) (enumerate ty1) ++ map (VInj R) (enumerate ty2)
-- enumerate _                = []  -- other cases shouldn't happen if the program type checks

-- interpCase :: Env -> [ABranch] -> IM Value
-- interpCase _ []     = throwError NonExhaustive
-- interpCase e (b:bs) = do
--   lunbind b $ \(gs, t) -> do
--   res <- checkGuards e gs
--   case res of
--     Nothing -> interpCase e bs
--     Just e' -> interpTerm e' t

-- checkGuards :: Env -> [AGuard] -> IM (Maybe Env)
-- checkGuards e []     = return $ Just e
-- checkGuards e (g:gs) = do
--   res <- checkGuard e g
--   case res of
--     Nothing -> return Nothing
--     Just e' -> checkGuards e' gs

-- checkGuard :: Env -> AGuard -> IM (Maybe Env)
-- checkGuard e (AGIf (unembed -> t)) = do
--   v <- interpTerm e t
--   case v of
--     VBool True -> return (Just e)
--     _          -> return Nothing
-- checkGuard e (AGWhen (unembed -> t) p) = do
--   -- XXX FIX ME!  Should be lazy, i.e. t should be evaluated only as
--   -- much as demanded by the pattern.  Perhaps the easiest way is to
--   -- switch to a small-step interpreter.
--   v <- interpTerm e t
--   matchPattern p v

-- matchPattern :: Pattern -> Value -> IM (Maybe Env)
-- matchPattern (PVar x) v = return $ Just (M.singleton (translate x) v)
-- matchPattern PWild     _ = ok
-- matchPattern PUnit     _ = ok
-- matchPattern (PBool b1) (VBool b2)
--   | b1 == b2  = ok
-- matchPattern (PPair p1 p2) (VPair v1 v2) = do
--   e1 <- matchPattern p1 v1
--   e2 <- matchPattern p2 v2
--   return $ M.union <$> e1 <*> e2
-- matchPattern (PInj s1 p) (VInj s2 v)
--   | s1 == s2 = matchPattern p v
-- matchPattern (PNat n) (VNum r)
--   | r == (n % 1) = ok
-- matchPattern (PSucc p) (VNum r) = matchPattern p (VNum (r-1))

-- matchPattern _ _ = return Nothing

-- ok :: IM (Maybe Env)
-- ok   = return $ Just M.empty

-- ------------------------------------------------------------

-- prettyValue :: Value -> String
-- prettyValue VUnit     = "()"
-- prettyValue (VBool f) = map toLower (show f)
-- prettyValue (VClos _ _) = "<closure>"
-- prettyValue (VPair v1 v2) = "(" ++ prettyValue v1 ++ ", " ++ prettyValue v2 ++ ")"
-- prettyValue (VInj s v) = (case s of { L -> "inl "; R -> "inr " }) ++ prettyValue v
-- prettyValue (VNum r)
--   | denominator r == 1 = show (numerator r)
--   | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)

-- eval :: String -> IO ()
-- eval = putStrLn . either show (either show prettyValue . runIM . interpTermTop) . evalTCM . infer . parseTermStr

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
  NonExhaustive ::            InterpError
  deriving Show

-- | Interpreter monad.  Can throw InterpErrors, and generate fresh
--   names.
type IM = ReaderT Env (ExceptT InterpError LFreshM)

runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT . flip runReaderT M.empty

extend :: AnyName -> Value -> IM a -> IM a
extend x v = local (M.insert x v)

mkThunk :: Core -> IM Value
mkThunk c = VThunk c <$> ask

mkEnum :: Enum e => e -> Value
mkEnum e = VCons (fromEnum e) []

-- | Reduce a value to weak head normal form.
whnf' :: Value -> IM Value
whnf' (VThunk c e) = local (const e) $ whnf c
whnf' v            = return v

-- | Reduce a Core expression to weak head normal form.
whnf :: Core -> IM Value
whnf (CVar x) = do
  e <- ask
  v <- maybe (throwError $ UnboundError x) return (M.lookup x e)
  whnf' v
whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)
whnf (CNat n)       = return $ VNum (n % 1)
whnf (CAbs b)       = VClos b <$> ask
whnf (CApp str c1 c2) = do
  v1 <- whnf c1
  v2 <- case str of
    Strict -> whnf c2       -- for types with strict evaluation, whnf = full reduction
    Lazy   -> mkThunk c2
  whnfApp v1 v2
whnf (COp op cs)    = whnfOp op cs
whnf (CLet str b)     =
  lunbind b $ \((x, unembed -> t1), t2) -> do
  v1 <- case str of
    Strict -> whnf t1
    Lazy   -> mkThunk t1
  extend x v1 $ do
  whnf t2
whnf (CCase bs)     = undefined

-- | Reduce an application to WHNF.  Precondition: the first argument
--   has already been reduced to WHNF (which means it must be a
--   closure).
whnfApp :: Value -> Value -> IM Value
whnfApp (VClos c e) v =
  lunbind c $ \(x,t) -> do
  local (const e)     $ do
  extend x v          $ do
  whnf t
whnfApp f _ = error "Impossible! First argument to whnfApp is not a closure."

-- | Reduce an operator application to WHNF.
whnfOp :: Op -> [Core] -> IM Value
whnfOp OAdd     = numOp (+)
whnfOp ONeg     = uNumOp negate
whnfOp OMul     = numOp (*)
whnfOp ODiv     = numOp' divOp
whnfOp OExp     = numOp (\m n -> m ^^ numerator n)
  -- If the program typechecks, n will be an integer.
whnfOp OAnd     = boolOp (&&)
whnfOp OOr      = boolOp (||)
whnfOp OMod     = numOp' modOp
whnfOp ODivides = numOp' divides
whnfOp ORelPm   = numOp' relPm
whnfOp (OEq ty) = undefined
whnfOp (OLt ty) = undefined
whnfOp ONot     = notOp

numOp :: (Rational -> Rational -> Rational) -> [Core] -> IM Value
numOp (#) = numOp' (\m n -> return (VNum (m # n)))

numOp' :: (Rational -> Rational -> IM Value) -> [Core] -> IM Value
numOp' (#) cs = do
  [VNum m, VNum n] <- mapM whnf cs     -- If the program type checked this can
  m # n                                -- never go wrong.

uNumOp :: (Rational -> Rational) -> [Core] -> IM Value
uNumOp f [c] = do
  VNum m <- whnf c
  return $ VNum (f m)

divOp :: Rational -> Rational -> IM Value
divOp _ 0 = throwError DivByZero
divOp m n = return $ VNum (m / n)

modOp :: Rational -> Rational -> IM Value
modOp m n
  | n == 0    = throwError DivByZero
  | otherwise = return $ VNum ((numerator m `mod` numerator n) % 1)
                -- This is safe since if the program typechecks, mod will only ever be
                -- called on integral things.

boolOp :: (Bool -> Bool -> Bool) -> [Core] -> IM Value
boolOp (#) cs = do
  [VCons i [], VCons j []] <- mapM whnf cs
  return . mkEnum $ toEnum i # toEnum j

divides :: Rational -> Rational -> IM Value
divides 0 0 = return $ mkEnum True
divides 0 _ = return $ mkEnum False
divides x y = return . mkEnum $ denominator (y / x) == 1

relPm :: Rational -> Rational -> IM Value
relPm (numerator -> x) (numerator -> y) = return . mkEnum $ gcd x y == 1

notOp :: [Core] -> IM Value
notOp [c] = do
  VCons i [] <- whnf c
  return . mkEnum . not . toEnum $ i

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

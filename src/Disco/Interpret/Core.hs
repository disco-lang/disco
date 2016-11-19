{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

module Disco.Interpret.Core where

import           Debug.Trace

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char               (toLower)
import           Data.List               (find)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)
import           Data.Ratio
import           Unbound.LocallyNameless hiding (enumerate, rnf)

import           Disco.Desugar
import           Disco.Parser            (parseTermStr)
import           Disco.Typecheck         (evalTCM, getType, infer)
import           Disco.Types

data Value where
  VNum   :: Rational -> Value
  VCons  :: Int -> [Value] -> Value
  VClos  :: Bind (Name Value) Core -> Env -> Value
  VThunk :: Core -> Env -> Value
  VFun   :: (Value -> Value) -> Value

instance Show Value where
  show _ = "<value>"

type Env = M.Map (Name Value) Value

derive [''Value]

data InterpError where
  UnboundError  :: Name Core -> InterpError
  NotANum       :: Value     -> InterpError  -- ^ v should be a number, but isn't
  DivByZero     ::              InterpError
  NotABool      :: Value     -> InterpError  -- ^ should be a boolean, but isn't
  NonExhaustive ::              InterpError
  Unimplemented :: String    -> InterpError
  deriving Show

-- | Interpreter monad.  Can throw InterpErrors, and generate fresh
--   names.
type IM = ReaderT Env (ExceptT InterpError LFreshM)

runIM :: IM a -> Either InterpError a
runIM = runLFreshM . runExceptT . flip runReaderT M.empty

emptyEnv :: Env
emptyEnv = M.empty

extend :: Name Value -> Value -> IM a -> IM a
extend x v = local (M.insert x v)

extends :: Env -> IM a -> IM a
extends e' = local (M.union e')

mkThunk :: Core -> IM Value
mkThunk c = VThunk c <$> ask

mkEnum :: Enum e => e -> Value
mkEnum e = VCons (fromEnum e) []

rnf :: Core -> IM Value
rnf c = mkThunk c >>= rnfV

rnfV :: Value -> IM Value
rnfV v = rnfV' v
rnfV' (VCons i vs)   = VCons i <$> mapM rnfV vs
rnfV' v@(VThunk _ _) = whnfV v >>= rnfV
rnfV' v              = return v

-- | Reduce a value to weak head normal form.
whnfV :: Value -> IM Value
whnfV v@(VThunk c e) = local (const e) $ whnf c
whnfV v              = return v

-- | Reduce a Core expression to weak head normal form.
whnf :: Core -> IM Value
whnf (CVar x) = do
  e <- ask
  v <- maybe (throwError $ UnboundError x) return (M.lookup (translate x) e)
  whnfV v
whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)
whnf (CNat n)       = return $ VNum (n % 1)
whnf (CAbs b)       = lunbind b $ \(x,t) -> VClos (bind (translate x) t) <$> ask
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
  extend (translate x) v1 $ whnf t2
whnf (CCase bs)     = whnfCase bs

-- | Reduce an application to WHNF.  Precondition: the first argument
--   has already been reduced to WHNF (which means it must be a
--   closure).
whnfApp :: Value -> Value -> IM Value
whnfApp (VClos c e) v =
  lunbind c $ \(x,t) -> do
  local (const e)     $ do
  extend x v          $ do
  whnf t
whnfApp (VFun f) v = whnfV (f v)
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
whnfOp (OEq ty) = eqOp ty
whnfOp (OLt ty) = ltOp ty
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

eqOp :: Type -> [Core] -> IM Value
eqOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  mkEnum <$> decideEqFor ty v1 v2

decideEqFor :: Type -> Value -> Value -> IM Bool
decideEqFor (TyPair ty1 ty2) v1 v2 = do
  VCons 0 [v11, v12] <- whnfV v1
  VCons 0 [v21, v22] <- whnfV v2
  b1 <- decideEqFor ty1 v11 v21
  case b1 of
    False -> return False
    True  -> decideEqFor ty2 v12 v22
decideEqFor (TySum ty1 ty2) v1 v2 = do
  VCons i1 [v1'] <- whnfV v1
  VCons i2 [v2'] <- whnfV v2
  case i1 == i2 of
    False -> return False
    True  -> decideEqFor ([ty1, ty2] !! i1) v1' v2'
decideEqFor (TyArr ty1 ty2) v1 v2 = do
  clos1 <- whnfV v1
  clos2 <- whnfV v2
  let ty1s = enumerate ty1
  decideEqForAll ty2 clos1 clos2 ty1s
decideEqFor _ v1 v2 = primValEq <$> whnfV v1 <*> whnfV v2

decideEqForAll :: Type -> Value -> Value -> [Value] -> IM Bool
decideEqForAll ty2 clos1 clos2 vs = go vs
  where
    go []     = return True
    go (v:vs) = do
      r1 <- whnfApp clos1 v
      r2 <- whnfApp clos2 v
      b  <- decideEqFor ty2 r1 r2
      case b of
        False -> return False
        True  -> go vs

primValEq :: Value -> Value -> Bool
primValEq (VCons i []) (VCons j []) = i == j
primValEq (VNum n1)    (VNum n2)    = n1 == n2
primValEq _ _                       = False

decideEqForRnf :: Type -> Value -> Value -> Bool
decideEqForRnf (TyPair ty1 ty2) (VCons 0 [v11, v12]) (VCons 0 [v21, v22])
  = decideEqForRnf ty1 v11 v21 && decideEqForRnf ty2 v12 v22
decideEqForRnf (TySum ty1 ty2) (VCons i1 [v1']) (VCons i2 [v2'])
  = i1 == i2 && decideEqForRnf ([ty1, ty2] !! i1) v1' v2'
decideEqForRnf (TyArr ty1 ty2) (VFun f1) (VFun f2)
  = all (\v -> decideEqForRnf ty2 (f1 v) (f2 v)) (enumerate ty1)

-- XXX once we have lists/sets, create a way to get access to
-- enumerations via surface syntax.
enumerate :: Type -> [Value]
enumerate TyVoid           = []
enumerate TyUnit           = [VCons 0 []]
enumerate TyBool           = [VCons 0 [], VCons 1 []]
enumerate (TyPair ty1 ty2) = [VCons 0 [x, y] | x <- enumerate ty1, y <- enumerate ty2]
enumerate (TySum ty1 ty2)  =
  map (VCons 0 . (:[])) (enumerate ty1) ++
  map (VCons 1 . (:[])) (enumerate ty2)
enumerate (TyArr ty1 ty2)  = map (mkFun vs1) (sequence (vs2 <$ vs1))
  where
    vs1 = enumerate ty1
    vs2 = enumerate ty2
    mkFun :: [Value] -> [Value] -> Value
    mkFun vs1 vs2
      = VFun $ \v -> snd . fromJust . find (decideEqForRnf ty1 v . fst) $ zip vs1 vs2
enumerate _ = []  -- other cases shouldn't happen if the program type checks

ltOp :: Type -> [Core] -> IM Value
ltOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  (mkEnum . (==LT)) <$> decideOrdFor ty v1 v2

decideOrdFor :: Type -> Value -> Value -> IM Ordering
decideOrdFor (TyPair ty1 ty2) v1 v2 = do
  VCons 0 [v11, v12] <- whnfV v1
  VCons 0 [v21, v22] <- whnfV v2
  o1 <- decideOrdFor ty1 v11 v21
  case o1 of
    EQ -> decideOrdFor ty2 v12 v22
    _  -> return o1
decideOrdFor (TySum ty1 ty2) v1 v2 = do
  VCons i1 [v1'] <- whnfV v1
  VCons i2 [v2'] <- whnfV v2
  case compare i1 i2 of
    EQ -> decideOrdFor ([ty1, ty2] !! i1) v1' v2'
    o  -> return o
decideOrdFor (TyArr ty1 ty2) v1 v2 = do
  clos1 <- whnfV v1
  clos2 <- whnfV v2
  let ty1s = enumerate ty1
  decideOrdForLex ty2 clos1 clos2 ty1s
decideOrdFor _ v1 v2 = primValOrd <$> whnfV v1 <*> whnfV v2

decideOrdForLex :: Type -> Value -> Value -> [Value] -> IM Ordering
decideOrdForLex ty2 clos1 clos2 vs = go vs
  where
    go []     = return EQ
    go (v:vs) = do
      r1 <- whnfApp clos1 v
      r2 <- whnfApp clos2 v
      o  <- decideOrdFor ty2 r1 r2
      case o of
        EQ -> go vs
        _  -> return o

primValOrd :: Value -> Value -> Ordering
primValOrd (VCons i []) (VCons j []) = compare i j
primValOrd (VNum n1) (VNum n2)       = compare n1 n2
primValOrd _ _                       = error "primValOrd: impossible!"

whnfCase :: [CBranch] -> IM Value
whnfCase []     = throwError NonExhaustive
whnfCase (b:bs) = do
  lunbind b $ \(gs, t) -> do
  res <- checkGuards gs
  case res of
    Nothing -> whnfCase bs
    Just e' -> extends e' $ whnf t

checkGuards :: CGuards -> IM (Maybe Env)
checkGuards CGEmpty = ok
checkGuards (CGCons (unrebind -> ((unembed -> c, p), gs))) = do
  v <- mkThunk c
  res <- match v p
  case res of
    Nothing -> return Nothing
    Just e  -> extends e (fmap (M.union e) <$> checkGuards gs)

match :: Value -> CPattern -> IM (Maybe Env)
match v (CPVar x)     = return $ Just (M.singleton (translate x) v)
match _ CPWild        = ok
match v (CPCons i ps) = do
  VCons j vs <- whnfV v
  case i == j of
    False -> noMatch
    True  -> do
      res <- sequence <$> zipWithM match vs ps
      case res of
        Nothing -> noMatch
        Just es -> return $ Just (M.unions es)
match v (CPNat n)     = do
  VNum m <- whnfV v
  case m == n % 1 of
    False -> noMatch
    True  -> ok
match v (CPSucc p) = do
  VNum n <- whnfV v
  case n > 0 of
    True  -> match (VNum (n-1)) p
    False -> return Nothing

ok :: IM (Maybe Env)
ok = return $ Just M.empty

noMatch :: IM (Maybe Env)
noMatch = return Nothing

-- ------------------------------------------------------------

prettyValue :: Type -> Value -> String
prettyValue TyUnit (VCons 0 []) = "()"
prettyValue TyBool (VCons i []) = map toLower (show (toEnum i :: Bool))
prettyValue _ (VClos _ _)       = "<closure>"
prettyValue _ (VThunk _ _)      = "<thunk>"
prettyValue (TyPair ty1 ty2) (VCons 0 [v1, v2])
  = "(" ++ prettyValue ty1 v1 ++ ", " ++ prettyValue ty2 v2 ++ ")"
prettyValue (TySum ty1 ty2) (VCons i [v])
  = case i of
      0 -> "inl " ++ prettyValue ty1 v
      1 -> "inr " ++ prettyValue ty2 v
prettyValue _ (VNum r)
  | denominator r               == 1 = show (numerator r)
  | otherwise                   = show (numerator r) ++ "/" ++ show (denominator r)


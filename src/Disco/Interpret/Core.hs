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

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interpret.Core
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- A big-step interpreter for the desugared, untyped Disco core
-- language.
--
-----------------------------------------------------------------------------

module Disco.Interpret.Core
       (
         -- * Values
         Value(..), ValFun(..)

         -- * Interpreter monad
       , Env, DefEnv
       , InterpError(..)
       , IM, runIM, runIM'
       , emptyEnv, extend, extends, getEnv, withEnv, getDefEnv

         -- * Evaluation
       , mkThunk
       , mkEnum

         -- ** Full reduction
       , rnf, rnfV

         -- ** Weak head reduction
       , whnf, whnfV, whnfApp

         -- ** Case analysis and pattern-matching
       , whnfCase, checkGuards, match, ok, noMatch

         -- ** Operations
       , whnfOp, numOp, numOp', uNumOp, divOp, modOp, boolOp
       , divides, relPm, binom, fact, notOp

         -- * Equality testing
       , eqOp, primValEq, enumerate
       , decideEqFor, decideEqForRnf, decideEqForClosures

         -- * Comparison testing
       , ltOp, primValOrd, decideOrdFor, decideOrdForClosures

       )
       where

import           Control.Lens                       ((%~), (.~), _1)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Coerce
import           Data.List                          (find)
import qualified Data.Map                           as M
import           Data.Monoid
import           Data.Ratio

import           Unbound.Generics.LocallyNameless

import           Math.Combinatorics.Exact.Binomial  (choose)
import           Math.Combinatorics.Exact.Factorial (factorial)

import           Math.NumberTheory.Logarithms       (integerLog2)
import           Math.NumberTheory.Moduli           (invertMod, powerModInteger)

import           Disco.AST.Surface (Ellipsis(..))
import           Disco.AST.Core
import           Disco.Types

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | The type of values produced by the interpreter.
data Value where
  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum   :: RationalDisplay -> Rational -> Value

  -- | A constructor with arguments.  The Int indicates which
  --   constructor it is.  For example, False is represented by
  --   @VCons 0 []@, and True by @VCons 1 []@.  A pair is
  --   represented by @VCons 0 [v1, v2]@, and @inr v@ by @VCons 1
  --   [v]@.
  VCons  :: Int -> [Value] -> Value

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClos  :: Bind (Name Core) Core -> Env -> Value

  -- | A thunk, i.e. an unevaluated core expression together with
  --   its environment.
  VThunk :: Core -> Env -> Value

  -- | A literal function value.  @VFun@ is only used when
  --   enumerating function values in order to decide comparisons at
  --   higher-order function types.  For example, in order to
  --   compare two values of type @(Bool -> Bool) -> Bool@ for
  --   equality, we have to enumerate all functions of type @Bool ->
  --   Bool@ as @VFun@ values.
  --
  --   We assume that all @VFun@ values are /strict/, that is, their
  --   arguments should be fully evaluated to RNF before being
  --   passed to the function.
  VFun   :: ValFun -> Value

  -- | A delayed value, containing an @IM Value@ computation which can
  --   be run later.
  VDelay  :: ValDelay -> Value
  deriving Show

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

-- | A @ValDelay@ is just an @IM Value@ computation.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValDelay = ValDelay (IM Value)

instance Show ValDelay where
  show _ = "<delay>"

-- | Delay an @IM Value@ computation by packaging it into a @VDelay@
--   constructor.  When it is evaluated later, it will be run with the
--   environment that was current at the time 'delay' was called,
--   /not/ the one that is in effect later.
delay :: IM Value -> IM Value
delay imv = do
  e <- getEnv
  return (VDelay . ValDelay $ withEnv e imv)

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
vnum :: Rational -> Value
vnum = VNum mempty

------------------------------------------------------------
-- Environments & errors
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = M.Map (Name Core) Value

-- | A definition environment is a mapping fron names to core terms.
type DefEnv = M.Map (Name Core) Core

-- | Errors that can be generated during interpreting.
data InterpError where

  -- | An unbound name.
  UnboundError  :: Name Core -> InterpError

  -- | v should be a number, but isn't.
  NotANum       :: Value     -> InterpError

  -- | Division by zero.
  DivByZero     ::              InterpError

  -- | Taking the base-2 logarithm of zero.
  LgOfZero      ::              InterpError

  -- | v should be a boolean, but isn't.
  NotABool      :: Value     -> InterpError

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              InterpError

  -- | Internal error for features not yet implemented.
  Unimplemented :: String    -> InterpError

  deriving Show

------------------------------------------------------------
-- Interpreter monad
------------------------------------------------------------

-- | The interpreter monad.  Combines read-only access to an
--   environment, and the ability to throw @InterpErrors@ and generate
--   fresh names.
type IM = ReaderT (Env, DefEnv) (ExceptT InterpError LFreshM)

-- | Run a computation in the @IM@ monad, starting in the empty
--   environment.
runIM :: IM a -> Either InterpError a
runIM = runIM' M.empty

-- | Run a computation in the @IM@ monad, starting in a given
--   environment of definitions.
runIM' :: DefEnv -> IM a -> Either InterpError a
runIM' cenv = runLFreshM . runExceptT . flip runReaderT (M.empty, cenv)

-- | The empty environment.
emptyEnv :: (Env, DefEnv)
emptyEnv = (M.empty, M.empty)

-- | Locally extend the environment with a new name -> value mapping,
--   (shadowing any existing binding for the given name).
extend :: Name Core -> Value -> IM a -> IM a
extend x v = avoid [AnyName x] . local (_1 %~ M.insert x v)

-- | Locally extend the environment with another environment.
--   Bindings in the new environment shadow bindings in the old.
extends :: Env -> IM a -> IM a
extends e' = avoid (map AnyName (M.keys e')) . local (_1 %~ M.union e')

-- | Get the current environment.
getEnv :: IM Env
getEnv = fst <$> ask

-- | Run an @IM@ computation with a /replaced/ (not extended)
--   environment.  This is used for evaluating things such as closures
--   and thunks that come with their own environment.
withEnv :: Env -> IM a -> IM a
withEnv e = local (_1 .~ e)

-- | Get the current definition environment.
getDefEnv :: IM DefEnv
getDefEnv = snd <$> ask

------------------------------------------------------------
-- Evaluation
------------------------------------------------------------

-- | Create a thunk by packaging up a @Core@ expression with the
--   current environment.
mkThunk :: Core -> IM Value
mkThunk c = VThunk c <$> getEnv

-- | Turn any instance of @Enum@ into a @Value@, by creating a
--   constructor with an index corresponding to the enum value.
mkEnum :: Enum e => e -> Value
mkEnum e = VCons (fromEnum e) []

-- | Evaluate a Core expression to reduced normal form.
rnf :: Core -> IM Value
rnf c = mkThunk c >>= rnfV

-- | Reduce a value to reduced normal form.
rnfV :: Value -> IM Value
rnfV (VCons i vs)   = VCons i <$> mapM rnfV vs
rnfV v@(VThunk _ _) = whnfV v >>= rnfV
rnfV v@(VDelay _)   = whnfV v >>= rnfV
rnfV v              = return v

-- | Reduce a value to weak head normal form.
whnfV :: Value -> IM Value
whnfV (VThunk c e)            = withEnv e $ whnf c
whnfV (VDelay (ValDelay imv)) = imv >>= whnfV
whnfV v                       = return v

-- | Reduce a Core expression to weak head normal form.
whnf :: Core -> IM Value
whnf (CVar x) = do
  e <- getEnv
  case (M.lookup (coerce x) e) of
    Just v  -> whnfV v
    Nothing -> do
      c <- getDefEnv
      case (M.lookup x c) of
        Just core -> whnf core
        Nothing   -> throwError $ UnboundError x

whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)
whnf (CNum d n)     = return $ VNum d n
whnf (CAbs b)       = VClos b <$> getEnv
whnf (CApp str c1 c2) = do
  v1 <- whnf c1
  v2 <- case str of
    Strict -> whnf c2       -- for types with strict evaluation, whnf = full reduction
    Lazy   -> mkThunk c2
  whnfApp v1 v2
whnf (COp op cs)    = whnfOp op cs
whnf (CLet str b)   =
  lunbind b $ \((x, unembed -> t1), t2) -> do
  v1 <- case str of
    Strict -> whnf t1
    Lazy   -> mkThunk t1
  extend (coerce x) v1 $ whnf t2
whnf (CCase bs)     = whnfCase bs
whnf (CListComp b)  =
  lunbind b $ \(qs, t) -> do
    lcmp <- expandComp t qs
    whnfV lcmp
whnf (CEllipsis ts ell) = expandEllipsis ts ell
whnf (CType _)      = error "Called whnf on CType"

-- | Reduce an application to weak head normal form (WHNF).
--   Precondition: the first argument has already been reduced to WHNF
--   (which means it must be a closure, or a @VFun@).
whnfApp :: Value -> Value -> IM Value
whnfApp (VClos c e) v =
  lunbind c $ \(x,t) -> do
  withEnv e           $ do
  extend x v          $ do
  whnf t
whnfApp (VFun (ValFun f)) v = rnfV v >>= \v' -> whnfV (f v')
whnfApp _ _ = error "Impossible! First argument to whnfApp is not a closure."

------------------------------------------------------------
-- Lists
------------------------------------------------------------

--------------------------------------------------
-- Utilities

-- | A lazy foldr on lists represented as 'Value's.  The entire
--   function is wrapped in a call to 'delay', so it does not actually
--   reduce the list argument to whnf until (potentially) forced by a
--   call to 'whnf' later.  That is, executing the resulting @IM
--   Value@ just results in a 'VDelay'; forcing that @VDelay@ with
--   'whnf' will then cause the foldr to actually start executing.
vfoldr :: (Value -> Value -> IM Value) -> Value -> Value -> IM Value
vfoldr f z xs = delay $ do
  xs' <- whnfV xs
  case xs' of
    VCons 0 []      -> return z
    VCons 1 [vh,vt] -> do
      r <- vfoldr f z vt
      f vh r
    _               -> error $ "Impossible! Got " ++ show xs' ++ " in vfoldr"

-- | Lazy append on 'Value' lists, implemented via 'vfoldr'.
vappend :: Value -> Value -> IM Value
vappend xs ys = vfoldr (\h t -> return $ VCons 1 [h,t]) ys xs

-- | Lazy concat on 'Value' lists, implemented via 'vfoldr'.
vconcat :: Value -> IM Value
vconcat = vfoldr vappend (VCons 0 [])

-- | Lazy map on 'Value' lists, implemented via 'vfoldr'.
vmap :: (Value -> IM Value) -> Value -> IM Value
vmap f = vfoldr (\h t -> f h >>= \h' -> return $ VCons 1 [h', t]) (VCons 0 [])

--------------------------------------------------
-- List comprehensions

-- | Expand a list comprehension to a lazy 'Value' list.
expandComp :: Core -> CQuals -> IM Value

-- [ t | ] = [ t ]
expandComp t CQEmpty = do
  c <- mkThunk t
  return $ VCons 1 [c, VCons 0 []]

-- [ t | q, qs ] = ...
expandComp t (CQCons (unrebind -> (q,qs))) = do
  case q of

    -- [ t | x in l, qs ] = concat (map (\x -> [t | qs]) l)
    CQBind x (unembed -> lst) -> do
      c <- mkThunk lst
      vmap (\v -> extend x v $ expandComp t qs) c >>= vconcat

    -- [ t | b, qs ] = if b then [ t | qs ] else []
    CQGuard (unembed -> g)    -> do
      v <- whnf g
      case v of
        VCons 0 [] {- False -} -> return $ VCons 0 []  {- Nil -}
        VCons 1 [] {- True  -} -> expandComp t qs
        _ -> error $ "Impossible! Got " ++ show v ++ " in expandComp CQGuard."

--------------------------------------------------
-- Polynomial sequences [a,b,c,d .. e]

-- Note, in order to reduce an ellipsis with an end value to WHNF we
-- have to evaluate ALL the elements first, because of examples like
--
--   [1, 3, 5, 7 .. 0] = []
--
-- The above example shows that we can't just lazily output the first
-- few elements and deal with the ellipsis when we get to it.  We have
-- to evaluate all the elements even to know whether the list is empty
-- or not.
--
-- However, when the end value is omitted, the resulting list will
-- always begin with the listed values before the '..', so we can
-- output them lazily, and evaluate them only when we need them to
-- compute the rest of the values.

expandEllipsis :: [Core] -> Ellipsis Core -> IM Value
expandEllipsis cs ell = do
  vs  <- mapM whnf cs
  end <- traverse whnf ell
  let (ds,rs)   = unzip (map fromVNum vs)
      (d, end') = traverse fromVNum end
  return . toDiscoList . map (VNum (mconcat ds <> d)) $ enumEllipsis rs end'
  where
    fromVNum (VNum d r) = (d,r)
    fromVNum v          = error $ "Impossible!  fromVNum on " ++ show v

enumEllipsis :: (Enum a, Num a, Ord a) => [a] -> Ellipsis a -> [a]
enumEllipsis [] _          = error "Impossible! Disco.Interpret.Core.enumEllipsis []"
enumEllipsis [x] Forever   = [x ..]
enumEllipsis [x] (Until y)
  | x <= y    = [x .. y]
  | otherwise = [x, pred x .. y]
enumEllipsis xs Forever    = babbage xs
enumEllipsis xs (Until y)
  | d > 0     = takeWhile (<= y) nums
  | d < 0     = takeWhile (>= y) nums
  | otherwise = nums
  where
    d    = constdiff xs
    nums = babbage xs

-- | Extend a sequence infinitely by interpolating it as a polynomial
--   sequence, via forward differences.  Essentially the same
--   algorithm used by Babbage's famous Difference Engine.
babbage :: Num a => [a] -> [a]
babbage []     = []
babbage [x]    = repeat x
babbage (x:xs) = scanl (+) x (babbage (diff (x:xs)))

-- | Compute the forward difference of the given sequence, that is,
--   differences of consecutive pairs of elements.
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

-- | Take forward differences until the result is constant, and return
--   the constant.  The sign of the constant difference tells us the
--   limiting behavior of the sequence.
constdiff :: (Eq a, Num a) => [a] -> a
constdiff [] = error "Impossible! Disco.Interpret.Core.constdiff []"
constdiff (x:xs)
  | all (==x) xs = x
  | otherwise    = constdiff (diff (x:xs))

------------------------------------------------------------
-- Case analysis
------------------------------------------------------------

-- | Reduce a case expression to weak head normal form.
whnfCase :: [CBranch] -> IM Value
whnfCase []     = throwError NonExhaustive
whnfCase (b:bs) = do
  lunbind b $ \(gs, t) -> do
  res <- checkGuards gs
  case res of
    Nothing -> whnfCase bs
    Just e' -> extends e' $ whnf t

-- | Check a chain of guards on one branch of a case.  Returns
--   @Nothing@ if the guards fail to match, or a resulting environment
--   of bindings if they do match.
checkGuards :: CGuards -> IM (Maybe Env)
checkGuards CGEmpty = ok
checkGuards (CGCons (unrebind -> ((unembed -> c, p), gs))) = do
  v <- mkThunk c
  res <- match v p
  case res of
    Nothing -> return Nothing
    Just e  -> extends e (fmap (M.union e) <$> checkGuards gs)

-- | Match a value against a pattern, returning an environment of
--   bindings if the match succeeds.
match :: Value -> CPattern -> IM (Maybe Env)
match v (CPVar x)     = return $ Just (M.singleton (coerce x) v)
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
  VNum _ m <- whnfV v
  case m == n % 1 of
    False -> noMatch
    True  -> ok
match v (CPSucc p) = do
  VNum _ n <- whnfV v
  case n > 0 of
    True  -> match (vnum (n-1)) p
    False -> noMatch

-- | Convenience function: successfully match with no bindings.
ok :: IM (Maybe Env)
ok = return $ Just M.empty

-- | Convenience function: fail to match.
noMatch :: IM (Maybe Env)
noMatch = return Nothing

------------------------------------------------------------
-- Operator evaluation
------------------------------------------------------------

-- | Reduce an operator application to WHNF.
whnfOp :: Op -> [Core] -> IM Value
whnfOp OAdd     = numOp (+)
whnfOp ONeg     = uNumOp negate
whnfOp OSqrt    = uNumOp integerSqrt
whnfOp OLg      = lgOp
whnfOp OFloor   = uNumOp floorOp
whnfOp OCeil    = uNumOp ceilOp
whnfOp OAbs     = uNumOp abs
whnfOp OMul     = numOp (*)
whnfOp ODiv     = numOp' divOp
whnfOp OExp     = numOp (\m n -> m ^^ numerator n)
  -- If the program typechecks, n will be an integer.
whnfOp OAnd     = boolOp (&&)
whnfOp OOr      = boolOp (||)
whnfOp OMod     = numOp' modOp
whnfOp ODivides = numOp' divides
whnfOp ORelPm   = numOp' relPm
whnfOp OBinom   = numOp binom
whnfOp OMultinom = multinomOp
whnfOp OFact    = uNumOp fact
whnfOp (OEq ty) = eqOp ty
whnfOp (OLt ty) = ltOp ty
whnfOp ONot     = notOp
whnfOp OEnum    = enumOp
whnfOp OCount   = countOp
-- Modular operations, for finite types
whnfOp (OMAdd n) = modArithBin (+) n
whnfOp (OMMul n) = modArithBin (*) n
whnfOp (OMSub n) = modArithBin (-) n
whnfOp (OMNeg n) = modArithUn negate n
whnfOp (OMDiv n) = modDiv n
whnfOp (OMExp n) = modExp n

-- | Perform a numeric binary operation.
numOp :: (Rational -> Rational -> Rational) -> [Core] -> IM Value
numOp (#) = numOp' (\m n -> return (vnum (m # n)))

-- | A more general version of 'numOp' where the binary operation has
--   a result in the @IM@ monad (/e.g./ for operations which can throw
--   a division by zero error).
numOp' :: (Rational -> Rational -> IM Value) -> [Core] -> IM Value
numOp' (#) cs = do
  [VNum d1 m, VNum d2 n] <- mapM whnf cs     -- If the program type checked this can
  res <- m # n                               -- never go wrong.
  case res of
    VNum _ r -> return $ VNum (d1 <> d2) r   -- Re-flag any resulting numeric value with
    _        -> return res                   --   the combination of the input flags.

-- | Perform a numeric unary operation.
uNumOp :: (Rational -> Rational) -> [Core] -> IM Value
uNumOp f [c] = do
  VNum d m <- whnf c
  return $ VNum d (f m)
uNumOp _ _ = error "Impossible! Second argument to uNumOp has length /= 1"

-- | For performing a modular unary operation within a finite type
modArithUn :: (Rational -> Rational) -> Integer -> [Core] -> IM Value
modArithUn op n [c] = do
  VNum _ r <- whnf c
  modOp (op r) (n % 1)
modArithUn _ _ _ = error "Impossible! modArithUn error (too many Cores)"

-- | For performing a modular binary operation within a finite type.
modArithBin :: (Rational -> Rational -> Rational) -> Integer -> [Core] -> IM Value
modArithBin op n [c1,c2] = do
  VNum _ r1 <- whnf c1
  VNum _ r2 <- whnf c2
  modOp (op r1 r2) (n % 1)
modArithBin _ _ _ = error "Impossible! modArithBin error (wrong # of Cores)"

-- | For performing modular division within a finite type.
modDiv :: Integer -> [Core] -> IM Value
modDiv n [c1,c2] = do
  VNum _ a <- whnf c1
  VNum _ b <- whnf c2
  case invertMod (numerator b) n of
    Just b' -> modOp (a * (b' % 1)) (n % 1)
    Nothing -> throwError DivByZero
modDiv _ _ = error "Impossible! Wrong # of cores in modDiv"

-- | For performing modular exponentiation within a finite type.
modExp :: Integer -> [Core] -> IM Value
modExp n [c1,c2] = do
  VNum _ r1 <- whnf c1
  VNum _ r2 <- whnf c2
  let a = numerator r1
  let b = numerator r2
  return $ vnum ((powerModInteger a b n) % 1)
modExp _ _ = error "Impossible! Wrong # of Cores in modExp"

-- | Perform a count on the number of values for the given type.
countOp :: [Core] -> IM Value
countOp [CType ty]  = return $ vnum ((countOp' ty) % 1)
countOp cs          = error $ "Impossible! Called countOp on " ++ show cs

countOp' :: Type -> Integer
countOp' TyVoid            = 0
countOp' TyUnit            = 1
countOp' TyBool            = 2
countOp' (TyFin n)         = n
countOp' (TyArr ty1 ty2)   = (countOp' ty2) ^ (countOp' ty1)
countOp' (TyPair ty1 ty2)  = (countOp' ty1) * (countOp' ty2)
countOp' (TySum ty1 ty2)   = (countOp' ty1) + (countOp' ty2)
-- All other types are infinite
countOp' t                 = error $ "Impossible! The type " ++ show t ++ " is infinite."

-- | Perform an enumeration of the values of a given type.
enumOp :: [Core] -> IM Value
enumOp [CType ty] = return $ (toDiscoList (enumerate ty))
enumOp cs         = error $ "Impossible! Called enumOp on " ++ show cs

-- | Convert a Haskell list of Values into a Value representing a
-- disco list.
toDiscoList :: [Value] -> Value
toDiscoList []       = VCons 0 []
toDiscoList (x : xs) = VCons 1 [x, toDiscoList xs]

-- | Perform a square root operation. If the program typechecks,
--   then the argument and output will really be Naturals
integerSqrt :: Rational -> Rational
integerSqrt n = integerSqrt' (fromIntegral (numerator n)) % 1

-- | implementation of `integerSqrt'` taken from the Haskell wiki:
--   https://wiki.haskell.org/Generic_number_type#squareRoot
integerSqrt' :: Integer -> Integer
integerSqrt' 0 = 0
integerSqrt' 1 = 1
integerSqrt' n =
  let twopows = iterate (^!2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (integerSqrt' (div n lowerN ) * lowerRoot)
      isRoot r = r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

-- this operator is used for `integerSqrt'`
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

floorOp :: Rational -> Rational
floorOp n = (floor n) % 1

ceilOp :: Rational -> Rational
ceilOp n  = (ceiling n) % 1

-- | Perform a base-2 logarithmic operation
lgOp :: [Core] -> IM Value
lgOp [c] = do
  VNum _ m <- whnf c
  lgOp' m
lgOp cs = error $ "Impossible! lgOp on " ++ show cs

lgOp' :: Rational -> IM Value
lgOp' 0 = throwError LgOfZero
lgOp' n = return $ vnum (toInteger (integerLog2 (numerator n)) % 1)

-- | Perform a division. Throw a division by zero error if the second
--   argument is 0.
divOp :: Rational -> Rational -> IM Value
divOp _ 0 = throwError DivByZero
divOp m n = return $ vnum (m / n)

-- | Perform a mod operation; throw division by zero error if the
--   second argument is zero.  Although this function takes two
--   'Rational' arguments, note that if the disco program typechecks
--   then the arguments must in fact be integers.
modOp :: Rational -> Rational -> IM Value
modOp m n
  | n == 0    = throwError DivByZero
  | otherwise = return $ vnum ((numerator m `mod` numerator n) % 1)
                -- This is safe since if the program typechecks, mod will only ever be
                -- called on integral things.

-- | Perform a boolean operation.
boolOp :: (Bool -> Bool -> Bool) -> [Core] -> IM Value
boolOp (#) cs = do
  [VCons i [], VCons j []] <- mapM whnf cs
  return . mkEnum $ toEnum i # toEnum j

-- | Test whether one number divides another.
divides :: Rational -> Rational -> IM Value
divides 0 0 = return $ mkEnum True
divides 0 _ = return $ mkEnum False
divides x y = return . mkEnum $ denominator (y / x) == 1

-- | Test relative primality.  Note that if the disco program
--   typechecks, the arguments here will always be integers.
relPm :: Rational -> Rational -> IM Value
relPm (numerator -> x) (numerator -> y) = return . mkEnum $ gcd x y == 1

-- | Binomial coefficient.  The arguments will always be natural
--   numbers.
binom :: Rational -> Rational -> Rational
binom (numerator -> n) (numerator -> k) = choose n k % 1

multinomOp :: [Core] -> IM Value
multinomOp [c1, c2] = do
  VNum _ n <- whnf c1
  ks       <- rnf  c2
  return . vnum $ multinomial (numerator n) (asList ks) % 1
  where
    asList :: Value -> [Integer]
    asList (VCons 0 _) = []
    asList (VCons 1 [VNum _ k, ks]) = numerator k : asList ks
    asList v = error $ "multinomOp.asList " ++ show v

    multinomial :: Integer -> [Integer] -> Integer
    multinomial _ []     = 1
    multinomial n (k:ks)
      | k > n     = 0
      | otherwise = choose n k * multinomial (n-k) ks

multinomOp cs = error $ "Impossible! multinomOp " ++ show cs

-- | Factorial.  The argument will always be a natural number.
fact :: Rational -> Rational
fact (numerator -> n) = factorial (fromIntegral n) % 1

-- | Perform boolean negation.
notOp :: [Core] -> IM Value
notOp [c] = do
  VCons i [] <- whnf c
  return . mkEnum . not . toEnum $ i
notOp _ = error "Impossible! notOp called on list of length /= 1"

------------------------------------------------------------
-- Equality testing
------------------------------------------------------------

-- | Test two expressions for equality at the given type.
eqOp :: Type -> [Core] -> IM Value
eqOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  mkEnum <$> decideEqFor ty v1 v2

-- | Lazily decide equality of two values at the given type.
decideEqFor :: Type -> Value -> Value -> IM Bool

-- To decide equality at a pair type:
decideEqFor (TyPair ty1 ty2) v1 v2 = do

  -- First, reduce both values to WHNF, which will produce pairs.
  VCons 0 [v11, v12] <- whnfV v1
  VCons 0 [v21, v22] <- whnfV v2

  -- Now decide equality of the first components.
  b1 <- decideEqFor ty1 v11 v21
  case b1 of
    -- If they are not equal, we know the pairs are not equal, so
    -- return False immediately without looking at the second
    -- components.
    False -> return False

    -- Otherwise, decide equality of the second components.
    True  -> decideEqFor ty2 v12 v22

-- To decide equality at a sum type:
decideEqFor (TySum ty1 ty2) v1 v2 = do

  -- Reduce both values to WHNF, which will produce constructors
  -- (either inl or inr) with one argument.
  VCons i1 [v1'] <- whnfV v1
  VCons i2 [v2'] <- whnfV v2

  -- Check whether the constructors are the same.
  case i1 == i2 of
    -- If not, the values are not equal.
    False -> return False
    -- If so, decide equality for the contained values at whichever
    -- type is appropriate.
    True  -> decideEqFor ([ty1, ty2] !! i1) v1' v2'

-- To decide equality at an arrow type, (ty1 -> ty2):
decideEqFor (TyArr ty1 ty2) v1 v2 = do

  -- Reduce both values to WHNF, which should produce closures (or
  -- @VFun@s).
  clos1 <- whnfV v1
  clos2 <- whnfV v2

  --  all the values of type ty1.
  let ty1s = enumerate ty1

  -- Try evaluating the functions on each value and check whether they
  -- agree.
  decideEqForClosures ty2 clos1 clos2 ty1s

-- To decide equality at a list type, [elTy]:
decideEqFor (TyList elTy) v1 v2 = do

  -- Reduce both values to WHNF; will be either nil with no arguments
  -- or cons with two.
  VCons c1 l1 <- whnfV v1
  VCons c2 l2 <- whnfV v2

  case (c1,c2) of
    (0,0) -> return True      -- Both are nil.
    (1,1) -> do               -- Both are cons.

      -- Check head equality.
      heq <- decideEqFor elTy (l1 !! 0) (l2 !! 0)
      case heq of

        -- If heads are unequal, so are lists.
        False -> return False
        -- Otherwise, check tails for equality.
        True  -> decideEqFor (TyList elTy) (l1 !! 1) (l2 !! 1)

    -- Different constructors => unequal.
    _     -> return False

-- For any other type (Void, Unit, Bool, N, Z, Q), we can just decide
-- by looking at the values reduced to WHNF.
decideEqFor _ v1 v2 = primValEq <$> whnfV v1 <*> whnfV v2

-- TODO: can the functions built by 'enumerate' be more efficient if
-- enumerate builds *both* a list and a bijection to a prefix of the
-- naturals?  Currently, the functions output by (enumerate (TyArr _ _))
-- take linear time in the size of the input to evaluate since they
-- have to do a lookup in an association list.  Does this even matter?

-- | Enumerate all the values of a given (finite) type.  If the type
--   has a linear order then the values are output in sorted order,
--   that is, @v@ comes before @w@ in the list output by @enumerate@
--   if and only if @v < w@.  This function will never be called on an
--   infinite type, since type checking ensures that equality or
--   comparison testing will only be done in cases where a finite
--   enumeration is required.
enumerate :: Type -> [Value]

-- There are zero, one, and two values of types Void, Unit, and Bool respectively.
enumerate TyVoid           = []
enumerate TyUnit           = [VCons 0 []]
enumerate TyBool           = [VCons 0 [], VCons 1 []]

enumerate (TyFin n)        = map enumTyFin [0..(n-1)]

-- To enumerate a pair type, take the Cartesian product of enumerations.
enumerate (TyPair ty1 ty2) = [VCons 0 [x, y] | x <- enumerate ty1, y <- enumerate ty2]

-- To enumerate a sum type, enumerate all the lefts followed by all the rights.
enumerate (TySum ty1 ty2)  =
  map (VCons 0 . (:[])) (enumerate ty1) ++
  map (VCons 1 . (:[])) (enumerate ty2)

-- To enumerate an arrow type @ty1 -> ty2@, enumerate all values of
-- @ty1@ and @ty2@, then create all possible @|ty1|@-length lists of
-- values from the enumeration of @ty2@, and make a function by
-- zipping each one together with the values of @ty1@.
enumerate (TyArr ty1 ty2)  = map mkFun (sequence (vs2 <$ vs1))
  where
    vs1 = enumerate ty1
    vs2 = enumerate ty2

    -- The actual function works by looking up the input value in an
    -- association list.
    mkFun :: [Value] -> Value
    mkFun outs
      = VFun . ValFun $ \v ->
        snd . fromJust' v . find (decideEqForRnf ty1 v . fst) $ zip vs1 outs

    -- A custom version of fromJust' so we get a better error message
    -- just in case it ever happens
    fromJust' _ (Just x) = x
    fromJust' v Nothing  = error $ "Impossible! fromJust in enumerate: " ++ show v

enumerate _ = []  -- other cases shouldn't happen if the program type checks

-- Used for more effecient enumeration of finite types
enumTyFin :: Integer -> Value
enumTyFin n = VNum Fraction (n%1)

-- | Decide equality for two values at a given type, when we already
--   know the values are in RNF.  This means the result doesn't need
--   to be in the @IM@ monad, because no evaluation needs to happen.
decideEqForRnf :: Type -> Value -> Value -> Bool
decideEqForRnf (TyPair ty1 ty2) (VCons 0 [v11, v12]) (VCons 0 [v21, v22])
  = decideEqForRnf ty1 v11 v21 && decideEqForRnf ty2 v12 v22
decideEqForRnf (TySum ty1 ty2) (VCons i1 [v1']) (VCons i2 [v2'])
  = i1 == i2 && decideEqForRnf ([ty1, ty2] !! i1) v1' v2'
decideEqForRnf (TyArr ty1 ty2) (VFun (ValFun f1)) (VFun (ValFun f2))
  = all (\v -> decideEqForRnf ty2 (f1 v) (f2 v)) (enumerate ty1)
decideEqForRnf _ v1 v2 = primValEq v1 v2

-- | @decideEqForClosures ty f1 f2 vs@ lazily decides whether the given
--   functions @f1@ and @f2@ produce the same output (of type @ty@) on
--   all inputs in @vs@.
decideEqForClosures :: Type -> Value -> Value -> [Value] -> IM Bool
decideEqForClosures ty2 clos1 clos2 = go
  where

    -- If we made it through all the values without finding one on
    -- which the functions disagree, then they are equal.
    go []     = return True

    go (v:vs) = do

      -- Apply the closures to v
      r1 <- whnfApp clos1 v
      r2 <- whnfApp clos2 v

      -- Decide whether the results are equal
      b  <- decideEqFor ty2 r1 r2

      case b of
        -- If the results are not equal, immediately return False
        -- without considering other inputs.
        False -> return False

        -- Otherwise, continue checking the rest of the inputs.
        True  -> go vs

-- | Decide whether two values of a primitive type (Void, Unit, Bool,
--   N, Z, Q) are equal.
primValEq :: Value -> Value -> Bool
primValEq (VCons i []) (VCons j []) = i == j
primValEq (VNum _ n1)  (VNum _ n2)  = n1 == n2
primValEq _ _                       = False

------------------------------------------------------------
-- Comparison testing
------------------------------------------------------------

-- | Test two expressions to see whether the first is less than the
--   second at the given type.
ltOp :: Type -> [Core] -> IM Value
ltOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  (mkEnum . (==LT)) <$> decideOrdFor ty v1 v2

-- | Lazily decide the ordering of two values at the given type.
decideOrdFor :: Type -> Value -> Value -> IM Ordering

-- To decide the ordering of two pairs:
decideOrdFor (TyPair ty1 ty2) v1 v2 = do

  -- Reduce both pairs to WHNF
  VCons 0 [v11, v12] <- whnfV v1
  VCons 0 [v21, v22] <- whnfV v2

  -- Decide the ordering of the first pair components.
  o1 <- decideOrdFor ty1 v11 v21
  case o1 of
    -- Pairs are ordered lexicographically, so if the first components
    -- are equal, then we decide the ordering for the second
    -- components
    EQ -> decideOrdFor ty2 v12 v22

    -- Otherwise we just return the ordering without looking at the
    -- second components
    _  -> return o1

-- To decide the ordering for two sum injections:
decideOrdFor (TySum ty1 ty2) v1 v2 = do

  -- Reduce to WHNF
  VCons i1 [v1'] <- whnfV v1
  VCons i2 [v2'] <- whnfV v2

  -- Compare the constructors
  case compare i1 i2 of
    -- Only compare the contents if the constructors are equal
    EQ -> decideOrdFor ([ty1, ty2] !! i1) v1' v2'

    -- Otherwise return the ordering of the constructors
    o  -> return o

-- To decide the ordering for two functions:
decideOrdFor (TyArr ty1 ty2) v1 v2 = do

  -- Reduce both to WHNF
  clos1 <- whnfV v1
  clos2 <- whnfV v2

  -- Enumerate the values of the input type, and then order the
  -- functions by applying them both to each value in the enumeration
  -- in turn, returning the ordering on the first value where the
  -- functions differ.
  let ty1s = enumerate ty1
  decideOrdForClosures ty2 clos1 clos2 ty1s

-- To decide the ordering for two lists:
decideOrdFor (TyList ty) v1 v2 = do

  -- Reduce both to WHNF
  l1 <- whnfV v1
  l2 <- whnfV v2

  -- Lexicographic ordering
  case (l1, l2) of
    (VCons 0 _, VCons 0 _) -> return EQ   -- Both empty list
    (VCons 0 _, VCons 1 _) -> return LT   -- Empty < cons
    (VCons 1 _, VCons 0 _) -> return GT   -- Cons > empty
    (VCons 1 [x1, l1'], VCons 1 [x2, l2']) -> do
      o <- decideOrdFor ty x1 x2
      case o of
        EQ -> decideOrdFor (TyList ty) l1' l2'
        _  -> return o
    _ -> error $ "Impossible! decideOrdFor " ++ show (TyList ty, v1, v2)

-- Otherwise we can compare the values primitively, without looking at
-- the type.
decideOrdFor _ v1 v2 = primValOrd <$> whnfV v1 <*> whnfV v2

-- | Compare two functions lazily.  Functions are ordered
--   lexicographically, if we think of a function @f : ty1 -> ty2@ as
--   a tuple of @ty2@ values indexed by the ordered list of all @ty1@
--   values.
--
--   Specifically, @decideOrdForClosures ty2 c1 c2 vs@, given two
--   closures @c1@ and @c2@ of type @ty1 -> ty2@, and an enumeration
--   @vs@ of values of type @ty1@, applies the closures to each
--   subsequent value in @vs@ until the first one where the outputs
--   differ; the ordering of those outputs is immediately returned
--   without evaluating the functions on any further values in @vs@.
--   Returns @EQ@ if the functions are equal on all values in @vs@.
decideOrdForClosures :: Type -> Value -> Value -> [Value] -> IM Ordering
decideOrdForClosures ty2 clos1 clos2 = go
  where

    -- If there are no more input values to compare on, the functions
    -- are equal.
    go []     = return EQ
    go (v:vs) = do

      -- Apply both functions to the value v.
      r1 <- whnfApp clos1 v
      r2 <- whnfApp clos2 v

      -- Check the ordering of the results at the output type.
      o  <- decideOrdFor ty2 r1 r2
      case o of

        -- If the functions agree on v, then keep comparing on the
        -- rest of the values.
        EQ -> go vs

        -- Otherwise return the ordering of their outputs.
        _  -> return o

-- | Decide the ordering of two values of a primitive type (Void,
--   Unit, Bool, N, Z, Q).
primValOrd :: Value -> Value -> Ordering
primValOrd (VCons i []) (VCons j []) = compare i j
primValOrd (VNum _ n1)  (VNum _ n2)  = compare n1 n2
primValOrd _ _                       = error "primValOrd: impossible!"

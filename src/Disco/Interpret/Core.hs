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
         Value(..), ValFun(..), prettyValue

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
       , decideEqFor, decideEqForRnf, decideEqForAll

         -- * Comparison testing
       , ltOp, primValOrd, decideOrdFor, decideOrdForLex

       )
       where

import           Control.Lens                       ((%~), (.~), _1)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char                          (toLower)
import           Data.List                          (find)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust)
import           Data.Ratio

import           Unbound.LocallyNameless            hiding (enumerate, rnf)

import           Math.Combinatorics.Exact.Binomial  (choose)
import           Math.Combinatorics.Exact.Factorial (factorial)

import           Disco.AST.Core
import           Disco.Desugar
import           Disco.Types

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | The type of values produced by the interpreter.
data Value where
  -- | A numeric value.
  VNum   :: Rational -> Value

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
  deriving Show

-- | A @ValFun@ is just a Haskell function @Value -> Value@.  It is a
--   @newtype@ just so we can have a custom @Show@ instance for it and
--   then derive a @Show@ instance for the rest of the @Value@ type.
newtype ValFun = ValFun (Value -> Value)

instance Show ValFun where
  show _ = "<fun>"

-- | Basic pretty-printing of values.
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

------------------------------------------------------------
-- Environments & errors
------------------------------------------------------------

-- | An environment is a mapping from names to values.
type Env  = M.Map (Name Core) Value

-- | A definition environment is a mapping fron names to core terms.
type DefEnv = M.Map (Name Core) Core

derive [''Value, ''ValFun]

-- | Errors that can be generated during interpreting.
data InterpError where

  -- | An unbound name.
  UnboundError  :: Name Core -> InterpError

  -- | v should be a number, but isn't.
  NotANum       :: Value     -> InterpError

  -- | Division by zero.
  DivByZero     ::              InterpError

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
rnfV v              = return v

-- | Reduce a value to weak head normal form.
whnfV :: Value -> IM Value
whnfV v@(VThunk c e) = withEnv e $ whnf c
whnfV v              = return v

-- | Reduce a Core expression to weak head normal form.
whnf :: Core -> IM Value
whnf (CVar x) = do
  e <- getEnv
  case (M.lookup (translate x) e) of
    Just v  -> whnfV v
    Nothing -> do
      c <- getDefEnv
      case (M.lookup x c) of
        Just core -> whnf core
        Nothing   -> throwError $ UnboundError x

whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)
whnf (CNat n)       = return $ VNum (n % 1)
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
  extend (translate x) v1 $ whnf t2
whnf (CCase bs)     = whnfCase bs

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
whnfApp f _ = error "Impossible! First argument to whnfApp is not a closure."

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
whnfOp OFact    = uNumOp fact
whnfOp (OEq ty) = eqOp ty
whnfOp (OLt ty) = ltOp ty
whnfOp ONot     = notOp

-- | Perform a numeric binary operation.
numOp :: (Rational -> Rational -> Rational) -> [Core] -> IM Value
numOp (#) = numOp' (\m n -> return (VNum (m # n)))

-- | A more general version of 'numOp' where the binary operation has
--   a result in the @IM@ monad (/e.g./ for operations which can throw
--   a division by zero error).
numOp' :: (Rational -> Rational -> IM Value) -> [Core] -> IM Value
numOp' (#) cs = do
  [VNum m, VNum n] <- mapM whnf cs     -- If the program type checked this can
  m # n                                -- never go wrong.

-- | Perform a numeric unary operation.
uNumOp :: (Rational -> Rational) -> [Core] -> IM Value
uNumOp f [c] = do
  VNum m <- whnf c
  return $ VNum (f m)

-- | Perform a division. Throw a division by zero error if the second
--   argument is 0.
divOp :: Rational -> Rational -> IM Value
divOp _ 0 = throwError DivByZero
divOp m n = return $ VNum (m / n)

-- | Perform a mod operation; throw division by zero error if the
--   second argument is zero.  Although this function takes two
--   'Rational' arguments, note that if the disco program typechecks
--   then the arguments must in fact be integers.
modOp :: Rational -> Rational -> IM Value
modOp m n
  | n == 0    = throwError DivByZero
  | otherwise = return $ VNum ((numerator m `mod` numerator n) % 1)
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

-- | Factorial.  The argument will always be a natural number.
fact :: Rational -> Rational
fact (numerator -> n) = factorial (fromIntegral n) % 1

-- | Perform boolean negation.
notOp :: [Core] -> IM Value
notOp [c] = do
  VCons i [] <- whnf c
  return . mkEnum . not . toEnum $ i

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

  -- Enumerate all the values of type ty1.
  let ty1s = enumerate ty1

  -- Try evaluating the functions on each value and check whether they
  -- agree.
  decideEqForAll ty2 clos1 clos2 ty1s

-- For any other type (Void, Unit, Bool, N, Z, Q), we can just decide
-- by looking at the values reduced to WHNF.
decideEqFor _ v1 v2 = primValEq <$> whnfV v1 <*> whnfV v2

-- | Enumerate all the values of a given (finite) type.  This function
--   will never be called on an infinite type, since type checking
--   ensures that equality or comparison testing will only be done in
--   cases where a finite enumeration is required.
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
      = VFun . ValFun $ \v -> snd . fromJust' v . find (decideEqForRnf ty1 v . fst) $ zip vs1 vs2
    fromJust' _ (Just x) = x
    fromJust' v Nothing  = error $ "fromJust in enumerate: " ++ show v
enumerate _ = []  -- other cases shouldn't happen if the program type checks

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

-- | @decideEqForAll ty f1 f2 vs@ lazily decides whether the given
--   functions @f1@ and @f2@ produce the same output (of type @ty@) on
--   all inputs in @vs@.
decideEqForAll :: Type -> Value -> Value -> [Value] -> IM Bool
decideEqForAll ty2 clos1 clos2 vs = go vs
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
primValEq (VNum n1)    (VNum n2)    = n1 == n2
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

-- XXX comment above implementation

-- XXX comment me
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

-- | Decide the ordering of two values of a primitive type (Void,
--   Unit, Bool, N, Z, Q).
primValOrd :: Value -> Value -> Ordering
primValOrd (VCons i []) (VCons j []) = compare i j
primValOrd (VNum n1) (VNum n2)       = compare n1 n2
primValOrd _ _                       = error "primValOrd: impossible!"

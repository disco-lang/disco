{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE ViewPatterns             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interpret.Core
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A big-step interpreter for the desugared, untyped Disco core
-- language.
--
-----------------------------------------------------------------------------

module Disco.Interpret.Core
       (
         -- * Evaluation
         loadDefs
       , vnum
       , mkEnum

         -- ** Full reduction
       , rnf, rnfV

         -- ** Weak head reduction
       , whnf, whnfV

         -- * Equality testing and enumeration
       , eqOp, primValEq, enumerate
       , decideEqFor, decideEqForRnf, decideEqForClosures

         -- * Comparison testing
       , ltOp, primValOrd, decideOrdFor, decideOrdForClosures

         -- * Lists

       , toDiscoList
       , vfoldr, vappend, vconcat, vmap

       )
       where

import           Control.Lens                       (use, (%=), (.=))
import           Control.Monad.Except               (throwError)
import           Data.Bifunctor                     (first)
import           Data.Char
import           Data.Coerce                        (coerce)
import           Data.IntMap.Lazy                   ((!))
import qualified Data.IntMap.Lazy                   as IntMap
import           Data.List                          (find, subsequences)
import qualified Data.Map                           as M
import           Data.Ratio

import           Unbound.Generics.LocallyNameless

import           Math.Combinatorics.Exact.Binomial  (choose)
import           Math.Combinatorics.Exact.Factorial (factorial)

import           Math.NumberTheory.Logarithms       (integerLog2)
import           Math.NumberTheory.Moduli.Class     (SomeMod (..), getVal,
                                                     invertSomeMod, modulo,
                                                     powSomeMod)
import           Math.NumberTheory.Primes.Testing   (isPrime)

import           Disco.AST.Core
import           Disco.AST.Surface                  (Ellipsis (..),
                                                     fromTelescope)
import           Disco.Context
import           Disco.Eval
import           Disco.Types

------------------------------------------------------------
-- Evaluation
------------------------------------------------------------

-- | Load a top-level environment of (potentially recursive)
--   definitions into memory.
loadDefs :: Ctx Core Core -> Disco e ()
loadDefs cenv = do

  -- Clear out any leftover memory.
  memory .= IntMap.empty

  -- Take the environment mapping names to definitions, and turn
  -- each one into an indirection to a thunk stored in memory.
  env <- traverse mkThunk cenv

  -- Top-level definitions are allowed to be recursive, so each
  -- one of those thunks should actually have the environment
  -- 'env' as its environment, so all the top-level definitions
  -- can mutually refer to each other.
  --
  -- For now we know that the only things we have stored in memory
  -- are the thunks we just made, so just iterate through them and
  -- replace their environments.
  memory %= IntMap.map (replaceThunkEnv env)

  -- Finally, set the top-level environment to the one we just
  -- created.
  topEnv .= env

  where
    replaceThunkEnv e (VThunk c _) = VThunk c e
    replaceThunkEnv _ v            = v


-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
vnum :: Rational -> Value
vnum = VNum mempty

-- | Turn any instance of @Enum@ into a @Value@, by creating a
--   constructor with an index corresponding to the enum value.
mkEnum :: Enum e => e -> Value
mkEnum e = VCons (fromEnum e) []

--------------------------------------------------
-- Reduced normal form
--------------------------------------------------

-- | Evaluate a @Core@ expression to reduced normal form, i.e. keep
--   reducing under constructors as much as possible.  In practice,
--   all this function actually does is turn the @Core@ expression
--   into a thunk and then call 'rnfV'.
rnf :: Core -> Disco IErr Value
rnf c = mkThunk c >>= rnfV

-- | Reduce a value to reduced normal form, i.e. keep reducing under
--   constructors as much as possible.
rnfV :: Value -> Disco IErr Value

-- The value is a constructor: keep the constructor and recursively
-- reduce all its contents.
rnfV (VCons i vs)   = VCons i <$> mapM rnfV vs

-- If the value is a thunk (i.e. unevaluated expression), a delayed
-- computation, or an indirection (i.e. a pointer to a value), reduce
-- it one step using 'whnfV' and then recursively continue reducing
-- the result.
rnfV v@(VThunk _ _) = whnfV v >>= rnfV
rnfV v@(VDelay _)   = whnfV v >>= rnfV
rnfV v@(VIndir _)   = whnfV v >>= rnfV

-- Otherwise, the value is already in reduced normal form (for
-- example, it could be a number or a function).
rnfV v              = return v

--------------------------------------------------
-- Weak head normal form (WHNF)
--------------------------------------------------

-- | Reduce a value to weak head normal form, that is, reduce it just
--   enough to find out what its top-level constructor is.
whnfV :: Value -> Disco IErr Value

-- If the value is a thunk, use its stored environment and evaluate
-- the expression to WHNF.
whnfV (VThunk c e) = withEnv e $ whnf c

-- If it is a delayed computation, we can't delay any longer: run it
-- and reduce the result to WHNF.
whnfV (VDelay imv) = imv >>= whnfV

-- If it is an indirection, call 'whnfIndir' which will look up the
-- value it points to and reduce it.
whnfV (VIndir loc) = whnfIndir loc

-- Otherwise, the value is already in WHNF (it is a number, a
-- function, or a constructor).
whnfV v            = return v


-- | Reduce the value stored at the given location to WHNF.  We need a
--   special function for this, because in addition to returning the
--   reduced value, we also update the memory location to contain it.
--   That way if anything else refers to the same location, it will
--   not need to be re-evaluated later.
whnfIndir :: Loc -> Disco IErr Value
whnfIndir loc = do
  m <- use memory                   -- Get the memory map
  v <- whnfV (m ! loc)              -- Look up the given location and reduce it to WHNF
  memory %= IntMap.insert loc v     -- Update memory with the reduced value
  return v                          -- Finally, return the value.


-- | Reduce a Core expression to weak head normal form.  This is where
--   the real work of interpreting happens.
whnf :: Core -> Disco IErr Value

-- To reduce a variable, look it up in the environment and reduce the
-- result.
whnf (CVar x) = do
  e <- getEnv
  case (M.lookup (coerce x) e) of
    Just v  -> whnfV v
    Nothing -> error $ "Unbound variable while interpreting! " ++ show x
      -- We should never encounter an unbound variable at this stage if the program
      -- already typechecked.

-- Interpret each supported primitive.
whnf (CPrim "isPrime") = return $ VFun primIsPrime
whnf (CPrim "crash")   = return $ VFun primCrash
whnf (CPrim x)         = throwError $ UnknownPrim x

-- whnf (CPrim PMap ty) =
--   return $ VFun (ValFun (\f -> VFun (ValFun (\s -> VDelay (ValDelay (mapSet f s ty))))))

-- -- whnf (CPrim PFoldSet ty) =
-- --   return $ VFun (ValFun (\f -> VFun (ValFun (\b -> VFun (ValFun (\s -> VDelay (ValDelay (foldSet f b s ty))))))))

-- -- whnf (CPrim PFoldMultiset ty) =
-- --   return $ VFun (ValFun (\f -> VFun (ValFun (\b -> VFun (ValFun (\m -> VDelay (ValDelay (foldMultiset f b m ty))))))))
-- whnf (CPrim PStoM _) =
--   return $ VFun (ValFun (\s -> VDelay (ValDelay (setToMultiset s))))

-- whnf (CPrim PMtoS _) =
--   return $ VFun (ValFun (\m -> VDelay (ValDelay (multisetToSet m))))

-- A constructor is already in WHNF, so just turn its contents into
-- thunks to be evaluated later when they are demanded.
whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)

-- A number is in WHNF, just turn it into a VNum.
whnf (CNum d n)     = return $ VNum d n

-- A lambda abstraction is already in WHNF; just package it up as a
-- closure with the current environment.
whnf (CAbs b)       = VClos b <$> getEnv

-- To reduce an application:
whnf (CApp str c1 c2) = do

  -- First reduce the function to WHNF
  v1 <- whnf c1

  -- Then either reduce the argument or turn it into a thunk,
  -- depending on whether the application is strict or lazy
  v2 <- case str of
    Strict -> whnf c2       -- for types with strict evaluation, whnf = full reduction
    Lazy   -> mkThunk c2

  -- Finally, call 'whnfApp' to do the application.
  whnfApp v1 v2

-- Ellipses, case expressions, and operators
-- all have their own function to do reduction.
whnf (CEllipsis ts ell) = expandEllipsis ts ell
whnf (CCase bs)     = whnfCase bs
whnf (COp op cs)    = whnfOp op cs

whnf (CoreSet t es) = do
  res <- mapM mkThunk es
  dres <- countValues t res
  return $ VSet ((map (fmap (const 1))) dres)

whnf (CoreMultiset tyElt es) = do
  res <- mapM mkThunk es
  cres <- countValues tyElt res
  return $ VMultiset cres

whnf (CType _)      = error "Called whnf on CType"

elemOf :: Type -> Value -> [Value] -> Disco IErr Bool
elemOf _ _ []     = return False
elemOf t x (y:ys) = (||) <$> (decideEqFor t x y) <*> (elemOf t x ys)

countValues :: Type -> [Value] -> Disco IErr [(Value, Integer)]
countValues ty = sortNCount (decideOrdFor ty) . (map (,1))

-- Merge sorts the values in the list using the given function, while
-- also counting the number of occurences of each element.
sortNCount :: (Monad m) => (a -> a -> m Ordering) -> [(a,Integer)] -> m [(a, Integer)]
sortNCount _ []  = return []
sortNCount _ [x] = return [x]
sortNCount f xs  = do
  fstSorted <- sortNCount f firstHalf
  sndSorted <- sortNCount f secondHalf
  merge (+) f fstSorted sndSorted
  where
    (firstHalf, secondHalf) = splitAt n xs
    n = length xs `div` 2

-- Merges two lists of type [(a,Integer)] together a la merge sort.  Is guaranteed to
-- work provided that the two lists given have already been sorted and counted.
merge :: (Monad m) => (Integer -> Integer -> Integer) -> (a -> a -> m Ordering) -> [(a, Integer)] -> [(a, Integer)] -> m [(a, Integer)]
merge g _ [] ys                    = case g 0 1 of
  1 -> return ys
  0 -> return []
merge g _ xs []                    = case g 1 0 of
  1 -> return xs
  0 -> return []
merge g comp ((x,n1):xs) ((y,n2): ys) = do
  ord <- comp x y
  case ord of
    LT -> (mergeCons x n1 0) <$> merge g comp xs ((y,n2):ys)
    EQ -> (mergeCons x n1 n2) <$> merge g comp xs ys
    GT -> (mergeCons y 0 n2) <$> merge g comp ((x,n1):xs) ys
    where
      mergeCons a m1 m2 zs = case g m1 m2 of
        0 -> zs
        n -> ((a,n):zs)

mapSet :: Value -> Value -> Type -> Disco IErr Value
mapSet f s ty = do
  fcn <- whnfV f
  (VSet xs) <- whnfV s
  ys <- mapM (\(x,n) -> (,n) <$> whnfApp fcn x) xs
  VSet <$> sortNCount (decideOrdFor ty) ys

-- foldrValues :: Value -> Value -> [Value] -> Disco IErr Value
-- foldrValues _ b []     = return b
-- foldrValues f b (x:xs) = do
--   fx <- whnfApp f x
--   foldedList <- foldrValues f b xs
--   whnfApp fx foldedList
--
-- foldSet :: Value -> Value -> Value -> Type -> Disco IErr Value
-- foldSet f b s ty = do
--   fcn <- whnfV f
--   vb <- whnfV b
--   (VSet xs) <- whnfV s
--   sortedXs <- mergeSort xs ty
--   foldrValues fcn vb sortedXs

setToMultiset :: Value -> Disco IErr Value
setToMultiset s = do
  (VSet xs) <- whnfV s
  return $ VMultiset xs

multisetToSet :: Value -> Disco IErr Value
multisetToSet m = do
  (VMultiset xs) <- whnfV m
  return $ VSet (map (\(v,n) -> (VCons 0 [v, VNum Fraction (n % 1)], 1)) xs)

-- | Reduce an application to weak head normal form (WHNF).
--   Precondition: the first argument has already been reduced to WHNF
--   (which means it must be either a closure or a @VFun@).
whnfApp :: Value -> Value -> Disco IErr Value
whnfApp (VClos c e) v =
  lunbind c $ \(x,t) -> do
  withEnv e           $ do
  extend x v          $ do
  whnf t
whnfApp (VFun f) v = rnfV v >>= \v' -> whnfV (f v')
whnfApp _ _ = error "Impossible! First argument to whnfApp is not a closure."

------------------------------------------------------------
-- Lists
------------------------------------------------------------

--------------------------------------------------
-- Utilities

-- | Convert a Haskell list of Values into a Value representing a
-- disco list.
toDiscoList :: [Value] -> Value
toDiscoList []       = VCons 0 []
toDiscoList (x : xs) = VCons 1 [x, toDiscoList xs]

-- | A lazy foldr on lists represented as 'Value's.  The entire
--   function is wrapped in a call to 'delay', so it does not actually
--   reduce the list argument to whnf until (potentially) forced by a
--   call to 'whnf' later.  That is, executing the resulting @Disco
--   Value@ just results in a 'VDelay'; forcing that @VDelay@ with
--   'whnf' will then cause the foldr to actually start executing.
vfoldr :: (Value -> Value -> Disco IErr Value) -> Value -> Value -> Disco IErr Value
vfoldr f z xs = delay $ do
  xs' <- whnfV xs
  case xs' of
    VCons 0 []      -> return z
    VCons 1 [vh,vt] -> do
      r <- vfoldr f z vt
      f vh r
    _               -> error $ "Impossible! Got " ++ show xs' ++ " in vfoldr"

-- | Lazy append on 'Value' lists, implemented via 'vfoldr'.
vappend :: Value -> Value -> Disco IErr Value
vappend xs ys = vfoldr (\h t -> return $ VCons 1 [h,t]) ys xs

-- | Lazy concat on 'Value' lists, implemented via 'vfoldr'.
vconcat :: Value -> Disco IErr Value
vconcat = vfoldr vappend (VCons 0 [])

-- | Lazy map on 'Value' lists, implemented via 'vfoldr'.
vmap :: (Value -> Disco IErr Value) -> Value -> Disco IErr Value
vmap f = vfoldr (\h t -> f h >>= \h' -> return $ VCons 1 [h', t]) (VCons 0 [])

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

expandEllipsis :: [Core] -> Ellipsis Core -> Disco IErr Value
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
whnfCase :: [CBranch] -> Disco IErr Value
whnfCase []     = throwError NonExhaustive
whnfCase (b:bs) = do
  lunbind b $ \(gs, t) -> do
  res <- checkGuards (fromTelescope gs)
  case res of
    Nothing -> whnfCase bs
    Just e' -> extends e' $ whnf t

-- | Check a chain of guards on one branch of a case.  Returns
--   @Nothing@ if the guards fail to match, or a resulting environment
--   of bindings if they do match.
checkGuards :: [(Embed Core, CPattern)] -> Disco IErr (Maybe Env)
checkGuards [] = ok
checkGuards ((unembed -> c, p) : gs) = do
  v <- mkThunk c
  res <- match v p
  case res of
    Nothing -> return Nothing
    Just e  -> extends e (fmap (M.union e) <$> checkGuards gs)

-- | Match a value against a pattern, returning an environment of
--   bindings if the match succeeds.
match :: Value -> CPattern -> Disco IErr (Maybe Env)
match v (CPVar x)     = return $ Just (M.singleton (coerce x) v)
match _ CPWild        = ok
match v (CPCons i xs) = do
  VCons j vs <- whnfV v
  case i == j of
    False -> noMatch
    True  -> return (Just . M.fromList $ zip xs vs)
match v (CPNat n)     = do
  VNum _ m <- whnfV v
  case m == n % 1 of
    False -> noMatch
    True  -> ok
match v (CPFrac x y) = do
  VNum _ r <- whnfV v
  return . Just . M.fromList $ [ (x, vnum (numerator   r % 1))
                               , (y, vnum (denominator r % 1))
                               ]

-- | Convenience function: successfully match with no bindings.
ok :: Disco e (Maybe Env)
ok = return $ Just M.empty

-- | Convenience function: fail to match.
noMatch :: Disco e (Maybe Env)
noMatch = return Nothing

------------------------------------------------------------
-- Operator evaluation
------------------------------------------------------------

-- | Reduce an operator application to WHNF.
whnfOp :: Op -> [Core] -> Disco IErr Value
whnfOp OAdd           = numOp (+)
whnfOp ONeg           = uNumOp negate
whnfOp OSqrt          = uNumOp integerSqrt
whnfOp OLg            = lgOp
whnfOp OFloor         = uNumOp floorOp
whnfOp OCeil          = uNumOp ceilOp
whnfOp OAbs           = uNumOp abs
whnfOp OMul           = numOp (*)
whnfOp ODiv           = numOp' divOp
whnfOp OExp           = numOp (\m n -> m ^^ numerator n)
  -- If the program typechecks, n will be an integer.
whnfOp OMod           = numOp' modOp
whnfOp ODivides       = numOp' (\m n -> return (mkEnum $ divides m n))
whnfOp OBinom         = numOp binom
whnfOp OMultinom      = multinomOp
whnfOp OFact          = uNumOp' fact
whnfOp (OEq ty)       = eqOp ty
whnfOp (OLt ty)       = ltOp ty
whnfOp OEnum          = enumOp
whnfOp OCount         = countOp

-- Modular operations, for finite types
whnfOp (OMDiv n)      = modDiv n
whnfOp (OMExp n)      = modExp n
whnfOp (OMDivides n)  = modDivides n

-- Set operations
whnfOp (OSize)        = setSize
whnfOp (OUnion  ty)   = setUnion ty
whnfOp (OInter  ty)   = setIntersection ty
whnfOp (ODiff   ty)   = setDifference ty
whnfOp (OPowerSet ty) = powerSet ty
whnfOp (OSubset ty)   = subsetTest ty

setSize :: [Core] -> Disco IErr Value
setSize [x] = do
  (VSet xs) <- whnf x
  return $ vnum (fromIntegral $ sum (map snd xs))
setSize _ = error "Impossible! Wrong # of Cores in setSize"

setUnion :: Type -> [Core] -> Disco IErr Value
setUnion ty [c1, c2] = do
  (VSet xs) <- whnf c1
  (VSet ys) <- whnf c2
  zs <- merge max (decideOrdFor ty) xs ys
  return $ VSet zs
setUnion _ _ = error "Impossible! Wrong # of Cores in setUnion"

setIntersection :: Type -> [Core] -> Disco IErr Value
setIntersection ty [c1, c2] = do
  (VSet xs) <- whnf c1
  (VSet ys) <- whnf c2
  zs <- merge min (decideOrdFor ty) xs ys
  return $ VSet zs
setIntersection _ _ = error "Impossible! Wrong # of Cores in setIntersection"

setDifference :: Type -> [Core] -> Disco IErr Value
setDifference ty [c1, c2] = do
  (VSet xs) <- whnf c1
  (VSet ys) <- whnf c2
  zs <- merge (\x y -> max 0 (x - y)) (decideOrdFor ty) xs ys
  return $ VSet zs
setDifference _ _ = error "Impossible! Wrong # of Cores in setDifference"

subsetTest :: Type -> [Core] -> Disco IErr Value
subsetTest ty [c1, c2] = do
  (VSet xs) <- whnf c1
  (VSet ys) <- whnf c2
  ys' <- merge (max) (decideOrdFor ty) xs ys
  mkEnum <$> setEquality ty ys ys'

powerSet :: Type -> [Core] -> Disco IErr Value
powerSet ty [c] = do
  (VSet xs) <- whnf c
  ys <- sortNCount (decideOrdFor ty) (map (\x -> (VSet x, 1)) (subsequences xs))
  return $ VSet ys

-- | Perform a numeric binary operation.
numOp :: (Rational -> Rational -> Rational) -> [Core] -> Disco IErr Value
numOp (#) = numOp' (\m n -> return (vnum (m # n)))

-- | A more general version of 'numOp' where the binary operation has
--   a result in the @Disco@ monad (/e.g./ for operations which can throw
--   a division by zero error).
numOp' :: (Rational -> Rational -> Disco IErr Value) -> [Core] -> Disco IErr Value
numOp' (#) cs = do
  [VNum d1 m, VNum d2 n] <- mapM whnf cs     -- If the program type checked this can
  res <- m # n                               -- never go wrong.
  case res of
    VNum _ r -> return $ VNum (d1 <> d2) r   -- Re-flag any resulting numeric value with
    _        -> return res                   --   the combination of the input flags.

-- | Perform a numeric unary operation.
uNumOp :: (Rational -> Rational) -> [Core] -> Disco IErr Value
uNumOp f = uNumOp' (return . f)

-- | Perform a numeric unary operation, with the ability to /e.g./
--   throw an error (used for factorial, which can overflow).
uNumOp' :: (Rational -> Disco IErr Rational) -> [Core] -> Disco IErr Value
uNumOp' f [c] = do
  VNum d m <- whnf c
  VNum d <$> f m
uNumOp' _ _ = error "Impossible! Second argument to uNumOp' has length /= 1"

-- | For performing modular division within a finite type.
modDiv :: Integer -> [Core] -> Disco IErr Value
modDiv n [c1,c2] = do
  VNum _ a <- whnf c1
  VNum _ b <- whnf c2
  case invertSomeMod (numerator b `modulo` fromInteger n) of
    Just (SomeMod b') -> modOp (a * (getVal b' % 1)) (n % 1)
    Just (InfMod{})   -> error "Impossible! InfMod in modDiv"
    Nothing           -> throwError DivByZero
modDiv _ _ = error "Impossible! Wrong # of cores in modDiv"

modDivides :: Integer -> [Core] -> Disco IErr Value
modDivides n [c1,c2] = do
  VNum _ a <- whnf c1
  VNum _ b <- whnf c2
  return $ mkEnum $ divides (toRational (gcd (numerator a) n)) b

modDivides _ _ = error "Impossible! Wrong # of cores in modDivides"

-- | For performing modular exponentiation within a finite type.
modExp :: Integer -> [Core] -> Disco IErr Value
modExp n [c1,c2] = do
  VNum _ r1 <- whnf c1
  VNum _ r2 <- whnf c2
  let base = numerator r1 `modulo` fromInteger n
      ma = if (numerator r2 >= 0)
             then Just base
             else invertSomeMod base
      b = abs (numerator r2)
  case ma of
    Nothing -> throwError DivByZero
    Just a  ->
      case powSomeMod a b of
        SomeMod v' -> return $ vnum (getVal v' % 1)
        InfMod {}  -> error "Impossible, got InfMod in modExp"
modExp _ _ = error "Impossible! Wrong # of Cores in modExp"

-- | Perform a count on the number of values for the given type.
countOp :: [Core] -> Disco IErr Value
countOp [CType ty]  = case countType ty of
                        Just num -> return $ VCons 1 [vnum (num % 1)]
                        Nothing  -> return $ VCons 0 [VCons 0 []]
countOp cs          = error $ "Impossible! Called countOp on " ++ show cs

-- | Perform an enumeration of the values of a given type.
enumOp :: [Core] -> Disco IErr Value
enumOp [CType ty] = case countType ty of
                    Just _  -> return $ (toDiscoList (enumerate ty))
                    Nothing -> throwError $ InfiniteTy ty
enumOp cs         = error $ "Impossible! Called enumOp on " ++ show cs

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
lgOp :: [Core] -> Disco IErr Value
lgOp [c] = do
  VNum _ m <- whnf c
  lgOp' m
lgOp cs = error $ "Impossible! lgOp on " ++ show cs

lgOp' :: Rational -> Disco IErr Value
lgOp' 0 = throwError LgOfZero
lgOp' n = return $ vnum (toInteger (integerLog2 (numerator n)) % 1)

-- | Perform a division. Throw a division by zero error if the second
--   argument is 0.
divOp :: Rational -> Rational -> Disco IErr Value
divOp _ 0 = throwError DivByZero
divOp m n = return $ vnum (m / n)

-- | Perform a mod operation; throw division by zero error if the
--   second argument is zero.  Although this function takes two
--   'Rational' arguments, note that if the disco program typechecks
--   then the arguments must in fact be integers.
modOp :: Rational -> Rational -> Disco IErr Value
modOp m n
  | n == 0    = throwError DivByZero
  | otherwise = return $ vnum ((numerator m `mod` numerator n) % 1)
                -- This is safe since if the program typechecks, mod will only ever be
                -- called on integral things.

-- | Test whether one number divides another.
divides :: Rational -> Rational -> Bool
divides 0 0 = True
divides 0 _ = False
divides x y = denominator (y / x) == 1

-- | Binomial coefficient.  The arguments will always be natural
--   numbers.
binom :: Rational -> Rational -> Rational
binom (numerator -> n) (numerator -> k) = choose n k % 1

multinomOp :: [Core] -> Disco IErr Value
multinomOp [c1, c2] = do
  VNum _ n <- whnf c1
  ks       <- rnf  c2
  return . vnum $ multinomial (numerator n) (asList ks) % 1
  where
    asList :: Value -> [Integer]
    asList (VCons 0 _)              = []
    asList (VCons 1 [VNum _ k, ks]) = numerator k : asList ks
    asList v                        = error $ "multinomOp.asList " ++ show v

    multinomial :: Integer -> [Integer] -> Integer
    multinomial _ []     = 1
    multinomial n (k:ks)
      | k > n     = 0
      | otherwise = choose n k * multinomial (n-k) ks

multinomOp cs = error $ "Impossible! multinomOp " ++ show cs

-- | Factorial.  The argument will always be a natural number.
fact :: Rational -> Disco IErr Rational
fact (numerator -> n)
  | n > fromIntegral (maxBound :: Int) = throwError Overflow
  | otherwise = return $ factorial (fromIntegral n) % 1

-- | Semantics of the @$isPrime@ prim: a relatively fast test for
--   primality using the 'isPrime' function from @arithmoi@ (trial
--   division + Baille-PSW).
--
--   Note @$isPrime@ should always be used at type @N -> Bool@; using
--   it at a different type will cause undefined behavior or a runtime
--   error.
primIsPrime :: Value -> Value
primIsPrime (VNum _ (numerator -> n)) = mkEnum (isPrime n)
primIsPrime _                         = error "impossible!  primIsPrime on non-VNum"

-- | Semantics of the @$crash@ prim, which crashes with a
--   user-supplied message.
--
--   @$crash@ should always be used at type @List Char -> a@; using it
--   at a different type will cause undefined behavior or a runtime
--   error.
primCrash :: Value -> Value
primCrash v = VDelay $ do
  s <- valueToString v
  throwError (Crash s)

-- | Convert a Disco value representing a list of characters into a
--   Haskell 'String'.
valueToString :: Value -> Disco IErr String
valueToString = fmap toString . rnfV
  where
    toString (VCons 0 _)              = ""
    toString (VCons 1 [VNum _ c, cs]) = chr (fromIntegral $ numerator c) : toString cs
    toString _ = "Impossible: valueToString.toString, non-list"

------------------------------------------------------------
-- Equality testing
------------------------------------------------------------

-- | Test two expressions for equality at the given type.
eqOp :: Type -> [Core] -> Disco IErr Value
eqOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  mkEnum <$> decideEqFor ty v1 v2

-- | Lazily decide equality of two values at the given type.
decideEqFor :: Type -> Value -> Value -> Disco IErr Bool

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

decideEqFor (TySet ty) v1 v2 = do
  (VSet xs) <- whnfV v1
  (VSet ys) <- whnfV v2
  setEquality ty xs ys


-- For any other type (Void, Unit, Bool, N, Z, Q), we can just decide
-- by looking at the values reduced to WHNF.
decideEqFor _ v1 v2 = primValEq <$> whnfV v1 <*> whnfV v2


setEquality :: Type -> [(Value, Integer)] -> [(Value, Integer)] -> Disco IErr Bool
setEquality _ [] [] = return True
setEquality _ [] _ = return False
setEquality _ _ [] = return False
setEquality ty ((x,n1):xs) ((y,n2):ys) = do
  eq <- (n1 == n2 &&) <$> decideEqFor ty x y
  case eq of
    False -> return False
    True  -> setEquality ty xs ys


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

enumerate (TyFin n)        = map (vnum . (%1)) [0..(n-1)]

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
enumerate (TyArr ty1 ty2)
  | isEmptyTy ty1 = [VFun $ \_ -> error "void!!"]
  | isEmptyTy ty2 = []
  | otherwise     =  map mkFun (sequence (vs2 <$ vs1))
  where
    vs1 = enumerate ty1
    vs2 = enumerate ty2

    -- The actual function works by looking up the input value in an
    -- association list.
    mkFun :: [Value] -> Value
    mkFun outs
      = VFun $ \v ->
        snd . fromJust' v . find (decideEqForRnf ty1 v . fst) $ zip vs1 outs

    -- A custom version of fromJust' so we get a better error message
    -- just in case it ever happens
    fromJust' _ (Just x) = x
    fromJust' v Nothing  = error $ "Impossible! fromJust in enumerate: " ++ show v

enumerate (TyList _ty) = [VCons 0 []]
  -- Right now, the only way for this to typecheck is if @ty@ is
  -- empty.  Perhaps in the future we'll allow 'enumerate' to work on
  -- countably infinite types, in which case we would need to change
  -- this.

enumerate _ = []   -- The only way other cases can happen at the
                   -- moment is in evaluating something like enumerate
                   -- (Nat * Void), in which case it doesn't matter if
                   -- we give back an empty list for Nat.

-- | Decide equality for two values at a given type, when we already
--   know the values are in RNF.  This means the result doesn't need
--   to be in the @Disco@ monad, because no evaluation needs to happen.
decideEqForRnf :: Type -> Value -> Value -> Bool
decideEqForRnf (TyPair ty1 ty2) (VCons 0 [v11, v12]) (VCons 0 [v21, v22])
  = decideEqForRnf ty1 v11 v21 && decideEqForRnf ty2 v12 v22
decideEqForRnf (TySum ty1 ty2) (VCons i1 [v1']) (VCons i2 [v2'])
  = i1 == i2 && decideEqForRnf ([ty1, ty2] !! i1) v1' v2'
decideEqForRnf (TyArr ty1 ty2) (VFun f1) (VFun f2)
  = all (\v -> decideEqForRnf ty2 (f1 v) (f2 v)) (enumerate ty1)
decideEqForRnf _ v1 v2 = primValEq v1 v2

-- | @decideEqForClosures ty f1 f2 vs@ lazily decides whether the given
--   functions @f1@ and @f2@ produce the same output (of type @ty@) on
--   all inputs in @vs@.
decideEqForClosures :: Type -> Value -> Value -> [Value] -> Disco IErr Bool
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
--   N, Z, Q, Zn) are equal.
primValEq :: Value -> Value -> Bool
primValEq (VCons i []) (VCons j []) = i == j
primValEq (VNum _ n1)  (VNum _ n2)  = n1 == n2
primValEq v1 v2                     = error $ "primValEq on non-primitive values " ++ show v1 ++ " and " ++ show v2

------------------------------------------------------------
-- Comparison testing
------------------------------------------------------------

-- | Test two expressions to see whether the first is less than the
--   second at the given type.
ltOp :: Type -> [Core] -> Disco IErr Value
ltOp ty cs = do
  [v1, v2] <- mapM mkThunk cs
  (mkEnum . (==LT)) <$> decideOrdFor ty v1 v2

-- | Lazily decide the ordering of two values at the given type.
decideOrdFor :: Type -> Value -> Value -> Disco IErr Ordering

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

-- To decide the ordering for two sets:
decideOrdFor (TySet ty) v1 v2 = do
  (VSet xs) <- whnfV v1
  (VSet ys) <- whnfV v2
  setComparison ty xs ys

-- Otherwise we can compare the values primitively, without looking at
-- the type.
decideOrdFor _ v1 v2 = primValOrd <$> whnfV v1 <*> whnfV v2

-- Helper function which decides the order for two sets.
setComparison :: Type -> [(Value, Integer)] -> [(Value, Integer)] -> Disco IErr Ordering
setComparison _ [] [] = return EQ
setComparison _ _ [] = return GT
setComparison _ [] _ = return LT
setComparison ty ((x,_):xs) ((y,_):ys) = do
  ord <- decideOrdFor ty x y
  case ord of
    EQ    -> setComparison ty xs ys
    other -> return other


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
decideOrdForClosures :: Type -> Value -> Value -> [Value] -> Disco IErr Ordering
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

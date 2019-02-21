{-# LANGUAGE LambdaCase               #-}
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

import           Control.Arrow                           ((***))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           ((>=>))
import           Control.Monad.Except                    (throwError)
import           Data.Bifunctor                          (second)
import           Data.Char
import           Data.Coerce                             (coerce)
import           Data.IntMap.Lazy                        ((!))
import qualified Data.IntMap.Lazy                        as IntMap
import           Data.List                               (find, subsequences)
import qualified Data.Map                                as M
import           Data.Ratio

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Math.Combinatorics.Exact.Binomial       (choose)
import           Math.Combinatorics.Exact.Factorial      (factorial)

import           Math.NumberTheory.Logarithms            (integerLog2)
import           Math.NumberTheory.Moduli.Class          (SomeMod (..), getVal,
                                                          invertSomeMod, modulo,
                                                          powSomeMod)
import           Math.NumberTheory.Primes.Factorisation  (factorise)
import           Math.NumberTheory.Primes.Testing        (isPrime)

import           Disco.AST.Core
import           Disco.AST.Surface                       (Ellipsis (..),
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

whnf (CConst x) = return $ VConst x

-- whnf (CPrim PMap ty) =
--   return $ VFun (ValFun (\f -> VFun (ValFun (\s -> VDelay (ValDelay (mapSet f s ty))))))

-- -- whnf (CPrim PFoldSet ty) =
-- --   return $ VFun (ValFun (\f -> VFun (ValFun (\b -> VFun (ValFun (\s -> VDelay (ValDelay (foldSet f b s ty))))))))

-- -- whnf (CPrim PFoldMultiset ty) =
-- --   return $ VFun (ValFun (\f -> VFun (ValFun (\b -> VFun (ValFun (\m -> VDelay (ValDelay (foldMultiset f b m ty))))))))

-- A constructor is already in WHNF, so just turn its contents into
-- thunks to be evaluated later when they are demanded.
whnf (CCons i cs)   = VCons i <$> (mapM mkThunk cs)

-- A number is in WHNF, just turn it into a VNum.
whnf (CNum d n)     = return $ VNum d n

-- A lambda abstraction is already in WHNF; just package it up as a
-- closure with the current environment.
whnf (CAbs b)       = VClos b <$> getEnv

-- To reduce an application:
whnf (CApp c cs) = do

  -- First reduce the function to WHNF
  v <- whnf c

  -- Then either reduce each argument or turn it into a thunk,
  -- depending on the specified strictness.
  vs <- mapM (uncurry whnfArg) cs

  -- Finally, call 'whnfApp' to do the application.
  whnfApp v vs

-- Case expressions have their own function to do reduction.
whnf (CCase bs)     = whnfCase bs

whnf (CType ty)      = return $ VType ty

-- XXX move all this container stuff to a different location in the file

-- | Check whether a certain value is present in a list using a linear search.
-- elemOf :: Type -> Value -> [Value] -> Disco IErr Bool
-- elemOf _ _ []     = return False
-- elemOf t x (y:ys) = (||) <$> (decideEqFor t x y) <*> (elemOf t x ys)

-- | Given a list of disco values, sort and collate them into a list
--   pairing each unique value with its count.  Used to
--   construct/normalize bags and sets.
countValues :: Type -> [Value] -> Disco IErr [(Value, Integer)]
countValues ty = sortNCount (decideOrdFor ty) . (map (,1))

-- | Normalize a list of values paired with a count.  The input list
--   could have duplicate values.  This function first uses merge sort
--   to sort the values according to the given comparison function,
--   then adds the counts of multiple instances of the same value.
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

-- | Generic function for merging two sorted, count-annotated lists of
--   type @[(a,Integer)]@ a la merge sort, using the given comparison
--   function, and using the provided count combining function to
--   decide what count to assign to each element of the output.  For
--   example, @(+)@ corresponds to bag union; @min@ corresponds to
--   intersection; and so on.
merge :: Monad m
      => (Integer -> Integer -> Integer)   -- ^ Function for combining counts
      -> (a -> a -> m Ordering)            -- ^ Comparison function
      -> [(a, Integer)] -> [(a, Integer)] -> m [(a, Integer)]
merge g _ [] ys                    = case g 0 1 of
  1 -> return ys
  0 -> return []
  _ -> error "invalid merge counting function"
merge g _ xs []                    = case g 1 0 of
  1 -> return xs
  0 -> return []
  _ -> error "invalid merge counting function"
merge g comp ((x,n1):xs) ((y,n2): ys) = do
  o <- comp x y
  case o of
    LT -> mergeCons x n1 0  <$> merge g comp xs ((y,n2):ys)
    EQ -> mergeCons x n1 n2 <$> merge g comp xs ys
    GT -> mergeCons y 0 n2  <$> merge g comp ((x,n1):xs) ys
    where
      mergeCons a m1 m2 zs = case g m1 m2 of
        0 -> zs
        n -> ((a,n):zs)

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
--   VBag xs <- whnfV s
--   sortedXs <- mergeSort xs ty
--   foldrValues fcn vb sortedXs

setToList :: Value -> Disco IErr Value
setToList s = do
  VBag xs <- whnfV s
  return . toDiscoList . map fst $ xs


bagToSet :: Value -> Disco IErr Value
bagToSet b = do
  VBag xs <- whnfV b
  return $ VBag (map (\(v,_) -> (v,1)) xs)

bagToList :: Value -> Disco IErr Value
bagToList b = do
  VBag xs <- whnfV b
  return . toDiscoList . concatMap (uncurry (flip (replicate . fromIntegral))) $ xs
    -- XXX could be more efficient if we add some sharing, so replicated values
    -- will only be evaluated once

-- | Convert a list to a set, sorting the values and removing
--   duplicates.
listToSet :: Type -> Value -> Disco IErr Value
listToSet ty v = do
  vs <- fromDiscoList v
  vcs <- countValues ty vs
  return . VBag $ (map . fmap) (const 1) vcs

-- | Convert a list to a bag, sorting and counting the values.
listToBag :: Type -> Value -> Disco IErr Value
listToBag ty v = do
  vs <- fromDiscoList v
  VBag <$> countValues ty vs

-- | Map a function over a list.
primMapList :: Value -> Value -> Disco IErr Value
primMapList f xs = do
  f' <- whnfV f
  vmap (\v -> whnfApp f' [v]) xs

-- | Map a function over a bag.  The type argument is the /output/
--   type of the function.
primMapBag :: Type -> Value -> Value -> Disco IErr Value
primMapBag ty f xs = do
  f'       <- whnfV f
  VBag cts <- whnfV xs
  cts' <- mapM (\(v,n) -> (,n) <$> whnfApp f' [v]) cts
  VBag <$> sortNCount (decideOrdFor ty) cts'

-- | Map a function over a bag.  The type argument is the /output/
--   type of the function.
primMapSet :: Type -> Value -> Value -> Disco IErr Value
primMapSet ty f xs = do
  f'       <- whnfV f
  VBag cts <- whnfV xs
  cts' <- mapM (\(v,n) -> (,n) <$> whnfApp f' [v]) cts
  (VBag . map (second (const 1))) <$> sortNCount (decideOrdFor ty) cts'

-- | Reduce a list according to a given combining function and base
--   case value.
primReduceList :: Value -> Value -> Value -> Disco IErr Value
primReduceList f z xs = do
  f' <- whnfV f
  vfoldr (\a b -> whnfApp f' [a,b]) z xs

-- | Reduce a bag (or set) according to a given combining function and
--   base case value.
primReduceBag :: Value -> Value -> Value -> Disco IErr Value
primReduceBag f z b = do
  f' <- whnfV f
  VBag cts <- whnfV b
  let xs = toDiscoList $ concatMap (\(x,n) -> replicate (fromIntegral n) x) cts
  vfoldr (\a b -> whnfApp f' [a,b]) z xs

  -- XXX this is super inefficient! (1) should have some sharing so
  -- replicated elements of bag aren't recomputed; (2) shouldn't have
  -- to go via toDiscoList -> vfoldr, just do a monadic fold directly
  -- in Haskell

-- | Turn a function argument into a Value according to its given
--   strictness: via 'whnf' if Strict, and as a 'Thunk' if not.
whnfArg :: Strictness -> Core -> Disco IErr Value
whnfArg Strict = whnf
whnfArg Lazy   = mkThunk

-- | Find the arity of a function-like thing.  Note that the input
--   must already be in WHNF.
funArity :: Value -> Int
funArity (VClos b _) = length (fst (unsafeUnbind b))
funArity (VPAp f vs) = funArity f - length vs
funArity (VConst op) = opArity op
funArity v           = error $ "Impossible! funArity on " ++ show v

-- | Reduce an application to weak head normal form (WHNF).
--   Precondition: the head of the application has already been
--   reduced to WHNF (which means it must be a closure, @VFun@, a
--   partial application (@VPAp@), or a constant (@VConst@)).
--
--   Note, however, that the arguments may or may not be reduced.
whnfApp :: Value -> [Value] -> Disco IErr Value
whnfApp (VPAp f vs1) vs2 = whnfApp f (vs1 ++ vs2)
whnfApp f vs =
  case compare k (length vs) of

      -- Exactly the right number of arguments.
      EQ -> whnfAppExact f vs

      -- More arguments than parameters: peel off the right number of arguments
      -- to pass, and apply the result to the rest.
      LT -> do
        let (vs1, vs2) = splitAt k vs
        f2 <- whnfAppExact f vs1
        whnfApp f2 vs2

      -- Fewer arguments than parameters: pack up the arguments so far
      -- in a partial application awaiting more.
      GT -> return $ VPAp f vs
  where
    k = funArity f

-- | Apply a function-thing (must be reduced to WHNF) to a list of
--   exactly the right number of arguments.
whnfAppExact :: Value -> [Value] -> Disco IErr Value
whnfAppExact (VClos b e) vs =
  lunbind b $ \(xs,t) -> withEnv e $ extends (M.fromList $ zip xs vs) $ whnf t
whnfAppExact (VFun f)    vs = mapM rnfV vs >>= \vs' -> whnfV (f vs')
whnfAppExact (VConst op) vs = whnfOp op vs
whnfAppExact v _ = error $ "Impossible! whnfAppExact on non-function " ++ show v

------------------------------------------------------------
-- Lists
------------------------------------------------------------

--------------------------------------------------
-- Utilities

-- | Convert a Haskell list of Values into a Value representing a
--   disco list.
toDiscoList :: [Value] -> Value
toDiscoList []       = VCons 0 []
toDiscoList (x : xs) = VCons 1 [x, toDiscoList xs]

-- | Convert a Value representing a disco list into a Haskell list of
--   Values.  Strict in the spine of the list.
fromDiscoList :: Value -> Disco IErr [Value]
fromDiscoList v =
  whnfV v >>= \case
    VCons 0 _       -> return []
    VCons 1 [x, xs] -> (x:) <$> fromDiscoList xs
    v'              -> error $ "Impossible!  fromDiscoList on non-list value " ++ show v'

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


ellipsis :: Ellipsis Value -> Value -> Disco IErr Value
ellipsis ell xs = do
  vs  <- mapM whnfV =<< fromDiscoList xs
  end <- traverse whnfV ell
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
-- Constant evaluation
------------------------------------------------------------

-- | Reduce an operator application to WHNF.
whnfOp :: Op -> [Value] -> Disco IErr Value
whnfOp OAdd            = arity2 "+"        $ numOp (+)
whnfOp ONeg            = arity1 "negate"   $ uNumOp negate
whnfOp OSqrt           = arity1 "sqrt"     $ uNumOp integerSqrt
whnfOp OLg             = arity1 "lg"       $ lgOp
whnfOp OFloor          = arity1 "floor"    $ uNumOp floorOp
whnfOp OCeil           = arity1 "ceil"     $ uNumOp ceilOp
whnfOp OAbs            = arity1 "abs"      $ uNumOp abs
whnfOp OMul            = arity2 "*"        $ numOp (*)
whnfOp ODiv            = arity2 "/"        $ numOp' divOp
whnfOp OExp            = arity2 "^"        $ numOp (\m n -> m ^^ numerator n)
  -- If the program typechecks, n will be an integer.
whnfOp OMod            = arity2 "mod"      $ numOp' modOp
whnfOp ODivides        = arity2 "divides"  $ numOp' (\m n -> return (mkEnum $ divides m n))
whnfOp OBinom          = arity2 "binom"    $ numOp binom
whnfOp OMultinom       = arity2 "multinom" $ multinomOp
whnfOp OFact           = arity1 "fact"     $ uNumOp' fact
whnfOp (OEq ty)        = arity2 "eqOp"     $ eqOp ty
whnfOp (OLt ty)        = arity2 "ltOp"     $ ltOp ty
whnfOp OEnum           = arity1 "enum"     $ enumOp
whnfOp OCount          = arity1 "count"    $ countOp

-- Modular operations, for finite types
whnfOp (OMDiv n)       = arity2 "modDiv"   $ modDiv n
whnfOp (OMExp n)       = arity2 "modExp"   $ modExp n
whnfOp (OMDivides n)   = arity2 "modDiv"   $ modDivides n

-- Set operations
whnfOp (OSize)         = arity1 "setSize"         $ setSize
whnfOp (OUnion  ty)    = arity2 "setUnion"        $ setUnion ty
whnfOp (OInter  ty)    = arity2 "setIntersection" $ setIntersection ty
whnfOp (ODiff   ty)    = arity2 "setDifference"   $ setDifference ty
whnfOp (OPowerSet ty)  = arity1 "powerSet"        $ powerSet ty
whnfOp (OSubset ty)    = arity2 "subset"          $ subsetTest ty

-- Bag operations
whnfOp ORep            = arity2 "repBag" $ repBag

-- Container conversions
whnfOp OBagToSet       = arity1 "bagToSet"  $ whnfV >=> bagToSet
whnfOp OBagToList      = arity1 "bagToList" $ whnfV >=> bagToList
whnfOp OSetToList      = arity1 "setToList" $ whnfV >=> setToList
whnfOp (OListToSet ty) = arity1 "listToSet" $ whnfV >=> listToSet ty
whnfOp (OListToBag ty) = arity1 "listToBag" $ whnfV >=> listToBag ty

whnfOp OMapList        = (arity2 "mapList"  $ primMapList  ) >=> whnfV
whnfOp (OMapBag ty)    = (arity2 "mapBag"   $ primMapBag ty) >=> whnfV
whnfOp (OMapSet ty)    = (arity2 "mapSet"   $ primMapSet ty) >=> whnfV

whnfOp OReduceList     = (arity3 "reduceList" $ primReduceList) >=> whnfV
whnfOp OReduceBag      = (arity3 "reduceBag"  $ primReduceBag ) >=> whnfV

-- List ellipsis
whnfOp OForever        = arity1 "forever"   $ ellipsis Forever
whnfOp OUntil          = arity2 "until"     $ ellipsis . Until

-- Number theory primitives
whnfOp OIsPrime        = arity1 "isPrime"   $ fmap primIsPrime . whnfV
whnfOp OFactor         = arity1 "factor"    $ whnfV >=> primFactor

-- Other primitives
whnfOp OCrash          = arity1 "crash"     $ fmap primCrash . whnfV

-- Identity
whnfOp OId             = arity1 "id" $ whnfV

-- | Convert an arity-1 function to the right shape to accept a list
--   of arguments; throw an error if the wrong number of arguments are
--   given.
arity1 :: String -> (Value -> Disco IErr Value) -> ([Value] -> Disco IErr Value)
arity1 _ f [v]   = f v
arity1 name _ vs = error $ arityError name vs

-- | Convert an arity-2 function to the right shape to accept a list
--   of arguments; throw an error if the wrong number of arguments are
--   given.
arity2 :: String -> (Value -> Value -> Disco IErr Value) -> ([Value] -> Disco IErr Value)
arity2 _ f [v1,v2] = f v1 v2
arity2 name _ vs   = error $ arityError name vs

arity3 :: String -> (Value -> Value -> Value -> Disco IErr Value) -> ([Value] -> Disco IErr Value)
arity3 _ f [v1,v2,v3] = f v1 v2 v3
arity3 name _ vs      = error $ arityError name vs

arityError :: String -> [Value] -> String
arityError name vs = error $ "Impossible! Wrong arity (" ++ show (length vs) ++ ") in " ++ name

-- | Compute the size of a set.
setSize :: Value -> Disco IErr Value
setSize v = do
  VBag xs <- whnfV v
  return $ vnum (fromIntegral $ sum (map snd xs))

-- | Compute the union of two sets.
setUnion :: Type -> Value -> Value -> Disco IErr Value
setUnion ty v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  zs <- merge max (decideOrdFor ty) xs ys
  return $ VBag zs

-- | Compute the intersection of two sets.
setIntersection :: Type -> Value -> Value -> Disco IErr Value
setIntersection ty v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  zs <- merge min (decideOrdFor ty) xs ys
  return $ VBag zs

-- | Compute the difference of two sets.
setDifference :: Type -> Value -> Value -> Disco IErr Value
setDifference ty v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  zs <- merge (\x y -> max 0 (x - y)) (decideOrdFor ty) xs ys
  return $ VBag zs

-- | Test whether one set is a subset of another.
subsetTest :: Type -> Value -> Value -> Disco IErr Value
subsetTest ty v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  ys' <- merge (max) (decideOrdFor ty) xs ys
  mkEnum <$> setEquality ty ys ys'

-- | Compute the power set of a set.
powerSet :: Type -> Value -> Disco IErr Value
powerSet ty v = do
  VBag xs <- whnfV v
  ys <- sortNCount (decideOrdFor (TySet ty)) (map (\x -> (VBag x, 1)) (subsequences xs))
  return $ VBag ys

-- | Create a bag having a certain number of copies of a single element.
repBag :: Value -> Value -> Disco IErr Value
repBag elt rep = do
  VNum _ r <- whnfV rep
  return $ VBag [(elt, numerator r)]

-- | Perform a numeric binary operation.
numOp :: (Rational -> Rational -> Rational) -> Value -> Value -> Disco IErr Value
numOp (#) = numOp' (\m n -> return (vnum (m # n)))

-- | A more general version of 'numOp' where the binary operation has
--   a result in the @Disco@ monad (/e.g./ for operations which can throw
--   a division by zero error).
numOp' :: (Rational -> Rational -> Disco IErr Value) -> Value -> Value -> Disco IErr Value
numOp' (#) v1 v2 = do
  VNum d1 m <- whnfV v1    -- If the program type checked this can
  VNum d2 n <- whnfV v2    -- never go wrong.
  res <- m # n
  case res of
    VNum _ r -> return $ VNum (d1 <> d2) r    -- Re-flag any resulting numeric value with
    _        -> return res                    --   the combination of the input flags.

-- | Perform a numeric unary operation.
uNumOp :: (Rational -> Rational) -> Value -> Disco IErr Value
uNumOp f = uNumOp' (return . f)

-- | Perform a numeric unary operation, with the ability to /e.g./
--   throw an error (used for factorial, which can overflow).
uNumOp' :: (Rational -> Disco IErr Rational) -> Value -> Disco IErr Value
uNumOp' f v = do
  VNum d m <- whnfV v
  VNum d <$> f m

-- | For performing modular division within a finite type.
modDiv :: Integer -> Value -> Value -> Disco IErr Value
modDiv n v1 v2 = do
  VNum _ a <- whnfV v1
  VNum _ b <- whnfV v2
  case invertSomeMod (numerator b `modulo` fromInteger n) of
    Just (SomeMod b') -> modOp (a * (getVal b' % 1)) (n % 1)
    Just (InfMod{})   -> error "Impossible! InfMod in modDiv"
    Nothing           -> throwError DivByZero

modDivides :: Integer -> Value -> Value -> Disco IErr Value
modDivides n v1 v2 = do
  VNum _ a <- whnfV v1
  VNum _ b <- whnfV v2
  return $ mkEnum $ divides (toRational (gcd (numerator a) n)) b

-- | For performing modular exponentiation within a finite type.
modExp :: Integer -> Value -> Value -> Disco IErr Value
modExp n v1 v2 = do
  VNum _ r1 <- whnfV v1
  VNum _ r2 <- whnfV v2
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

-- | Perform a count on the number of values for the given type.
countOp :: Value -> Disco IErr Value
countOp (VType ty) = case countType ty of
  Just num -> return $ VCons 1 [vnum (num % 1)]
  Nothing  -> return $ VCons 0 [VCons 0 []]
countOp v = error $ "Impossible! countOp on non-type " ++ show v

-- | Perform an enumeration of the values of a given type.
enumOp :: Value -> Disco IErr Value
enumOp (VType ty) = case countType ty of
  Just _  -> return $ (toDiscoList (enumerate ty))
  Nothing -> throwError $ InfiniteTy ty
enumOp v = error $ "Impossible! enumOp on non-type " ++ show v

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
lgOp :: Value -> Disco IErr Value
lgOp v = do
  VNum _ m <- whnfV v
  lgOp' m

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

multinomOp :: Value -> Value -> Disco IErr Value
multinomOp v1 v2 = do
  VNum _ n <- whnfV v1
  ks       <- rnfV  v2
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

-- | Semantics of the @$factor@ prim: turn a natural number into its
--   bag of prime factors.  Crash if called on 0, which does not have
--   a prime factorization.
primFactor :: Value -> Disco IErr Value
primFactor (VNum d (numerator -> n)) =
  case n of
    0 -> throwError (Crash "0 has no prime factorization!")
    _ -> return . VBag $ map ((VNum d . (%1)) *** fromIntegral) (factorise n)
primFactor _                         = error "impossible! primFactor on non-VNum"

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
eqOp :: Type -> Value -> Value -> Disco IErr Value
eqOp ty v1 v2 = mkEnum <$> decideEqFor ty v1 v2

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
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
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
      = VFun $ \case
          { [v] -> snd . fromJust' v . find (decideEqForRnf ty1 v . fst) $ zip vs1 outs
          ; vs  -> error $ "Impossible! Got " ++ show vs ++ " in enumerate.mkFun"
          }

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
  = all (\v -> decideEqForRnf ty2 (f1 [v]) (f2 [v])) (enumerate ty1)
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
      r1 <- whnfApp clos1 [v]
      r2 <- whnfApp clos2 [v]

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
ltOp :: Type -> Value -> Value -> Disco IErr Value
ltOp ty v1 v2 = (mkEnum . (==LT)) <$> decideOrdFor ty v1 v2

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
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
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
  o <- decideOrdFor ty x y
  case o of
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
      r1 <- whnfApp clos1 [v]
      r2 <- whnfApp clos2 [v]

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

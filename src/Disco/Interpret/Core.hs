{-# LANGUAGE NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interpret.Core
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A big-step, call-by-need interpreter for the desugared, untyped
-- Disco core language.
--
-----------------------------------------------------------------------------

module Disco.Interpret.Core
       (
         -- * Evaluation
         vnum, vint
       , mkEnum

         -- ** Full reduction
       , rnf, rnfV

         -- ** Weak head reduction
       , whnf, whnfV, whnfList

         -- * Container utilities
       , valuesToBag, valuesToSet, valuesToMap, mapToSet

         -- * Property testing
       , testProperty
       , runTest

         -- * Equality testing and enumeration
       , eqOp, primValEq
       , decideEqFor, decideEqForRnf, decideEqForClosures

         -- * Comparison testing
       , ltOp, primValOrd, decideOrdFor, decideOrdForClosures

         -- * Lists

       , toDiscoList
       , vfoldr, vappend, vconcat, vmap

         -- * Converting graphs to manipulable Haskell objects
       , graphSummary

       )
       where

import           Control.Arrow                           ((***))
import           Control.Monad                           (filterM, (>=>))
import           Data.Bifunctor                          (first, second)
import           Data.Char
import           Data.Coerce                             (coerce)
import qualified Data.Map                                as M
import           Data.Ratio

import           Unbound.Generics.LocallyNameless        (Embed, unembed)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Math.Combinatorics.Exact.Binomial       (choose)
import           Math.Combinatorics.Exact.Factorial      (factorial)

import           Math.NumberTheory.Moduli.Class          (SomeMod (..), getVal,
                                                          invertSomeMod, modulo,
                                                          powSomeMod)
import           Math.NumberTheory.Primes                (factorise, unPrime)
import           Math.NumberTheory.Primes.Testing        (isPrime)

import           Disco.Effects.LFresh
import           Disco.Effects.Store
import           Polysemy                                hiding (Embed)
import           Polysemy.Error

import           Disco.AST.Core
import           Disco.AST.Generic                       (Side (..), selectSide)
import           Disco.AST.Surface                       (Ellipsis (..),
                                                          fromTelescope)
import           Disco.AST.Typed                         (AProperty)
import           Disco.Compile                           (compileProperty)
import           Disco.Context
import           Disco.Enumerate
import           Disco.Error
import           Disco.Eval
import           Disco.Property
import           Disco.Types
import           Disco.Value

import           Math.OEIS                               (catalogNums,
                                                          extendSequence,
                                                          lookupSequence)

import           Algebra.Graph                           (Graph (Connect, Empty, Overlay, Vertex),
                                                          foldg)
import qualified Algebra.Graph.AdjacencyMap              as AdjMap


------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
vnum :: Rational -> Value
vnum = VNum mempty

-- | A convenience function for creating a default @VNum@ value with a
--   default (@Fractional@) flag.
vint :: Integer -> Value
vint = vnum . (% 1)

-- | Turn any instance of @Enum@ into a @Value@, by creating a
--   constructor with an index corresponding to the enum value.
mkEnum :: Enum e => e -> Value
mkEnum e = VInj (toEnum $ fromEnum e) VUnit

--------------------------------------------------
-- Reduced normal form
--------------------------------------------------

-- | Evaluate a @Core@ expression to reduced normal form, i.e. keep
--   reducing under constructors as much as possible.  In practice,
--   all this function actually does is turn the @Core@ expression
--   into a thunk and then call 'rnfV'.
rnf :: Members EvalEffects r => Core -> Sem r Value
rnf c = whnf c >>= rnfV

-- | Reduce a value to reduced normal form, i.e. keep reducing under
--   constructors as much as possible.
rnfV :: Members EvalEffects r => Value -> Sem r Value

-- The value is an injection: keep the tag and recursively
-- reduce all its contents.
rnfV (VInj s v)    = VInj s <$> rnfV v

-- The value is a pair: recursively reduce its contents.
rnfV (VPair v1 v2) = VPair <$> rnfV v1 <*> rnfV v2

-- If the value is a thunk (i.e. unevaluated expression), a delayed
-- computation, or an indirection (i.e. a pointer to a value), reduce
-- it one step using 'whnfV' and then recursively continue reducing
-- the result.
rnfV v@VThunk{}    = whnfV v >>= rnfV
rnfV v@VDelay{}    = whnfV v >>= rnfV
rnfV v@VIndir{}    = whnfV v >>= rnfV

-- Otherwise, the value is already in reduced normal form (for
-- example, it could be a number or a function).
rnfV v             = return v

--------------------------------------------------
-- Weak head normal form (WHNF)
--------------------------------------------------

-- | Reduce a value to weak head normal form, that is, reduce it just
--   enough to find out what its top-level constructor is.
whnfV :: Members EvalEffects r => Value -> Sem r Value

-- If the value is a thunk, use its stored environment and evaluate
-- the expression to WHNF.
whnfV v@(VThunk c e)          = do
  debug $ "whnfV " ++ show v
  withEnv e $ whnf c

-- If it is a delayed computation, we can't delay any longer: run it
-- in its stored environment and reduce the result to WHNF.
whnfV (VDelay imv _ e)      = withEnv e imv >>= whnfV

-- If it is an indirection, call 'whnfIndir' which will look up the
-- value it points to, reduce it, and store the result so it won't
-- have to be re-evaluated the next time loc is referenced.
whnfV (VIndir loc)          = whnfIndir loc

-- If it is an injection or a pair, all well and good, it is already
-- in WHNF---but at the same time make sure that any subparts are
-- either simple constants or are turned into indirections to new
-- memory cells.  This way, when the subparts are eventually
-- evaluated, the new memory cells can be updated with their result.
whnfV (VInj s v)            = VInj s <$> mkSimple v
whnfV (VPair v1 v2)         = VPair <$> mkSimple v1 <*> mkSimple v2

-- Arity 0 functions can be boiled down to their core values
whnfV (VConst op)
  | opArity op == 0 = whnfOp op []

-- Otherwise, the value is already in WHNF (it is a number, a
-- function, or a constructor).
whnfV v                     = return v


-- | Reduce the value stored at the given location to WHNF.  We need a
--   special function for this, because in addition to returning the
--   reduced value, we also update the memory location to contain it.
--   That way if anything else refers to the same location, it will
--   not need to be re-evaluated later.
whnfIndir :: Members EvalEffects r => Loc -> Sem r Value
whnfIndir loc = do
  debug $ "whnfIndir " ++ show loc
  Just c <- lookupStore loc
  case c of
    Cell v True  -> return v          -- Already evaluated, just return it
    Cell v False -> do
      v' <- whnfV v                   -- Needs to be reduced
      debug $ "  -> " ++ show v'
      insertStore loc (Cell v' True)  -- Update memory with the reduced value
      return v'                       -- Finally, return the value.


-- | Reduce a list to WHNF.  Note that in the case of a cons, this
--   actually requires two calls to whnfV --- one to evaluate the
--   injection at its head, and one to evaluate the pair inside it.
--
--   For convenience, this is written in a sort of
--   continuation-passing/fold style: in addition to the 'Value' to be
--   reduced, it takes two arguments representing the desired result
--   if it is the empty list, and the desired continuation if it is a
--   cons with a given head and tail.
whnfList :: Members EvalEffects r => Value -> Sem r a -> (Value -> Value -> Sem r a) -> Sem r a
whnfList v nil cons = whnfV v >>= \case
  VNil -> nil
  VInj R p -> whnfV p >>= \case
    VPair hd tl -> cons hd tl
    v'          -> whnfListError $ VInj R v'
  v' -> whnfListError v'

  where
    whnfListError z = error $ "Impossible! whnfList on non-list value " ++ show z

-- | Reduce a Core expression to weak head normal form.  This is where
--   the real work of interpreting happens.  Most rules are
--   uninteresting except for function application and case.
whnf :: Members EvalEffects r => Core -> Sem r Value

------------------------------------------------------------
-- Boring cases (variables, constants, constructors, lambdas)

-- To reduce a variable, look it up in the environment and reduce the
-- result.
whnf (CVar x) = do
  e <- getEnv
  case M.lookup (coerce x) e of
    Just v  -> whnfV v
    Nothing -> error $ "Impossible! Unbound variable while interpreting: " ++ show x
      -- We should never encounter an unbound variable at this stage
      -- if the program already typechecked.

-- Function constants don't reduce in and of themselves. Map & Graph's empty constructors are exceptions because they have arity 0
whnf (CConst x)     = return $ VConst x

-- Unit value is already in WHNF.
whnf CUnit          = return VUnit

-- Injections and pairs are already in WHNF, so just turn their contents into
-- thunks to be evaluated later when they are demanded.
whnf (CInj s c)     = VInj s <$> mkValue c
whnf (CPair c1 c2)  = VPair <$> mkValue c1 <*> mkValue c2

-- A number is in WHNF, just turn it into a VNum.
whnf (CNum d n)     = return $ VNum d n

-- A lambda abstraction is already in WHNF; just package it up as a
-- closure with the current environment.
whnf (CAbs b)       = VClos b <$> getEnv

-- Type constants don't reduce.
whnf (CType ty)     = return $ VType ty

------------------------------------------------------------
-- Interesting cases! (application, case)

-- To reduce an application:
whnf t@(CApp c1 c2)    = do

  debug $ "whnf " ++ show t

  -- First reduce the function to WHNF...
  v1 <- whnf c1

  -- Then either reduce each argument or turn it into a thunk,
  -- depending on the specified strictness.
  v2 <- uncurry whnfArg c2

  -- Finally, call 'whnfApp' to do the actual application.
  whnfApp v1 [v2]

-- See 'whnfCase' for case reduction logic.
whnf (CCase bs)     = whnfCase bs

-- Reduce under a test frame.
whnf (CTest vars c) = whnfTest (TestVars vars) c

------------------------------------------------------------
-- Function application
------------------------------------------------------------

-- | Turn a function argument into a Value according to its given
--   strictness: via 'whnf' if Strict, and as a 'Thunk' if not.
whnfArg :: Members EvalEffects r => Strictness -> Core -> Sem r Value
whnfArg Strict = whnf
whnfArg Lazy   = mkValue

-- | Find the arity of a function-like thing.  Note that the input
--   must already be in WHNF.
funArity :: Value -> Int
funArity (VClos b _) = length (fst (unsafeUnbind b))
funArity (VPAp f vs) = funArity f - length vs
funArity (VConst op) = opArity op
funArity (VFun _)    = 1
funArity v           = error $ "Impossible! funArity on " ++ show v

-- | Reduce an application to weak head normal form (WHNF).
--   Precondition: the head of the application has already been
--   reduced to WHNF (which means it must be a closure (@VClos@), an
--   embedded Haskell function (@VFun@), a partial application
--   (@VPAp@), or a function constant (@VConst@)).
--
--   Note, however, that the arguments may or may not be reduced.
whnfApp :: Members EvalEffects r => Value -> [Value] -> Sem r Value

-- A partial application is waiting for more arguments, so feed it the
-- additional arguments and call whnfApp again.
whnfApp (VPAp f vs1) vs2 = whnfApp f (vs1 ++ vs2)

-- Otherwise, decide what to do based on the arity of f and the number
-- of arguments supplied:
whnfApp f vs =
  case compare k (length vs) of

      -- Exactly the right number of arguments: call whnfAppExact.
      EQ -> whnfAppExact f vs

      -- More arguments than parameters: peel off the right number of
      -- arguments to pass, evaluate the exact application, and then
      -- apply the result to the remaining arguments.
      LT -> do
        let (vs1, vs2) = splitAt k vs
        f2 <- whnfAppExact f vs1
        whnfApp f2 vs2

      -- Fewer arguments than parameters: pack up the arguments so far
      -- in a partial application awaiting more.
      GT -> return $ VPAp f vs
  where
    k = funArity f

-- | Apply a function-thing (must be reduced to WHNF, either VClos,
--   VFun, or VConst) to a list of exactly the right number of
--   arguments.
whnfAppExact :: Members EvalEffects r => Value -> [Value] -> Sem r Value
whnfAppExact (VClos b e) vs  =
  lunbind b $ \(xs,t) -> withEnv e $ extends (M.fromList $ zip xs vs) $ whnf t
whnfAppExact (VFun f)    [v] = rnfV v >>= \v' -> whnfV (f v')
whnfAppExact (VFun _)    vs  =
  error $ "Impossible! whnfAppExact with " ++ show (length vs) ++ " arguments to a VFun"
whnfAppExact (VConst op) vs  = whnfOp op vs
whnfAppExact v _ = error $ "Impossible! whnfAppExact on non-function " ++ show v

------------------------------------------------------------
-- Case analysis
------------------------------------------------------------

-- | Reduce a case expression to weak head normal form.
whnfCase :: Members EvalEffects r => [CBranch] -> Sem r Value
whnfCase []     = throw NonExhaustive
whnfCase (b:bs) = do
  lunbind b $ \(gs, t) -> do
  res <- checkGuards (fromTelescope gs)
  case res of
    Nothing -> whnfCase bs
    Just e' -> extends e' $ whnf t

-- | Check a chain of guards on one branch of a case.  Returns
--   @Nothing@ if the guards fail to match, or a resulting environment
--   of bindings if they do match.
checkGuards :: Members EvalEffects r => [(Embed Core, CPattern)] -> Sem r (Maybe Env)
checkGuards [] = ok
checkGuards ((unembed -> c, p) : gs) = do
  v <- mkValue c
  res <- match v p
  case res of
    Nothing -> return Nothing
    Just e  -> extends e (fmap (M.union e) <$> checkGuards gs)

-- | Match a value against a pattern, returning an environment of
--   bindings if the match succeeds.
match :: Members EvalEffects r => Value -> CPattern -> Sem r (Maybe Env)
match v (CPVar x)      = return $ Just (M.singleton x v)
match _ CPWild         = ok
match v CPUnit         = whnfV v >> ok
    -- Matching the unit value is guaranteed to succeed without
    -- binding anything, but we still need to reduce in case v throws
    -- an error.

match v (CPInj s x)    = do
  VInj t v' <- whnfV v
  if s == t then return (Just $ M.singleton x v') else noMatch
match v (CPPair x1 x2) = do
  VPair v1 v2 <- whnfV v
  return . Just . M.fromList $ [(x1,v1), (x2,v2)]

-- | Convenience function: successfully match with no bindings.
ok :: Applicative f => f (Maybe Env)
ok = pure $ Just M.empty

-- | Convenience function: fail to match.
noMatch :: Applicative f => f (Maybe Env)
noMatch = pure Nothing

------------------------------------------------------------
-- Lists
------------------------------------------------------------

--------------------------------------------------
-- Utilities

-- | Convert a Haskell list of Values into a Value representing a
--   disco list.
toDiscoList :: Members EvalEffects r => [Value] -> Sem r Value
toDiscoList []       = return VNil
toDiscoList (x : xs) = do
  xv  <- mkSimple x
  xsv <- mkSimple =<< delay (toDiscoList xs)
  return $ VCons xv xsv

-- | Convert a Value representing a disco list into a Haskell list of
--   Values.  Strict in the spine of the list.
fromDiscoList :: Members EvalEffects r => Value -> Sem r [Value]
fromDiscoList v =
  whnfList v (return []) (\x xs -> (x:) <$> fromDiscoList xs)

-- | A lazy foldr on lists represented as 'Value's.  The entire
--   function is wrapped in a call to 'delay', so it does not actually
--   reduce the list argument to whnf until (potentially) forced by a
--   call to 'whnf' later.  That is, executing the resulting @Disco
--   Value@ just results in a 'VDelay'; forcing that @VDelay@ with
--   'whnf' will then cause the foldr to actually start executing.
vfoldr :: Members EvalEffects r => (forall r'. Members EvalEffects r' => Value -> Value -> Sem r' Value) -> Value -> Value -> Sem r Value
vfoldr f z xs = delay' [z,xs] $
  whnfList xs (return z) $ \vh vt -> f vh =<< vfoldr f z vt

-- | Lazy append on 'Value' lists, implemented via 'vfoldr'.
vappend :: Members EvalEffects r => Value -> Value -> Sem r Value
vappend xs ys = vfoldr (\h t -> return $ VCons h t) ys xs

-- | Lazy concat on 'Value' lists, implemented via 'vfoldr'.
vconcat :: Members EvalEffects r => Value -> Sem r Value
vconcat = vfoldr vappend VNil

-- | Lazy map on 'Value' lists, implemented via 'vfoldr'.
vmap :: Members EvalEffects r => (forall r'. Members EvalEffects r' => Value -> Sem r' Value) -> Value -> Sem r Value
vmap f = vfoldr (\h t -> f h >>= \h' -> return $ VCons h' t) VNil

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

ellipsis :: Members EvalEffects r => Ellipsis Value -> Value -> Sem r Value
ellipsis ell xs = do
  vs  <- mapM whnfV =<< fromDiscoList xs
  end <- traverse whnfV ell
  let (ds,rs)   = unzip (map fromVNum vs)
      (d, end') = traverse fromVNum end
  toDiscoList . map (VNum (mconcat ds <> d)) $ enumEllipsis rs end'
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
-- Containers
------------------------------------------------------------

--------------------------------------------------
-- Normalizing bags/sets

-- | Given a list of disco values, sort and collate them into a list
--   pairing each unique value with its count.  Used to
--   construct/normalize bags and sets.
countValues :: Members EvalEffects r => Type -> [Value] -> Sem r [(Value, Integer)]
countValues ty = sortNCount (decideOrdFor ty) . map (,1)

-- | Normalize a list of values where each value is paired with a
--   count, but there could be duplicate values.  This function uses
--   merge sort to sort the values according to the given comparison
--   function, adding the counts of multiple instances of the same
--   value.
sortNCount :: Monad m => (a -> a -> m Ordering) -> [(a,Integer)] -> m [(a, Integer)]
sortNCount _ []  = return []
sortNCount _ [x] = return [x]
sortNCount f xs  = do
  fstSorted <- sortNCount f firstHalf
  sndSorted <- sortNCount f secondHalf
  merge (+) f fstSorted sndSorted
  where
    (firstHalf, secondHalf) = splitAt n xs
    n = length xs `div` 2

-- | Convert a list of values to a bag.
valuesToBag :: Members EvalEffects r => Type -> [Value] -> Sem r Value
valuesToBag ty = fmap VBag . countValues ty

-- | Convert a list of values to a set.
valuesToSet :: Members EvalEffects r => Type -> [Value] -> Sem r Value
valuesToSet ty = fmap (VBag . map (second (const 1))) . countValues ty

-- | Convert a list of pairs of values to a map. Note that key values should be Simple
valuesToMap :: Members EvalEffects r => [Value] -> Sem r Value
valuesToMap l = VMap . M.fromList <$> mapM simpleKey l
  where
    simpleKey (VPair k v) = (,v) <$> toSimpleValue k
    simpleKey v'          = error $ "unexpected value " ++ show v' ++ " in simpleKey"

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
merge g = mergeM (\x y -> return (g x y))

mergeM :: Monad m
       => (Integer -> Integer -> m Integer)   -- ^ Function for combining counts
       -> (a -> a -> m Ordering)              -- ^ Comparison function
       -> [(a, Integer)] -> [(a, Integer)] -> m [(a, Integer)]
mergeM g cmp = go
  where
    go [] []         = return []
    go [] ((y,n):ys) = mergeCons y 0 n =<< go [] ys
    go ((x,n):xs) [] = mergeCons x n 0 =<< go xs []
    go ((x,n1):xs) ((y,n2): ys) = do
      o <- cmp x y
      case o of
        LT -> mergeCons x n1 0  =<< go xs ((y,n2):ys)
        EQ -> mergeCons x n1 n2 =<< go xs ys
        GT -> mergeCons y 0 n2  =<< go ((x,n1):xs) ys

    mergeCons a m1 m2 zs = do
      i <- g m1 m2
      case i of
        0 -> return zs
        n -> return ((a,n):zs)

--------------------------------------------------
-- Conversion

-- | Convert a set to a (sorted) list.
setToList :: Members EvalEffects r => Value -> Sem r Value
setToList s = do
  VBag xs <- whnfV s
  toDiscoList . map fst $ xs

-- | Convert a bag to a set, by setting the count of every element
--   to 1.
bagToSet :: Members EvalEffects r => Value -> Sem r Value
bagToSet b = do
  VBag xs <- whnfV b
  return $ VBag (map (\(v,_) -> (v,1)) xs)

-- | Convert a bag to a list, duplicating any elements with a count
--   greater than 1.
bagToList :: Members EvalEffects r => Value -> Sem r Value
bagToList b = do
  VBag xs <- whnfV b
  toDiscoList . concatMap (uncurry (flip (replicate . fromIntegral))) $ xs
    -- XXX could be more efficient if we add some sharing? so
    -- replicated values will only be evaluated once

-- | Convert a list to a set, sorting the values and removing
--   duplicates.  Takes the type of the elements as an additional
--   argument, since it needs to know how to order them.
listToSet :: Members EvalEffects r => Type -> Value -> Sem r Value
listToSet ty v = do
  vs <- fromDiscoList v
  vcs <- countValues ty vs
  return . VBag $ (map . fmap) (const 1) vcs

-- | Convert a list to a bag, sorting and counting the values. Takes
--   the type of the elements as an additional argument, since it
--   needs to know how to order them.
listToBag :: Members EvalEffects r => Type -> Value -> Sem r Value
listToBag ty v = do
  vs <- fromDiscoList v
  VBag <$> countValues ty vs

-- | Convert a map to a set of pairs.
mapToSet :: Members EvalEffects r => Type -> Type -> Value -> Sem r Value
mapToSet tyk tyv (VMap val) = do
  vcs <- countValues (tyk :*: tyv) . map (\(k,v) -> VPair (fromSimpleValue k) v) $ M.toList val
  return $ VBag $ (map . fmap) (const 1) vcs
mapToSet _ _ v' = error $ "unexpected value " ++ show v' ++ " in mapToSet"

-- | Convert a set of pairs to a map.
setToMap :: Members EvalEffects r => Value -> Sem r Value
setToMap (VBag cs) = do
  let kvs = map fst cs
  kvs' <- mapM (whnfV >=> convertAssoc) kvs
  return . VMap . M.fromList $ kvs'

  where
    convertAssoc (VPair k v) = (,v) <$> toSimpleValue k
    convertAssoc v           = error $ "unexpected value " ++ show v ++ " in setToMap.convertAssoc"
setToMap v' = error $ "unexpected value " ++ show v' ++ " in setToMap"

-- | Convert a bag to a set of pairs, with each element paired with
--   its count.
primBagCounts :: Members EvalEffects r => Value -> Sem r Value
primBagCounts b = do
  VBag cs <- whnfV b
  return $ VBag (map (\(x,n) -> (VPair x (vint n), 1)) cs)

-- | Take a set of pairs consisting of values paired with a natural
--   number count, and convert to a bag.  Note the counts need not be
--   positive, and the elements need not be distinct.
primBagFromCounts :: Members EvalEffects r => Type -> Value -> Sem r Value
primBagFromCounts ty b = do
  VBag cs <- whnfV b
  cs' <- mapM getCount cs
  VBag <$> sortNCount (decideOrdFor ty) cs'

  where
    getCount (cnt, k) = do
      VPair x nv <- whnfV cnt
      VNum _ n <- whnfV nv
      return (x, numerator n * k)

--------------------------------------------------
-- Map

-- | Map a function over a list.
primEachList :: Members EvalEffects r => Value -> Value -> Sem r Value
primEachList f xs = do
  f' <- whnfV f
  vmap (\v -> whnfApp f' [v]) xs

-- | Map a function over a bag.  The type argument is the /output/
--   type of the function.
primEachBag :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
primEachBag ty f xs = do
  f'       <- whnfV f
  VBag cts <- whnfV xs
  cts' <- mapM (\(v,n) -> (,n) <$> whnfApp f' [v]) cts
  VBag <$> sortNCount (decideOrdFor ty) cts'

-- | Map a function over a bag.  The type argument is the /output/
--   type of the function.
primEachSet :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
primEachSet ty f xs = do
  f'       <- whnfV f
  VBag cts <- whnfV xs
  cts' <- mapM (\(v,n) -> (,n) <$> whnfApp f' [v]) cts
  VBag . map (second (const 1)) <$> sortNCount (decideOrdFor ty) cts'

--------------------------------------------------
-- Reduce

-- | Reduce a list according to a given combining function and base
--   case value.
primReduceList :: Members EvalEffects r => Value -> Value -> Value -> Sem r Value
primReduceList f z xs = do
  f' <- whnfV f
  vfoldr (\a b -> whnfApp f' [VPair a b]) z xs

-- | Reduce a bag (or set) according to a given combining function and
--   base case value.
primReduceBag :: Members EvalEffects r => Value -> Value -> Value -> Sem r Value
primReduceBag f z b = do
  f' <- whnfV f
  VBag cts <- whnfV b
  xs <- toDiscoList $ concatMap (\(x,n) -> replicate (fromIntegral n) x) cts
  vfoldr (\a r -> whnfApp f' [VPair a r]) z xs

  -- XXX this is super inefficient! (1) should have some sharing so
  -- replicated elements of bag aren't recomputed; (2) shouldn't have
  -- to go via toDiscoList -> vfoldr, just do a monadic fold directly
  -- in Haskell

--------------------------------------------------
-- Filter

-- | Filter a list according to a given predicate.
primFilterList :: Members EvalEffects r => Value -> Value -> Sem r Value
primFilterList p xs = do
  p' <- whnfV p
  vfoldr (filterOne p') VNil xs

  where
    filterOne :: Members EvalEffects r => Value -> Value -> Value -> Sem r Value
    filterOne p' a as = do
      b <- testPredicate p' a
      if b then return $ VCons a as else return as

-- | Filter a bag (or set) according to a given predicate.
primFilterBag :: Members EvalEffects r => Value -> Value -> Sem r Value
primFilterBag p b = do
  p' <- whnfV p
  VBag cs <- whnfV b
  VBag <$> filterM (testPredicate p' . fst) cs

testPredicate :: Members EvalEffects r => Value -> Value -> Sem r Bool
testPredicate p' x = do
  b <- whnfApp p' [x]
  case b of
    VInj L _ -> return False
    _        -> return True

--------------------------------------------------
-- Join

primBagUnions :: Members EvalEffects r => Type -> Value -> Sem r Value
primBagUnions ty bbs = do
  VBag cts <- whnfV bbs
  bs <- mapM (\(b,n) -> (,n) <$> whnfV b) cts
  VBag <$> sortNCount (decideOrdFor ty) [(x, m*n) | (VBag xs, n) <- bs, (x,m) <- xs]

primUnions :: Members EvalEffects r => Type -> Value -> Sem r Value
primUnions ty s = do
  VBag cts <- whnfV s
  ss <- mapM (whnfV . fst) cts
  valuesToSet ty [ x | VBag xs <- ss, (x,_) <- xs ]

--------------------------------------------------
-- Merge

primMerge :: Members EvalEffects r => Type -> Value -> Value -> Value -> Sem r Value
primMerge ty m b1 b2 = do
  m' <- whnfV m
  VBag xs <- whnfV b1
  VBag ys <- whnfV b2
  VBag <$> mergeM (mkMergeFun m') (decideOrdFor ty) xs ys

  where
    mkMergeFun m' i j = do
      VNum _ r <- whnfApp m' [VPair (vint i) (vint j)]
      return (numerator r)

------------------------------------------------------------
-- Set and bag operations

-- | Compute the size of a set or bag.
ctrSize :: Members EvalEffects r => Value -> Sem r Value
ctrSize v = do
  VBag xs <- whnfV v
  return $ vint (sum (map snd xs))

-- | Compute the power set/bag of a set/bag.
power :: Members EvalEffects r => Type -> Value -> Sem r Value
power ty v = do
  VBag xs <- whnfV v
  ys <- sortNCount (decideOrdFor (TyBag ty)) (map (first VBag) (choices xs))
  return $ VBag ys

  where
    choices :: [(Value, Integer)] -> [([(Value, Integer)], Integer)]
    choices [] = [([], 1)]
    choices ((x, n) : xs) = xs' ++ concatMap (\k -> map (cons n (x,k)) xs') [1 .. n]
      where
        xs' = choices xs
    cons n (x,k) (zs, m) = ((x,k):zs , choose n k * m)

-- | Test whether a given value is an element of a bag or set.
bagElem :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
bagElem ty x b = do
  VBag xs <- whnfV b
  mkEnum <$> elemOf (map fst xs)

  where
    elemOf [] = return False
    elemOf (y:ys) = do
      eq <- decideEqFor ty x y
      if eq then return True else elemOf ys

-- | Test whether a given value is an element of a list.
listElem :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
listElem ty x xs = do
  whnfList xs (return $ mkEnum False) $ \y ys -> do
    eq <- decideEqFor ty x y
    if eq then return $ mkEnum True else listElem ty x ys

------------------------------------------------------------
-- Constant evaluation
------------------------------------------------------------

-- | Reduce an operator application to WHNF.
whnfOp :: Members EvalEffects r => Op -> [Value] -> Sem r Value

--------------------------------------------------
-- Arithmetic

whnfOp OAdd            = arity2 "+"        $ numOp (+)
whnfOp ONeg            = arity1 "negate"   $ uNumOp negate
whnfOp OSqrt           = arity1 "sqrt"     $ uNumOp integerSqrt
whnfOp OFloor          = arity1 "floor"    $ uNumOp floorOp
whnfOp OCeil           = arity1 "ceil"     $ uNumOp ceilOp
whnfOp OAbs            = arity1 "abs"      $ uNumOp abs
whnfOp OMul            = arity2 "*"        $ numOp (*)
whnfOp ODiv            = arity2 "/"        $ numOp' divOp
whnfOp OExp            = arity2 "^"        $ numOp (\m n -> m ^^ numerator n)
whnfOp OMod            = arity2 "mod"      $ numOp' modOp
whnfOp ODivides        = arity2 "divides"  $ numOp' (\m n -> return (mkEnum $ divides m n))

--------------------------------------------------
-- Modular arithmetic for finite types

whnfOp (OMDiv n)       = arity2 "modDiv"   $ modDiv n
whnfOp (OMExp n)       = arity2 "modExp"   $ modExp n
whnfOp (OMDivides n)   = arity2 "modDiv"   $ modDivides n

--------------------------------------------------
-- Number theory

whnfOp OIsPrime        = arity1 "isPrime"   $ fmap primIsPrime . whnfV
whnfOp OFactor         = arity1 "factor"    $ whnfV >=> primFactor
whnfOp OFrac           = arity1 "frac"      $ whnfV >=> primFrac

--------------------------------------------------
-- Combinatorics

whnfOp OMultinom       = arity2 "multinom" multinomOp
whnfOp OFact           = arity1 "fact"     $ uNumOp' fact
whnfOp OEnum           = arity1 "enum"     enumOp
whnfOp OCount          = arity1 "count"    countOp

--------------------------------------------------
-- Graphs

whnfOp OSummary         = arity1 "graphSummary"  graphSummary
whnfOp (OEmptyGraph ty) = arity0 "emptyGraph"    $ newGraph ty Empty
whnfOp (OVertex ty)     = arity1 "graphVertex"   $ whnfV >=> toSimpleValue >=> graphVertex ty
whnfOp (OOverlay ty)    = arity2 "graphOverlay"  $ graphOverlay ty
whnfOp (OConnect ty)    = arity2 "graphConnect"  $ graphConnect ty

--------------------------------------------------
-- Maps

whnfOp OEmptyMap       = arity0 "emptyMap"  $ return (VMap M.empty)
whnfOp OInsert         = arity3 "mapInsert" mapInsert
whnfOp OLookup         = arity2 "mapLookup" mapLookup

--------------------------------------------------
-- Comparison
whnfOp (OEq ty)        = arity2 "eqOp"     $ eqOp ty
whnfOp (OLt ty)        = arity2 "ltOp"     $ ltOp ty

--------------------------------------------------
-- Container operations

whnfOp OSize           = arity1 "ctrSize" ctrSize
whnfOp (OPower ty)     = arity1 "power"    $ power ty
whnfOp (OBagElem ty)   = arity2 "bagElem"  $ bagElem ty
whnfOp (OListElem ty)  = arity2 "listElem" $ listElem ty

--------------------------------------------------
-- Container conversions

whnfOp OBagToSet       = arity1 "bagToSet"  $ whnfV >=> bagToSet
whnfOp OBagToList      = arity1 "bagToList" $ whnfV >=> bagToList
whnfOp OSetToList      = arity1 "setToList" $ whnfV >=> setToList
whnfOp (OListToSet ty) = arity1 "listToSet" $ whnfV >=> listToSet ty
whnfOp (OListToBag ty) = arity1 "listToBag" $ whnfV >=> listToBag ty

whnfOp OBagToCounts    = arity1 "bagCounts" primBagCounts
whnfOp (OCountsToBag ty) = arity1 "bagFromCounts" $ primBagFromCounts ty

whnfOp (OMapToSet tyK tyV) = arity1 "mapToSet" $ whnfV >=> mapToSet tyK tyV
whnfOp OSetToMap           = arity1 "map"      $ whnfV >=> setToMap

--------------------------------------------------
-- Each/reduce

whnfOp OEachList        = arity2 "eachList"  primEachList >=> whnfV
whnfOp (OEachBag ty)    = arity2 "eachBag" (primEachBag ty) >=> whnfV
whnfOp (OEachSet ty)    = arity2 "eachSet" (primEachSet ty) >=> whnfV

whnfOp OReduceList     = arity3 "reduceList" primReduceList >=> whnfV
whnfOp OReduceBag      = arity3 "reduceBag"  primReduceBag >=> whnfV

--------------------------------------------------
-- Filter

whnfOp OFilterList     = arity2 "filterList" primFilterList >=> whnfV
whnfOp OFilterBag      = arity2 "filterBag"  primFilterBag  >=> whnfV

--------------------------------------------------
-- Join

whnfOp OConcat         = arity1 "concat"   vconcat >=> whnfV
whnfOp (OBagUnions ty) = arity1 "bagUnions" $ primBagUnions ty
whnfOp (OUnions ty)    = arity1 "unions"    $ primUnions ty

--------------------------------------------------
-- Merge

whnfOp (OMerge ty)     = arity3 "merge"     $ primMerge ty

--------------------------------------------------
-- Ellipsis

whnfOp OForever        = arity1 "forever"   $ ellipsis Forever
whnfOp OUntil          = arity2 "until"     $ ellipsis . Until

--------------------------------------------------
-- Propositions

whnfOp (OExists tys)   = arity1 "exists"    $ (primExists tys <$>) . whnfV
whnfOp (OForall tys)   = arity1 "forall"    $ (primForall tys <$>) . whnfV
whnfOp OHolds          = arity1 "holds"     $ whnfV >=> primHolds
whnfOp ONotProp        = arity1 "notProp"   $ whnfV >=> primNotProp
whnfOp (OShouldEq ty)  = arity2 "shouldEq"  $ shouldEqOp ty

--------------------------------------------------
-- Other primitives

whnfOp OCrash          = arity1 "crash"     primCrash
whnfOp OId             = arity1 "id"        whnfV

whnfOp OExtendSeq      = arity1 "extendSequence" oeisExtend
whnfOp OLookupSeq      = arity1 "lookupSequence" oeisLookup

--------------------------------------------------
-- Utility functions

-- | Convert a constant ("arity-0 function") to the right shape to
--   accept a list of arguments; throw an error if the wrong number of
--   arguments are given.
arity0 :: String -> r -> [Value] -> r
arity0 _    x [] = x
arity0 name _ vs = error $ arityError name vs

-- | Convert an arity-1 function to the right shape to accept a list
--   of arguments; throw an error if the wrong number of arguments are
--   given.
arity1 :: String -> (Value -> r) -> ([Value] -> r)
arity1 _ f [v]   = f v
arity1 name _ vs = error $ arityError name vs

-- | Convert an arity-2 function to the right shape to accept a tuple
--   of arguments; throw an error if the wrong number of arguments are
--   given.
arity2 :: String -> (Value -> Value -> r) -> ([Value] -> r)
arity2 _ f [VPair v1 v2] = f v1 v2
arity2 name _ vs         = error $ arityError name vs

-- | Convert an arity-3 function to the right shape to accept a tuple
--   of arguments; throw an error if the wrong number of arguments are
--   given.
arity3 :: String -> (Value -> Value -> Value -> r) -> ([Value] -> r)
arity3 _ f [VPair v1 (VPair v2 v3)] = f v1 v2 v3
arity3 name _ vs                    = error $ arityError name vs

-- | Construct an error message for reporting an incorrect arity.
arityError :: String -> [Value] -> String
arityError name vs = error $ "Impossible! Wrong arity (" ++ show vs ++ ") in " ++ name

------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------

-- | Perform a numeric binary operation.
numOp :: Members EvalEffects r => (Rational -> Rational -> Rational) -> Value -> Value -> Sem r Value
numOp (#) = numOp' (\m n -> return (vnum (m # n)))

-- | A more general version of 'numOp' where the binary operation has
--   a result in the @Disco@ monad (/e.g./ for operations which can throw
--   a division by zero error).
numOp' :: Members EvalEffects r => (Rational -> Rational -> Sem r Value) -> Value -> Value -> Sem r Value
numOp' (#) v1 v2 = do
  VNum d1 m <- whnfV v1    -- If the program type checked this can
  VNum d2 n <- whnfV v2    -- never go wrong.
  res <- m # n
  case res of
    VNum _ r -> return $ VNum (d1 <> d2) r    -- Re-flag any resulting numeric value with
    _        -> return res                    --   the combination of the input flags.

-- | Perform a numeric unary operation.
uNumOp :: Members EvalEffects r => (Rational -> Rational) -> Value -> Sem r Value
uNumOp f = uNumOp' (return . f)

-- | Perform a numeric unary operation, with the ability to /e.g./
--   throw an error (used for factorial, which can overflow).
uNumOp' :: Members EvalEffects r => (Rational -> Sem r Rational) -> Value -> Sem r Value
uNumOp' f v = do
  VNum d m <- whnfV v
  VNum d <$> f m

-- | For performing modular division within a finite type.
modDiv :: Members EvalEffects r => Integer -> Value -> Value -> Sem r Value
modDiv n v1 v2 = do
  VNum _ a <- whnfV v1
  VNum _ b <- whnfV v2
  case invertSomeMod (numerator b `modulo` fromInteger n) of
    Just (SomeMod b') -> modOp (a * (getVal b' % 1)) (n % 1)
    Just InfMod{}     -> error "Impossible! InfMod in modDiv"
    Nothing           -> throw DivByZero

modDivides :: Members EvalEffects r => Integer -> Value -> Value -> Sem r Value
modDivides n v1 v2 = do
  VNum _ a <- whnfV v1
  VNum _ b <- whnfV v2
  return $ mkEnum $ divides (toRational (gcd (numerator a) n)) b

-- | For performing modular exponentiation within a finite type.
modExp :: Members EvalEffects r => Integer -> Value -> Value -> Sem r Value
modExp n v1 v2 = do
  VNum _ r1 <- whnfV v1
  VNum _ r2 <- whnfV v2
  let base = numerator r1 `modulo` fromInteger n
      ma = if numerator r2 >= 0
             then Just base
             else invertSomeMod base
      b = abs (numerator r2)
  case ma of
    Nothing -> throw DivByZero
    Just a  ->
      case powSomeMod a b of
        SomeMod v' -> return $ vint (getVal v')
        InfMod {}  -> error "Impossible, got InfMod in modExp"

-- | Perform a count on the number of values for the given type.
countOp :: Members EvalEffects r => Value -> Sem r Value
countOp (VType ty) = case countType ty of
  Just num -> return $ VInj R (vint num)
  Nothing  -> return VNil
countOp v = error $ "Impossible! countOp on non-type " ++ show v

-- | Perform an enumeration of the values of a given type.
enumOp :: Members EvalEffects r => Value -> Sem r Value
enumOp (VType ty) = toDiscoList (enumerateType ty)
enumOp v          = error $ "Impossible! enumOp on non-type " ++ show v

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
floorOp n = floor n % 1

ceilOp :: Rational -> Rational
ceilOp n  = ceiling n % 1

-- | Perform a division. Throw a division by zero error if the second
--   argument is 0.
divOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
divOp _ 0 = throw DivByZero
divOp m n = return $ vnum (m / n)

-- | Perform a mod operation; throw division by zero error if the
--   second argument is zero.  Although this function takes two
--   'Rational' arguments, note that if the disco program typechecks
--   then the arguments must in fact be integers.
modOp :: Member (Error EvalError) r => Rational -> Rational -> Sem r Value
modOp m n
  | n == 0    = throw DivByZero
  | otherwise = return $ vint (numerator m `mod` numerator n)
                -- This is safe since if the program typechecks, mod will only ever be
                -- called on integral things.

-- | Test whether one number divides another.
divides :: Rational -> Rational -> Bool
divides 0 0 = True
divides 0 _ = False
divides x y = denominator (y / x) == 1

-- | Multinomial coefficient.  The first argument is a number, the
--   second is a list.
multinomOp :: Members EvalEffects r => Value -> Value -> Sem r Value
multinomOp v1 v2 = do
  VNum _ n <- whnfV v1
  ks       <- rnfV  v2
  return . vint $ multinomial (numerator n) (asList ks)
  where
    asList :: Value -> [Integer]
    asList (VInj L _)                     = []
    asList (VInj R (VPair (VNum _ k) ks)) = numerator k : asList ks
    asList v                              = error $ "multinomOp.asList " ++ show v

    multinomial :: Integer -> [Integer] -> Integer
    multinomial _ []     = 1
    multinomial n (k:ks)
      | k > n     = 0
      | otherwise = choose n k * multinomial (n-k) ks

-- | Factorial.  The argument will always be a natural number.
fact :: Member (Error EvalError) r => Rational -> Sem r Rational
fact (numerator -> n)
  | n > fromIntegral (maxBound :: Int) = throw Overflow
  | otherwise = return $ factorial (fromIntegral n) % 1

-- | Semantics of the @$isPrime@ prim: a relatively fast test for
--   primality using the 'isPrime' function from @arithmoi@ (trial
--   division + Baille-PSW).
primIsPrime :: Value -> Value
primIsPrime (VNum _ (numerator -> n)) = mkEnum (isPrime n)
primIsPrime _                         = error "impossible!  primIsPrime on non-VNum"

-- | Semantics of the @$factor@ prim: turn a natural number into its
--   bag of prime factors.  Crash if called on 0, which does not have
--   a prime factorization.
primFactor :: Member (Error EvalError) r => Value -> Sem r Value
primFactor (VNum d (numerator -> n)) =
  case n of
    0 -> throw (Crash "0 has no prime factorization!")
    _ -> return . VBag $ map ((VNum d . (%1) . unPrime) *** fromIntegral) (factorise n)
primFactor _                         = error "impossible! primFactor on non-VNum"

-- | Semantics of the @$frac@ prim: turn a rational number into a pair
--   of its numerator and denominator.
primFrac :: Value -> Sem r Value
primFrac (VNum d n) = return $ VPair (VNum d . (%1) $ p) (VNum d . (%1) $ q)
  where
    p = numerator n
    q = denominator n
primFrac _ = error "impossible! primFrac on non-VNum"

-- | Semantics of the @$crash@ prim, which crashes with a
--   user-supplied message.
primCrash :: Members EvalEffects r => Value -> Sem r Value
primCrash v = do
  s <- valueToString v
  throw (Crash s)

-- | Convert a Disco value representing a list of characters into a
--   Haskell 'String'.
valueToString :: Members EvalEffects r => Value -> Sem r String
valueToString = fmap toString . rnfV
  where
    toString (VInj L _)                     = ""
    toString (VInj R (VPair (VNum _ c) cs)) = chr (fromIntegral $ numerator c) : toString cs
    toString _ = "Impossible: valueToString.toString, non-list"

------------------------------------------------------------
-- Propositions
------------------------------------------------------------

-- ~~~~ Note [Counterexample reporting & test frames]
--
-- When a property test fails underneath a forall quantifier (or
-- succeeds under an exists), we may need to report the values of
-- the quantified variables to the user, via the `TestEnv` on the
-- `TestResult` of the proposition.
--
-- The regular evaluator environment doesn't have what we need,
-- which is (1) which variables should be reported, (2) their
-- types (without which we can't hope to print their values), and
-- (3) their original names as written by the user, before any
-- alpha renaming that the compiler may have done.
--
-- It would be a pain to keep track of this and make it available
-- at every program point where a prop might fail (including via
-- throwing an exception), so we don't. Instead every TestResult
-- initially has an empty TestEnv, and information on reportable
-- variables is added as that result bubbles up through enclosing
-- CTest expressions.
--
-- The compilation pipeline maintains the invariant that every
-- quantifier body will contain a CTest naming its pattern-bound
-- variables, directly inside the various lambdas and cases that
-- actually bind them. So we never have to worry about attaching
-- counterexample information except in `whnfTest`. We also don't
-- have to worry about a reportable variable being shadowed or
-- not yet bound at the site of an error or test failure, as long
-- as things are alright at the CTest frame that encloses it.

-- | Convert a @Value@ to a @ValProp@, embedding booleans if necessary.
ensureProp :: Monad m => Value -> m ValProp
ensureProp (VProp p)  = return p
ensureProp (VInj L _) = return $ VPDone (TestResult False TestBool emptyTestEnv)
ensureProp (VInj R _) = return $ VPDone (TestResult True TestBool emptyTestEnv)
ensureProp _          = error "ensureProp: non-prop value"

failTestOnError :: Member (Error EvalError) r => Sem r ValProp -> Sem r ValProp
failTestOnError m = catch m $ \e ->
  return $ VPDone (TestResult False (TestRuntimeError e) emptyTestEnv)

-- | Normalize under a test frame, augmenting the reported prop
--   with the frame's variables.
whnfTest :: Members EvalEffects r => TestVars -> Core -> Sem r Value
whnfTest vs c = do
  result <- failTestOnError (ensureProp =<< whnf c)
  e' <- getTestEnv vs
  return . VProp $ extendPropEnv e' result

primExists :: [Type] -> Value -> Value
primExists tys v = VProp (VPSearch SMExists tys v emptyTestEnv)

primForall :: [Type] -> Value -> Value
primForall tys v = VProp (VPSearch SMForall tys v emptyTestEnv)

-- | Assert the equality of two values.
shouldEqOp :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
shouldEqOp t x y = toProp <$> decideEqFor t x y
  where
    toProp b = VProp (VPDone (TestResult b (TestEqual t x y) emptyTestEnv))

-- | Convert a prop to a boolean by dropping its evidence.
primHolds :: Members EvalEffects r => Value -> Sem r Value
primHolds v = resultToBool =<< testProperty Exhaustive v
  where
    resultToBool :: Member (Error EvalError) r => TestResult -> Sem r Value
    resultToBool (TestResult _ (TestRuntimeError e) _) = throw e
    resultToBool (TestResult b _ _)                    = return $ mkEnum b

-- | Invert a prop, keeping its evidence or its suspended search.
primNotProp :: Members EvalEffects r => Value -> Sem r Value
primNotProp v = ensureProp v >>= \case
  VPDone r            -> return $ VProp $ VPDone $ invertPropResult r
  VPSearch sm tys p e -> return $ VProp $ VPSearch (invertMotive sm) tys p e

-- | Test whether a property holds on generated examples.
testProperty :: Members EvalEffects r => SearchType -> Value -> Sem r TestResult
testProperty initialSt v = whnfV v >>= ensureProp >>= checkProp
  where
    checkProp :: Members EvalEffects r => ValProp -> Sem r TestResult
    checkProp (VPDone r)            = return r
    checkProp (VPSearch sm tys f e) =
      extendResultEnv e <$> (generateSamples initialSt vals >>= go)
      where
        vals = enumTypes tys
        (SearchMotive (whenFound, wantsSuccess)) = sm

        go :: Members EvalEffects r => ([[Value]], SearchType) -> Sem r TestResult
        go ([], st)   = return $ TestResult (not whenFound) (TestNotFound st) emptyTestEnv
        go (x:xs, st) = do
          prop <- ensureProp =<< whnfApp f x
          case prop of
            VPDone r    -> continue st xs r
            VPSearch {} -> checkProp prop >>= continue st xs

        continue :: Members EvalEffects r => SearchType -> [[Value]] -> TestResult -> Sem r TestResult
        continue st xs r@(TestResult _ _ e')
          | testIsError r              = return r
          | testIsOk r == wantsSuccess =
            return $ TestResult whenFound (TestFound r) e'
          | otherwise                  = go (xs, st)

runTest :: Members EvalEffects r => Int -> AProperty -> Sem r TestResult
runTest n p = testProperty (Randomized n' n') =<< mkValue (compileProperty p)
  where
    n' = fromIntegral (n `div` 2)

------------------------------------------------------------
-- Equality testing
------------------------------------------------------------

-- | Test two expressions for equality at the given type.
eqOp :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
eqOp ty v1 v2 = mkEnum <$> decideEqFor ty v1 v2

{-# ANN decideEqFor "HLint: ignore Use head" #-}

-- | Lazily decide equality of two values at the given type.
decideEqFor :: Members EvalEffects r => Type -> Value -> Value -> Sem r Bool

-- To decide equality at a pair type:
decideEqFor ty@(ty1 :*: ty2) v1 v2 = do

  debug $ "decideEqFor " ++ show ty ++ " " ++ show v1 ++ " " ++ show v2

  -- First, reduce both values to WHNF, which will produce pairs.
  VPair v11 v12 <- whnfV v1
  VPair v21 v22 <- whnfV v2

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
decideEqFor ty@(ty1 :+: ty2) v1 v2 = do

  debug $ "decideEqFor " ++ show ty ++ " " ++ show v1 ++ " " ++ show v2

  -- Reduce both values to WHNF, which will produce constructors
  -- (either inl or inr) with one argument.
  VInj s1 v1' <- whnfV v1
  VInj s2 v2' <- whnfV v2

  -- Check whether the constructors are the same.
  case s1 == s2 of
    -- If not, the values are not equal.
    False -> return False
    -- If so, decide equality for the contained values at whichever
    -- type is appropriate.
    True  -> decideEqFor (selectSide s1 ty1 ty2) v1' v2'

-- To decide equality at an arrow type, (ty1 -> ty2):
decideEqFor (ty1 :->: ty2) v1 v2 = do

  -- Reduce both values to WHNF, which should produce closures (or
  -- @VFun@s).
  clos1 <- whnfV v1
  clos2 <- whnfV v2

  --  all the values of type ty1.
  let ty1s = enumerateType ty1

  -- Try evaluating the functions on each value and check whether they
  -- agree.
  decideEqForClosures ty2 clos1 clos2 ty1s

-- To decide equality at a list type, [elTy]:
decideEqFor ty@(TyList elTy) v1 v2 = do

  -- Reduce both values to WHNF; will be either nil with no arguments
  -- or cons with two.
  v1'@(VInj c1 l1) <- whnfV v1
  v2'@(VInj c2 l2) <- whnfV v2

  debug $ "decideEqFor " ++ show ty ++ ": " ++ show v1' ++ ", " ++ show v2'

  case (c1,c2) of
    (L,L) -> return True      -- Both are nil.
    (R,R) -> do               -- Both are cons.

      VPair h1 t1 <- whnfV l1
      VPair h2 t2 <- whnfV l2

      -- Check head equality.
      heq <- decideEqFor elTy h1 h2
      case heq of

        -- If heads are unequal, so are lists.
        False -> return False
        -- Otherwise, check tails for equality.
        True  -> decideEqFor (TyList elTy) t1 t2

    -- Different constructors => unequal.
    _     -> return False

decideEqFor (TySet ty) v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  bagEquality ty xs ys

decideEqFor (TyBag ty) v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  bagEquality ty xs ys

decideEqFor ty@TyGraph{} g h = (==EQ) <$> decideOrdFor ty g h

decideEqFor ty@TyMap{} m1 m2 = (==EQ) <$> decideOrdFor ty m1 m2

-- For any other type (Void, Unit, Bool, N, Z, Q), we can just decide
-- by looking at the values reduced to WHNF.
decideEqFor ty v1 v2 = do
  debug $ "decideEqFor fallthrough @ " ++ show ty ++ ": " ++ show v1 ++ ", " ++ show v2
  primValEq <$> whnfV v1 <*> whnfV v2

bagEquality :: Members EvalEffects r => Type -> [(Value, Integer)] -> [(Value, Integer)] -> Sem r Bool
bagEquality _ [] [] = return True
bagEquality _ [] _ = return False
bagEquality _ _ [] = return False
bagEquality ty ((x,n1):xs) ((y,n2):ys) = do
  eq <- (n1 == n2 &&) <$> decideEqFor ty x y
  if eq then bagEquality ty xs ys else return False

-- | Decide equality for two values at a given type, when we already
--   know the values are in RNF.  This means the result doesn't need
--   to be in the @Disco@ monad, because no evaluation needs to happen.
decideEqForRnf :: Type -> Value -> Value -> Bool
decideEqForRnf (ty1 :*: ty2) (VPair v11 v12) (VPair v21 v22)
  = decideEqForRnf ty1 v11 v21 && decideEqForRnf ty2 v12 v22
decideEqForRnf (ty1 :+: ty2) (VInj s1 v1') (VInj s2 v2')
  = s1 == s2 && decideEqForRnf (selectSide s1 ty1 ty2) v1' v2'
decideEqForRnf (ty1 :->: ty2) (VFun f1) (VFun f2)
  = all (\v -> decideEqForRnf ty2 (f1 v) (f2 v)) (enumerateType ty1)
decideEqForRnf _ v1 v2 = primValEq v1 v2

-- | @decideEqForClosures ty f1 f2 vs@ lazily decides whether the given
--   functions @f1@ and @f2@ produce the same output (of type @ty@) on
--   all inputs in @vs@.
decideEqForClosures :: Members EvalEffects r => Type -> Value -> Value -> [Value] -> Sem r Bool
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
primValEq VUnit VUnit                   = True
primValEq (VInj i VUnit) (VInj j VUnit) = i == j
primValEq (VNum _ n1)  (VNum _ n2)      = n1 == n2
primValEq v1 v2                         = error $ "primValEq on non-primitive values " ++ show v1 ++ " and " ++ show v2

------------------------------------------------------------
-- Comparison testing
------------------------------------------------------------

-- | Test two expressions to see whether the first is less than the
--   second at the given type.
ltOp :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
ltOp ty v1 v2 = mkEnum . (==LT) <$> decideOrdFor ty v1 v2

-- | Lazily decide the ordering of two values at the given type.
decideOrdFor :: Members EvalEffects r => Type -> Value -> Value -> Sem r Ordering

-- To decide the ordering of two pairs:
decideOrdFor (ty1 :*: ty2) v1 v2 = do

  -- Reduce both pairs to WHNF
  VPair v11 v12 <- whnfV v1
  VPair v21 v22 <- whnfV v2

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
decideOrdFor (ty1 :+: ty2) v1 v2 = do

  -- Reduce to WHNF
  VInj s1 v1' <- whnfV v1
  VInj s2 v2' <- whnfV v2

  -- Compare the constructors
  case compare s1 s2 of
    -- Only compare the contents if the constructors are equal
    EQ -> decideOrdFor (selectSide s1 ty1 ty2) v1' v2'

    -- Otherwise return the ordering of the constructors
    o  -> return o

-- To decide the ordering for two functions:
decideOrdFor (ty1 :->: ty2) v1 v2 = do

  -- Reduce both to WHNF
  clos1 <- whnfV v1
  clos2 <- whnfV v2

  -- Enumerate the values of the input type, and then order the
  -- functions by applying them both to each value in the enumeration
  -- in turn, returning the ordering on the first value where the
  -- functions differ.
  let ty1s = enumerateType ty1
  decideOrdForClosures ty2 clos1 clos2 ty1s

-- To decide the ordering for two lists:
decideOrdFor (TyList ty) v1 v2 = do

  -- Reduce both to WHNF
  l1 <- whnfV v1
  l2 <- whnfV v2

  -- Lexicographic ordering
  case (l1, l2) of
    (VInj L _, VInj L _) -> return EQ   -- Both empty list
    (VInj L _, VInj R _) -> return LT   -- Empty < cons
    (VInj R _, VInj L _) -> return GT   -- Cons > empty
    (VInj R p1, VInj R p2) -> do

      VPair x1 l1' <- whnfV p1
      VPair x2 l2' <- whnfV p2

      o <- decideOrdFor ty x1 x2
      case o of
        EQ -> decideOrdFor (TyList ty) l1' l2'
        _  -> return o
    _ -> error $ "Impossible! decideOrdFor " ++ show (TyList ty, v1, v2)

-- To decide the ordering for two sets:
decideOrdFor (TySet ty) v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  bagComparison ty xs ys

-- Deciding the ordering for two bags is the same.
decideOrdFor (TyBag ty) v1 v2 = do
  VBag xs <- whnfV v1
  VBag ys <- whnfV v2
  bagComparison ty xs ys

-- Graphs are compared directly
decideOrdFor (TyGraph _) g h = do
  VGraph g' _ <- whnfV g
  VGraph h' _ <- whnfV h
  return $ compare g' h'

-- Deciding the ordering for two maps is very similar to function ordering.
decideOrdFor (TyMap k v) m1 m2 = do
  VMap m1' <- whnfV m1
  VMap m2' <- whnfV m2
  go (M.assocs m1') (M.assocs m2')
  where
    go []    [] = return EQ
    go (_:_) [] = return GT
    go [] (_:_) = return LT
    go ((k1,v1):xs) ((k2,v2):ys) = do

      -- we want to invert the ordering of keys
      -- because if one map contains a higher key
      -- that means that it is actually missing
      -- the lesser key, so we think of it as being less
      kOrd <- decideOrdFor k (fromSimpleValue k2) (fromSimpleValue k1)
      vOrd <- decideOrdFor v v1 v2
      -- Order primarily on keys, and then on values
      let o = if kOrd == EQ then vOrd else kOrd
      case o of

        -- Recurse if all are EQ.
        EQ -> go xs ys

        -- Otherwise return the found ordering.
        _  -> return o

-- Otherwise we can compare the values primitively, without looking at
-- the type.
decideOrdFor _ v1 v2 = primValOrd <$> whnfV v1 <*> whnfV v2

-- Helper function which decides the order for two sets or bags.
bagComparison :: Members EvalEffects r => Type -> [(Value, Integer)] -> [(Value, Integer)] -> Sem r Ordering
bagComparison _ [] [] = return EQ
bagComparison _ _ [] = return GT
bagComparison _ [] _ = return LT
bagComparison ty ((x,xn):xs) ((y,yn):ys) = do
  o <- decideOrdFor ty x y
  case (o, compare xn yn) of
    (EQ, EQ)   -> bagComparison ty xs ys
    (EQ, o2)   -> return o2
    (other, _) -> return other

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
decideOrdForClosures :: Members EvalEffects r => Type -> Value -> Value -> [Value] -> Sem r Ordering
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
primValOrd VUnit VUnit                   = EQ
primValOrd (VInj i VUnit) (VInj j VUnit) = compare i j
primValOrd (VNum _ n1)  (VNum _ n2)      = compare n1 n2
primValOrd v1           v2
  = error $ "primValOrd: impossible! (got " ++ show v1 ++ ", " ++ show v2 ++ ")"

------------------------------------------------------------
-- SimpleValue Utilities
------------------------------------------------------------

toSimpleValue :: Members EvalEffects r => Value -> Sem r SimpleValue
toSimpleValue v = do
    v' <- whnfV v
    case v' of
        VNum d n    -> return $ SNum d n
        VUnit       -> return SUnit
        VInj s v1   -> SInj s <$> toSimpleValue v1
        VPair v1 v2 -> SPair <$> toSimpleValue v1 <*> toSimpleValue v2
        VBag bs     -> SBag <$> mapM (\(a,b) -> (,b) <$> toSimpleValue a) bs
        VType t     -> return $ SType t
        t           -> error $ "A non-simple value was passed as simple" ++ show t

fromSimpleValue :: SimpleValue -> Value
fromSimpleValue (SNum d n)    = VNum d n
fromSimpleValue SUnit         = VUnit
fromSimpleValue (SInj s v)    = VInj s (fromSimpleValue v)
fromSimpleValue (SPair v1 v2) = VPair (fromSimpleValue v1) (fromSimpleValue v2)
fromSimpleValue (SBag bs)     = VBag $ map (first fromSimpleValue) bs
fromSimpleValue (SType t)     = VType t

------------------------------------------------------------
-- OEIS
------------------------------------------------------------

-- | Looks up a sequence of integers in OEIS.
--   Returns 'left()' if the sequence is unknown in OEIS,
--   otherwise 'right "https://oeis.org/<oeis_sequence_id>"'
oeisLookup :: Members EvalEffects r => Value -> Sem r Value
oeisLookup v = do
    vs <- fromDiscoList v
    let hvs = toHaskellList vs
    case lookupSequence hvs of
      Just result -> parseResult result
      Nothing     -> return VNil
  where
    parseResult r = do
          let seqNum = getCatalogNum $ catalogNums r
          l <- toDiscoList $ toVal ("https://oeis.org/" ++ seqNum)
          return $ VInj R l -- right "https://oeis.org/foo"
    getCatalogNum []    = error "No catalog info"
    getCatalogNum (n:_) = n
    toVal = map (vint . toInteger . ord)

-- | Extends a Disco integer list with data from a known OEIS sequence.
--   Returns a list of integers upon success, otherwise the original list (unmodified).
oeisExtend :: Members EvalEffects r => Value -> Sem r Value
oeisExtend v = do
    vs <- fromDiscoList v
    let xs = toHaskellList vs
    let newseq = extendSequence xs
    toDiscoList $ map vint newseq

------------------------------------------------------------
-- Graph Utilities
------------------------------------------------------------

-- | Convert a Disco integer list to a Haskell list
toHaskellList :: [Value] -> [Integer]
toHaskellList [] = []
toHaskellList xs = map fromVNum xs
                   where
                      fromVNum (VNum _ x) = fromIntegral $ numerator x
                      fromVNum v          = error $ "Impossible!  fromVNum on " ++ show v

newGraph :: Members EvalEffects r => Type -> Graph SimpleValue -> Sem r Value
newGraph a g = do
  adj <- delay $ directlyReduceSummary a g
  loc <- allocate adj
  return $ VGraph g $ VIndir loc

toDiscoAdjMap :: Members EvalEffects r => Type -> [(SimpleValue, [SimpleValue])] -> Sem r Value
toDiscoAdjMap ty l =
    VMap . M.fromList <$>
    mapM (\(v,edges) -> do
              set <- valuesToSet ty $ map fromSimpleValue edges
              return (v,set)) l

reifyGraph :: Graph SimpleValue -> [(SimpleValue, [SimpleValue])]
reifyGraph =
    AdjMap.adjacencyList . foldg AdjMap.empty AdjMap.vertex AdjMap.overlay AdjMap.connect

-- Actually calculate the adjacency map, which we will store in the graph instances
directlyReduceSummary :: Members EvalEffects r => Type -> Graph SimpleValue -> Sem r Value
directlyReduceSummary ty = toDiscoAdjMap ty . reifyGraph

-- Lookup the stored adjacency map from the indirection stored in this graph
graphSummary :: Members EvalEffects r => Value -> Sem r Value
graphSummary g = do
  VGraph _ adj <- whnfV g
  whnfV adj

graphVertex :: Members EvalEffects r => Type -> SimpleValue -> Sem r Value
graphVertex a v = newGraph a $ Vertex v

graphOverlay :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
graphOverlay a g h = do
  VGraph g' _ <- whnfV g
  VGraph h' _ <- whnfV h
  newGraph a $ Overlay g' h'

graphConnect :: Members EvalEffects r => Type -> Value -> Value -> Sem r Value
graphConnect a g h = do
  VGraph g' _ <- whnfV g
  VGraph h' _ <- whnfV h
  newGraph a $ Connect g' h'

------------------------------------------------------------
-- Map Utilities
------------------------------------------------------------

mapInsert :: Members EvalEffects r => Value -> Value -> Value -> Sem r Value
mapInsert k v m = do
  VMap m' <- whnfV m
  k' <- toSimpleValue k
  return $ VMap $ M.insert k' v m'

mapLookup :: Members EvalEffects r => Value -> Value -> Sem r Value
mapLookup k m = do
  VMap m' <- whnfV m
  k' <- toSimpleValue k
  case M.lookup k' m' of
    Just v' -> return $ VInj R v'
    _       -> return VNil

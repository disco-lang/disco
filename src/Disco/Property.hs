{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Property
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Properties of disco functions.
--
-----------------------------------------------------------------------------

module Disco.Property
       where

import           Unbound.Generics.LocallyNameless (Name, lunbind)

import qualified Test.QuickCheck                  as QC

import           Control.Monad.Except
import           Data.Char                        (ord)
import           Data.Coerce
import           Data.List                        (transpose)
import qualified Data.Map                         as M
import           Data.Ratio
import           Data.Traversable                 (for)

import           Disco.AST.Core
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Eval
import           Disco.Interpret.Core
import           Disco.Syntax.Operators           (BOp (..))
import           Disco.Syntax.Prims
import           Disco.Types

--------------------------------------------------
-- Test results
--------------------------------------------------

-- | Test success can come either from exhaustively testing all
--   possible inputs, or from succeeding on a number of randomly
--   chosen inputs.
data SuccessType
  = Exhaustive
  | Randomized Integer

-- | The possible outcomes of a test.
data TestResult
  = TestOK SuccessType
    -- ^ The test succeeded.

  | TestRuntimeFailure IErr
    -- ^ The test failed at runtime.

  | TestFalse                            Env
    -- ^ The test evaluated to false.  The @Env@ records the
    --   particular inputs which caused the failure, /i.e./ a
    --   counterexample.

  | TestEqualityFailure Type Value Value Env
    -- ^ The test was an equality test, and evaluated to false.
    --   Records the type at which equality was tested and the two
    --   values we got on either side of the =, as well as the
    --   counterexample which led to the failure.

instance Semigroup SuccessType where
  Exhaustive <> s = s
  s <> Exhaustive = s
  Randomized m <> Randomized n = Randomized (m + n)

instance Monoid SuccessType where
  mempty  = Exhaustive
  mappend = (<>)

instance Semigroup TestResult where
  TestOK s1 <> TestOK s2 = TestOK (s1 <> s2)
  TestOK _ <> r          = r
  r        <> _          = r

instance Monoid TestResult where
  mempty  = TestOK mempty
  mappend = (<>)

testIsOK :: TestResult -> Bool
testIsOK (TestOK {}) = True
testIsOK _           = False

-- XXX do shrinking for randomly generated test cases

-- XXX don't reload defs every time?

-- | @runTest n defs prop@ test property @prop@, using at most @n@
--   randomly generated inputs.
runTest :: Int -> AProperty -> Disco IErr TestResult
runTest n aprop
  = flip catchError (return . TestRuntimeFailure) . fmap mconcat $ do
  lunbind aprop $ \(binds, at) -> do
    (exhaustive, envs) <- testCases n binds
    let success = if exhaustive then Exhaustive else Randomized 1
    for envs $ \env -> extendsEnv env $ do
      case getEquatands at of
        Nothing        -> do
          v <- evalTerm at
          case v of
            VCons 1 [] -> return $ TestOK success
            _          -> return $ TestFalse env
        Just (at1,at2) -> do
          v1 <- evalTerm at1
          v2 <- evalTerm at2
          v <- decideEqFor (getType at1) v1 v2
          case v of
            True  -> return $ TestOK success
            False -> return $ TestEqualityFailure (getType at1) v1 v2 env
  where
    evalTerm = rnf . compileTerm

-- | Check whether a term looks like a top-level equality test.
getEquatands :: ATerm -> Maybe (ATerm, ATerm)
getEquatands (ATApp _ (ATApp _ (ATPrim _ (PrimBOp Eq)) at1) at2) = Just (at1, at2)
getEquatands _ = Nothing

-- | @testCases n bindings@ generates at most n environments in which
--   to conduct tests.
--
--   * If @bindings@ is empty, only one test is
--     necessary, and @testCases@ returns a singleton list with the
--     empty environment.
--
--   * If the number of all possible combinations of values for
--     @bindings@ is at most @n@, then one environment is generated
--     for each combination, and @True@ is returned to signal that the
--     tests are exhaustive.
--
--   * Otherwise, @testCases@ generates exactly @n@ environments; in
--     each environment the given names are bound to randomly chosen
--     values.  The values in the first environment are simplest; they
--     become increasingly complex as the environments progress.
testCases :: Int -> [(Name ATerm, Type)] -> Disco IErr (Bool, [Env])
testCases _ []    = return (True, [M.empty])
testCases n binds
  | Just m <- fmap product . sequence . map countType $ tys
  , m <= (fromIntegral n)
  = return $ (True, map (M.fromList . zip ys) $ mapM enumerate tys)
    -- The above mapM is in the list monad!

  | otherwise = do
      valLists <- mapM (genValues n) tys
      return $ (False, map (M.fromList . zip ys) $ transpose valLists)
  where
    (xs, tys) = unzip binds
    ys :: [Name Core]
    ys = map coerce xs

------------------------------------------------------------
-- Random test case generation
------------------------------------------------------------

-- | A generator of disco values.
data DiscoGen where

  -- | A generator for an uninhabited type.
  EmptyGen :: DiscoGen

  -- | A @DiscoGen@ contains a QuickCheck generator of an
  --   existentially quantified type, and a way to turn that type into
  --   a disco 'Value'.
  DiscoGen :: QC.Gen a -> (a -> Disco IErr Value) -> DiscoGen

emptyGenerator :: DiscoGen
emptyGenerator = EmptyGen

unitGenerator :: DiscoGen
unitGenerator = DiscoGen (return ()) (return . const (VCons 0 []))

-- | Map a function over the values generated by a 'DiscoGen'.
mapDiscoGen :: (Value -> Value) -> DiscoGen -> DiscoGen
mapDiscoGen _ EmptyGen               = EmptyGen
mapDiscoGen f (DiscoGen gen toValue) = DiscoGen gen (fmap f . toValue)

-- | Create the 'DiscoGen' for a given type.
discoGenerator :: Type -> DiscoGen
discoGenerator TyN = DiscoGen
  (QC.arbitrary :: QC.Gen (QC.NonNegative Integer))
  (return . vnum . (%1) . QC.getNonNegative)
discoGenerator TyZ = DiscoGen
  (QC.arbitrary :: QC.Gen Integer)
  (return . vnum . (%1))
discoGenerator TyF = DiscoGen
  (QC.arbitrary :: QC.Gen (QC.NonNegative Integer, QC.Positive Integer))
  (\(QC.NonNegative m, QC.Positive n) -> return (vnum (m % (n+1))))
discoGenerator TyQ = DiscoGen
  (QC.arbitrary :: QC.Gen (Integer, QC.Positive Integer))
  (\(m, QC.Positive n) -> return (vnum (m % (n+1))))
discoGenerator TyC = DiscoGen
  (QC.arbitrary :: QC.Gen Char)
  (return . vnum . (%1) . toInteger . ord)

discoGenerator (TyFin 0) = emptyGenerator
discoGenerator (TyFin n) = DiscoGen
  (QC.choose (0,n-1) :: QC.Gen Integer)
  (return . vnum . (%1))

discoGenerator ty@(TyVar _)  = error $ "discoGenerator " ++ show ty
discoGenerator ty@(Skolem _) = error $ "discoGenerator " ++ show ty

discoGenerator TyVoid       = emptyGenerator
discoGenerator TyUnit       = DiscoGen (return ()) (return . const (VCons 0 []))
discoGenerator TyBool       = DiscoGen (QC.elements [False, True]) (return . mkEnum)

discoGenerator (TySum ty1 ty2) =
  case (discoGenerator ty1, discoGenerator ty2) of
    (g1, EmptyGen) -> mapDiscoGen vLeft  g1
    (EmptyGen, g2) -> mapDiscoGen vRight g2
    (DiscoGen gen1 toValue1, DiscoGen gen2 toValue2) ->
      DiscoGen
        (QC.choose (0 :: Double, 1) >>= \r ->
            if r < 0.5 then Left <$> gen1 else Right <$> gen2)
        (either (fmap vLeft . toValue1) (fmap vRight . toValue2))
  where
    vLeft  v = VCons 0 [v]
    vRight v = VCons 1 [v]

discoGenerator (TyPair ty1 ty2) =
  case (discoGenerator ty1, discoGenerator ty2) of
    (EmptyGen, _) -> EmptyGen
    (_, EmptyGen) -> EmptyGen
    (DiscoGen gen1 toValue1, DiscoGen gen2 toValue2) ->
      DiscoGen
        ((,) <$> gen1 <*> gen2)
        (\(a,b) -> VCons 0 <$> sequence [toValue1 a, toValue2 b])

discoGenerator (TyList ty) =
  case discoGenerator ty of
    EmptyGen -> unitGenerator
    DiscoGen tyGen tyToValue ->
      DiscoGen (QC.listOf tyGen) (toDiscoList <=< mapM tyToValue)

discoGenerator (TyBag _ty) =
  error "discoGenerator is not yet implemented for TyBag"

discoGenerator (TySet ty) =
  case discoGenerator ty of
    EmptyGen -> DiscoGen (return ()) (return . const (VBag []))
    DiscoGen tyGen tyToValue ->
      DiscoGen (QC.listOf tyGen) (valuesToSet ty <=< mapM tyToValue)

discoGenerator (_ty1 :->: _ty2) =
  error "discoGenerator is not yet implemented for function types"

discoGenerator (TyUser _ _) =
  error "discoGenerator is not yet implemented for user-defined types"

-- | @genValues n ty@ generates a random sequence of @n@ increasingly
--   complex values of type @ty@, using the 'DiscoGen' for @ty@.
genValues :: Int -> Type -> Disco IErr [Value]
genValues n ty = case discoGenerator ty of
  EmptyGen -> return []
  DiscoGen gen toValue -> do
    as <- generate n gen
    mapM toValue as

-- | Use a QuickCheck generator to generate a given number of
--   increasingly complex values of a given type.  Like the @sample'@
--   function from QuickCheck, but the number of values is
--   configurable, and it lives in the @Disco@ monad.
generate :: Int -> QC.Gen a -> Disco e [a]
generate n gen = io . QC.generate $ sequence [QC.resize m gen | m <- [0 .. n]]


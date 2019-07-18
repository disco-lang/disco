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

import           Control.Monad.Except
import           Data.Char                        (ord)
import           Data.Coerce
import           Data.List                        (transpose)
import qualified Data.Map                         as M
import           Data.Ratio
import           Data.Traversable                 (for)
import           System.Random

import           Data.Enumeration.Invertible

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
  = do
      let
        vals :: [[Disco IErr Value]]
        vals = mapM enumerateType tys
      -- The above mapM is in the list monad!
      vals' <- mapM (mapM delay) vals
      return $ (True, map (M.fromList . zip ys) $ vals')

  | otherwise = do
      valLists <- mapM (genValues n) tys
      return $ (False, map (M.fromList . zip ys) $ transpose valLists)
  where
    (xs, tys) = unzip binds
    ys :: [Name Core]
    ys = map coerce xs

-- XXX change this
genValues :: Int -> Type -> Disco IErr [Value]
genValues k ty = do
  let e = discoEnumeration ty
  case card e of
    Finite n -> do
      is <- liftIO $ replicateM k (randomRIO (0,n-1))
      mapM (evalStruct . select e) is
    Infinite -> mapM (evalStruct . select e) [0 .. fromIntegral k - 1]

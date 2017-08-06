
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Property
-- Copyright   :  (c) 2016-2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Properties of disco functions.
--
-----------------------------------------------------------------------------

module Disco.Property
       where

import Unbound.Generics.LocallyNameless (Name, lunbind)

import Control.Monad
import Data.Coerce
import qualified Data.Map as M
import Data.Traversable (for)

import Disco.Types
import Disco.Syntax.Operators (BOp(..))
import Disco.AST.Typed
import Disco.AST.Core
import Disco.Interpret.Core
import Disco.Eval
import Disco.Desugar

data TestResult
  = TestOK
  | TestRuntimeFailure  InterpError
  | TestFalse
  | TestEqualityFailure Value Type Value Type

instance Monoid TestResult where
  mempty = TestOK
  TestOK `mappend` r = r
  r `mappend` _      = r

testIsOK :: TestResult -> Bool
testIsOK TestOK = True
testIsOK _ = False

-- XXX if there is a quantifier, present it as a counterexample rather
-- than just an equality test failure
runTest :: M.Map (Name Core) Core -> AProperty -> TestResult
runTest defs aprop = either TestRuntimeFailure mconcat $ runDisco $ withDefs defs $ do
  lunbind aprop $ \(binds, at) ->
    for (testCases binds) $ \env -> extendsEnv env $ do
      case getEquatands at of
        Nothing        -> do
          v <- evalTerm at
          case v of
            VCons 1 [] -> return TestOK
            _          -> return TestFalse
        Just (at1,at2) -> do
          v1 <- evalTerm at1
          v2 <- evalTerm at2
          v <- decideEqFor (getType at1) v1 v2
          case v of
            True  -> return TestOK
            False -> return $ TestEqualityFailure v1 (getType at1) v2 (getType at2)
  where
    evalTerm = rnf . runDSM . desugarTerm

-- | Check whether a term looks like a top-level equality test.
getEquatands :: ATerm -> Maybe (ATerm, ATerm)
getEquatands (ATBin _ Eq at1 at2) = Just (at1, at2)
getEquatands _                    = Nothing

-- XXX just something simple for now.  Later do this a la QuickCheck,
-- with randomness and shrinking.
testCases :: [(Name ATerm, Type)] -> [Env]
testCases binds = map (M.fromList . zip ys) (mapM genExamples tys)
  where
    (xs, tys) = unzip binds
    ys :: [Name Core]
    ys = map coerce xs
    genExamples :: Type -> [Value]
    genExamples TyN = map vnum [0 .. 10]
    genExamples TyZ = map vnum ([0 .. 10] ++ [-1, -2 .. -10])
    genExamples (TyList ty) =
      map toDiscoList ([0 .. 2] >>= \n -> replicateM n (genExamples ty))

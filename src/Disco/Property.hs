
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

import qualified Data.Map as M

import Disco.Types
import Disco.AST.Surface (BOp(..))
import Disco.AST.Typed
import Disco.AST.Core
import Disco.Interpret.Core
import Disco.Desugar

data TestResult
  = TestOK
  | TestRuntimeFailure  InterpError
  | TestFalse
  | TestEqualityFailure Value Type Value Type

testIsOK :: TestResult -> Bool
testIsOK TestOK = True
testIsOK _ = False

runTest :: M.Map (Name Core) Core -> AProperty -> TestResult
runTest defns aprop = either TestRuntimeFailure id $ runIM' defns $ do
  lunbind aprop $ \(_binds, at) ->
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

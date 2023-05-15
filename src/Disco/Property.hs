
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

{-# LANGUAGE OverloadedStrings #-}

module Disco.Property
  (
  -- * Generation
    generateSamples

  -- * Utility
  , invertMotive, invertPropResult

  -- * Pretty-printing
  , prettyTestResult
  )
       where

import           Prelude                     hiding ((<>))

import qualified Data.Enumeration.Invertible as E

import           Disco.Effects.Random
import           Polysemy

import           Disco.AST.Typed
import           Disco.Syntax.Prims
import           Disco.Syntax.Operators
import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Error
import           Disco.Pretty
import           Disco.Typecheck.Erase       (eraseProperty)
import           Disco.Types                 (TyDefCtx)
import           Disco.Value
import           Polysemy.Reader

-- | Toggles which outcome (finding or not finding the thing being
--   searched for) qualifies as success, without changing the thing
--   being searched for.
invertMotive :: SearchMotive -> SearchMotive
invertMotive (SearchMotive (a, b)) = SearchMotive (not a, b)

-- | Flips the success or failure status of a @PropResult@, leaving
--   the explanation unchanged.
invertPropResult :: TestResult -> TestResult
invertPropResult res@(TestResult b r env)
  | TestRuntimeError _ <- r = res
  | otherwise               = TestResult (not b) r env

randomLarge :: Member Random r => [Integer] -> Sem r [Integer]
randomLarge [] = return []
randomLarge [_] = return []
randomLarge (x : y : xs) = (:) <$> randomR (x, y) <*> randomLarge (y : xs)

-- | Select samples from an enumeration according to a search type. Also returns
--   a 'SearchType' describing the results, which may be 'Exhaustive' if the
--   enumeration is no larger than the number of samples requested.
generateSamples :: Member Random r => SearchType -> E.IEnumeration a -> Sem r ([a], SearchType)
generateSamples Exhaustive e           = return (E.enumerate e, Exhaustive)
generateSamples (Randomized n m) e
  | E.Finite k <- E.card e, k <= n + m = return (E.enumerate e, Exhaustive)
  | otherwise                          = do
    let small = [0 .. n]
    rs <- randomLarge [100, 1000, 10000, 100000, 1000000]
    let samples = map (E.select e) $ small ++ rs
    return (samples, Randomized n m)

-- XXX do shrinking for randomly generated test cases?

------------------------------------------------------------
-- Pretty-printing for test results
------------------------------------------------------------

prettyResultCertainty :: Members '[LFresh, Reader PA] r => TestReason -> AProperty -> String -> Sem r Doc
prettyResultCertainty r prop res
  = (if resultIsCertain r then "Certainly" else "Possibly") <+> text res <> ":" <+> pretty (eraseProperty prop)

prettyTestReason
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => Bool -> AProperty -> TestReason -> Sem r Doc
prettyTestReason _ _ TestBool = empty
prettyTestReason b prop (TestFound (TestResult _ tr env))
  | b = prettyTestEnv "Found example:" env
  | not b = prettyTestReason b prop tr $+$ prettyTestEnv "Found counterexample:" env
prettyTestReason b _ (TestNotFound Exhaustive)
  | b = "No counterexamples exist; all possible values were checked."
  | not b = "No example exists; all possible values were checked."
prettyTestReason b _ (TestNotFound (Randomized n m))
  | b = "Checked" <+> text (show (n + m)) <+> "possibilities without finding a counterexample."
  | not b = "No example was found; checked" <+> text (show (n + m)) <+> "possibilities."
prettyTestReason _ _ (TestEqual t a1 a2) =
  bulletList "-"
  [ "Left side:  " <> prettyValue t a1
  , "Right side: " <> prettyValue t a2
  ]
prettyTestReason _ _ (TestLt t a1 a2) =
  bulletList "-"
  [ "Left side:  " <> prettyValue t a1
  , "Right side: " <> prettyValue t a2
  ]
prettyTestReason _ _ (TestRuntimeError ee) =
  "Test failed with an error:"
  $+$
  nest 2 (pretty (EvalErr ee))
prettyTestReason b (ATApp _ (ATPrim _ (PrimBOp And)) (ATTup _ [p1, p2])) (TestBin And tr1 tr2) =
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyTestResult' b p1 tr1)
  , "Right side: " $+$ nest 2 (prettyTestResult' b p2 tr2)
  ]
prettyTestReason b (ATApp _ (ATPrim _ (PrimBOp Or)) (ATTup _ [p1, p2])) (TestBin Or tr1 tr2) =
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyTestResult' b p1 tr1)
  , "Right side: " $+$ nest 2 (prettyTestResult' b p2 tr2)
  ]
prettyTestReason b (ATApp _ (ATPrim _ (PrimBOp Impl)) (ATTup _ [p1, p2])) (TestBin Impl tr1 tr2) =
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyTestResult' b p1 tr1)
  , "Right side: " $+$ nest 2 (prettyTestResult' b p2 tr2)
  ]
prettyTestReason _ _ _ = "!!! unexpected arguments in prettyTestReason!"

prettyTestResult'
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => Bool -> AProperty -> TestResult -> Sem r Doc
prettyTestResult' _ prop (TestResult bool tr _) =
  prettyResultCertainty tr prop (show bool)
  $+$
  prettyTestReason bool prop tr

prettyTestResult
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r Doc
prettyTestResult prop (TestResult b r env) = prettyTestResult' b prop (TestResult b r env)

prettyTestEnv
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => String -> TestEnv -> Sem r Doc
prettyTestEnv _ (TestEnv []) = empty
prettyTestEnv s (TestEnv vs) = text s $+$ nest 2 (vcat (map prettyBind vs))
  where
    maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
    prettyBind (x, ty, v) =
      text x <> text (replicate (maxNameLen - length x) ' ') <+> "=" <+> prettyValue ty v

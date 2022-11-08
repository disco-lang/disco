
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
  , prettyTestFailure, prettyTestResult
  )
       where

import           Prelude                     hiding ((<>))

import qualified Data.Enumeration.Invertible as E
import qualified Test.QuickCheck             as QC

import           Disco.Effects.Random
import           Polysemy

import           Disco.AST.Typed             (AProperty)
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

-- | Select samples from an enumeration according to a search type. Also returns
--   a 'SearchType' describing the results, which may be 'Exhaustive' if the
--   enumeration is no larger than the number of samples requested.
generateSamples :: Member Random r => SearchType -> E.IEnumeration a -> Sem r ([a], SearchType)
generateSamples Exhaustive e           = return (E.enumerate e, Exhaustive)
generateSamples (Randomized n m) e
  | E.Finite k <- E.card e, k <= n + m = return (E.enumerate e, Exhaustive)
  | otherwise                          = do
    let small = [0 .. n]
    rs <- runGen . mapM sizedNat $ [n .. n + m]
    let samples = map (E.select e) $ small ++ rs
    return (samples, Randomized n m)
  where
    sizedNat k = QC.resize (fromIntegral k) QC.arbitrarySizedNatural

-- XXX do shrinking for randomly generated test cases?

------------------------------------------------------------
-- Pretty-printing for test results
------------------------------------------------------------

prettyResultCertainty :: Members '[LFresh, Reader PA] r => TestReason -> AProperty -> String -> Sem r Doc
prettyResultCertainty r prop res
  = (if resultIsCertain r then "Certainly" else "Possibly") <+> text res <> ":" <+> pretty (eraseProperty prop)

prettyTestFailure
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r Doc
prettyTestFailure _    (TestResult True _ _)    = empty
prettyTestFailure prop (TestResult False r env) =
  prettyResultCertainty r prop "false"
  $+$
  prettyFailureReason r
  $+$
  prettyTestEnv "Counterexample:" env

prettyTestResult
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r Doc
prettyTestResult prop r | not (testIsOk r) = prettyTestFailure prop r
prettyTestResult prop (TestResult _ r _)   =
  prettyResultCertainty r prop "true"
  $+$
  prettySuccessReason r

prettySuccessReason
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => TestReason -> Sem r Doc
prettySuccessReason (TestFound (TestResult _ _ vs)) = prettyTestEnv "Found example:" vs
prettySuccessReason (TestNotFound Exhaustive) = "No counterexamples exist."
prettySuccessReason (TestNotFound (Randomized n m)) =
  "Checked" <+> text (show (n + m)) <+> "possibilities without finding a counterexample."
prettySuccessReason _ = empty

prettyFailureReason
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => TestReason -> Sem r Doc
prettyFailureReason TestBool = empty
prettyFailureReason (TestEqual ty v1 v2) =
  "Test result mismatch, the two sides are not equal:"
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyFailureReason (TestLt ty v1 v2)    =
  "Test result mismatch, the left side is not less than the right:"
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyFailureReason (TestRuntimeError e) =
  "Test failed with an error:"
  $+$
  nest 2 (pretty (EvalErr e))
prettyFailureReason (TestFound (TestResult _ r _)) = prettyFailureReason r
prettyFailureReason (TestNotFound Exhaustive) =
  "No example exists; all possible values were checked."
prettyFailureReason (TestNotFound (Randomized n m)) = do
  "No example was found; checked" <+> text (show (n + m)) <+> "possibilities."
prettyFailureReason (TestOr (TestResult _ tr1 _) (TestResult _ tr2 _)) =
  "Test did not evaluate to true:"
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyFailureReason tr1
  , "Right side: " <> prettyFailureReason tr2
  ]
prettyFailureReason (TestAnd (TestResult b1 tr1 _) (TestResult b2 tr2 _))
  | b1 =
    "Test did not evaluate to true:"
    $+$
    bulletList "-"
    [ "Left side:  " <> prettySuccessReason tr1
    , "Right side: " <> prettyFailureReason tr2
    ]
  | b2 =
    "Test did not evaluate to true:"
    $+$
    bulletList "-"
    [ "Left side:  " <> prettyFailureReason tr1
    , "Right side: " <> prettySuccessReason tr2
    ]
  | otherwise =
    "Test did not evaluate to true:"
    $+$
    bulletList "-"
    [ "Left side:  " <> prettyFailureReason tr1
    , "Right side: " <> prettyFailureReason tr2
    ]

prettyTestEnv
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => String -> TestEnv -> Sem r Doc
prettyTestEnv _ (TestEnv []) = empty
prettyTestEnv s (TestEnv vs) = text s $+$ nest 2 (vcat (map prettyBind vs))
  where
    maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
    prettyBind (x, ty, v) =
      text x <> text (replicate (maxNameLen - length x) ' ') <+> "=" <+> prettyValue ty v


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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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

import           Debug.Trace

import qualified Data.Enumeration.Invertible as E
import qualified Test.QuickCheck             as QC

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

-- prettyTestResult
--   :: Members '[Input TyDefCtx, LFresh, Reader PA] r
--   => AProperty -> TestResult -> Sem r Doc
-- prettyTestResult prop r | not (testIsOk r) = prettyTestFailure prop r
-- prettyTestResult prop (TestResult _ r _)   =
--   prettyResultCertainty r prop "true"
--   $+$
--   prettySuccessReason r prop

prettyTestResult 
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r Doc
prettyTestResult prop (TestResult b r _) = prettyReason b r prop

prettyTestFailure
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r Doc
prettyTestFailure _    (TestResult True _ _)    = empty
prettyTestFailure prop (TestResult False r env) =
  prettyResultCertainty r prop "false"
  $+$
  prettyFailureReason r prop
  $+$
  prettyTestEnv "Counterexample:" env

prettyReason 
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => Bool -> TestReason -> AProperty -> Sem r Doc
prettyReason True (TestFound (TestResult _ _ vs)) _ = prettyTestEnv "Found example:" vs
prettyReason False (TestFound (TestResult _ r _)) p = prettyFailureReason r p
prettyReason True (TestNotFound Exhaustive) _ = "No counterexamples exist."
prettyReason False (TestNotFound Exhaustive) _ = 
  "No example exists; all possible values were checked."
prettyReason True (TestNotFound (Randomized n m)) _ =
  "Checked" <+> text (show (n + m)) <+> "possibilities without finding a counterexample."
prettyReason False (TestNotFound (Randomized n m)) _ = do
  "No example was found; checked" <+> text (show (n + m)) <+> "possibilities."
prettyReason b (TestEqual ty v1 v2) p =
  prettyResultCertainty (TestEqual ty v1 v2) p (show b)
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyReason b (TestLt ty v1 v2) p =
  prettyResultCertainty (TestLt ty v1 v2) p (show b)  
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyReason b (TestAnd (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp And)) (ATTup t3 [p1, p2])) = 
  prettyResultCertainty (TestAnd (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp And)) (ATTup t3 [p1, p2])) (show b)
  $+$
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyReason b1 tr1 p1)
  , "Right side: " $+$ nest 2 (prettyReason b2 tr2 p2)
  ]
prettyReason b (TestOr (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp Or)) (ATTup t3 [p1, p2])) = 
  prettyResultCertainty (TestOr (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp Or)) (ATTup t3 [p1, p2])) (show b)
  $+$
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyReason b1 tr1 p1)
  , "Right side: " $+$ nest 2 (prettyReason b2 tr2 p2)
  ]
prettyReason b (TestImpl (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp Impl)) (ATTup t3 [p1, p2])) = 
  prettyResultCertainty (TestOr (TestResult b1 tr1 e1) (TestResult b2 tr2 e2)) (ATApp t1 (ATPrim t2 (PrimBOp Impl)) (ATTup t3 [p1, p2])) (show b)
  $+$
  bulletList "-"
  [ "Left side:  " $+$ nest 2 (prettyReason b1 tr1 p1)
  , "Right side: " $+$ nest 2 (prettyReason b2 tr2 p2)
  ]
prettyReason b tr p = traceShow tr $ traceShow p $ prettyResultCertainty tr p (show b)

prettySuccessReason
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => TestReason -> AProperty -> Sem r Doc
prettySuccessReason (TestFound (TestResult _ _ vs)) _ = prettyTestEnv "Found example:" vs
prettySuccessReason (TestNotFound Exhaustive) _ = "No counterexamples exist."
prettySuccessReason (TestNotFound (Randomized n m)) _ =
  "Checked" <+> text (show (n + m)) <+> "possibilities without finding a counterexample."
prettySuccessReason (TestAnd tr1 tr2) p = prettyTestResult p tr1 $+$ prettyTestResult p tr2
prettySuccessReason (TestOr tr1 tr2) p  = prettyTestResult p tr1 $+$ prettyTestResult p tr2
prettySuccessReason tr p = traceShow tr $ traceShow p $ prettyResultCertainty tr p "True"

prettyFailureReason
  :: Members '[Input TyDefCtx, LFresh, Reader PA] r
  => TestReason -> AProperty -> Sem r Doc
prettyFailureReason TestBool _ = empty
prettyFailureReason (TestEqual ty v1 v2) _ =
  "Test result mismatch, the two sides are not equal:"
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyFailureReason (TestLt ty v1 v2) _    =
  "Test result mismatch, the left side is not less than the right:"
  $+$
  bulletList "-"
  [ "Left side:  " <> prettyValue ty v1
  , "Right side: " <> prettyValue ty v2
  ]
prettyFailureReason (TestRuntimeError e) _ =
  "Test failed with an error:"
  $+$
  nest 2 (pretty (EvalErr e))
prettyFailureReason (TestFound (TestResult _ r _)) p = prettyFailureReason r p
prettyFailureReason (TestNotFound Exhaustive) _ =
  "No example exists; all possible values were checked."
prettyFailureReason (TestNotFound (Randomized n m)) _ = do
  "No example was found; checked" <+> text (show (n + m)) <+> "possibilities."
prettyFailureReason (TestOr (TestResult _ tr1 _) (TestResult _ tr2 _))  p =
  bulletList "-"
  [ "Left side:  " <> prettyFailureReason tr1 p
  , "Right side: " <> prettyFailureReason tr2 p
  ]
prettyFailureReason (TestAnd (TestResult b1 tr1 _) (TestResult b2 tr2 _)) p =
  bulletList "-"
  [ "Left side:  " $+$ nest 2 ((if b1 then prettySuccessReason else prettyFailureReason) tr1 p)
  , "Right side: " $+$ nest 2 ((if b2 then prettySuccessReason else prettyFailureReason) tr2 p)
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

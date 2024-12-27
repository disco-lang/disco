{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Disco.Property
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Properties of disco functions.
module Disco.Property (
  -- * Generation
  generateSamples,

  -- * Utility
  invertMotive,
  invertPropResult,

  -- * Pretty-printing
  prettyTestResult,
)
where

import Prelude hiding ((<>))

import Data.Char (toLower)
import qualified Data.Enumeration.Invertible as E

import Polysemy

import Disco.AST.Typed
import Disco.Effects.Input
import Disco.Effects.LFresh
import Disco.Error
import Disco.Pretty
import Disco.Syntax.Prims
import Disco.Typecheck.Erase (eraseProperty)
import Disco.Types (TyDefCtx)
import Disco.Value
import Polysemy.Random
import Polysemy.Reader

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
  | otherwise = TestResult (not b) r env

randomLarge :: Member Random r => [Integer] -> Sem r [Integer]
randomLarge [] = return []
randomLarge [_] = return []
randomLarge (x : y : xs) = (:) <$> randomR (x, y) <*> randomLarge (y : xs)

-- | Select samples from an enumeration according to a search type. Also returns
--   a 'SearchType' describing the results, which may be 'Exhaustive' if the
--   enumeration is no larger than the number of samples requested.
generateSamples :: Member Random r => SearchType -> E.IEnumeration a -> Sem r ([a], SearchType)
generateSamples Exhaustive e = return (E.enumerate e, Exhaustive)
generateSamples (Randomized n m) e
  | E.Finite k <- E.card e, k <= n + m = return (E.enumerate e, Exhaustive)
  | otherwise = do
      let small = [0 .. n]
      rs <- randomLarge [100, 1000, 10000, 100000, 1000000]
      let samples = map (E.select e) $ small ++ rs
      return (samples, Randomized n m)

-- XXX do shrinking for randomly generated test cases?

------------------------------------------------------------
-- Pretty-printing for test results
------------------------------------------------------------

prettyResultCertainty :: Members '[LFresh, Reader PA] r => TestReason -> AProperty -> String -> Sem r (Doc ann)
prettyResultCertainty r prop res =
  (if resultIsCertain r then "Certainly" else "Possibly") <+> text res <> ":" <+> pretty (eraseProperty prop)

prettyTestReason ::
  Members '[Input TyDefCtx, LFresh, Reader PA] r =>
  Bool ->
  AProperty ->
  TestReason ->
  Sem r (Doc ann)
prettyTestReason _ _ TestBool = empty
prettyTestReason b (ATAbs _ _ body) (TestFound (TestResult b' r' env)) = do
  lunbind body $ \(_, p) ->
    prettyTestEnv ("Found " ++ if b then "example:" else "counterexample:") env
      $+$ prettyTestReason b' p r'
prettyTestReason b _ (TestNotFound Exhaustive)
  | b = "No counterexamples exist; all possible values were checked."
  | otherwise = "No example exists; all possible values were checked."
prettyTestReason b _ (TestNotFound (Randomized n m))
  | b = "Checked" <+> text (show (n + m)) <+> "possibilities without finding a counterexample."
  | otherwise = "No example was found; checked" <+> text (show (n + m)) <+> "possibilities."
prettyTestReason _ _ (TestCmp _ t a1 a2) =
  bulletList
    "-"
    [ "Left side:  " <> prettyValue t a1
    , "Right side: " <> prettyValue t a2
    ]
prettyTestReason _ _ (TestRuntimeError ee) =
  nest 2 $
    "Test failed with an error:"
      $+$ pretty (EvalErr ee)
-- \$+$
-- prettyTestEnv "Example inputs that caused the error:" env
-- See #364
prettyTestReason b (ATApp _ (ATPrim _ (PrimBOp _)) (ATTup _ [p1, p2])) (TestBin _ tr1 tr2) =
  bulletList
    "-"
    [ nest 2 $ prettyTestResult' b p1 tr1
    , nest 2 $ prettyTestResult' b p2 tr2
    ]
-- See Note [prettyTestReason fallback]
prettyTestReason _ _ _ = empty

-- ~~~~ Note [prettyTestReason fallback]
--
-- prettyTestReason can do a decent job printing out reasons for a
-- test result when operators like /\, \/, etc. are written
-- explicitly; then it can structurally recurse on the original Prop
-- expression in parllel with the TestReason.  However, it is possible
-- to e.g. write a function which returns a Prop, making the structure
-- of the Prop expression opaque.  For example, consider this example
-- (from test/prop-higher-order):
--
-- !!! all [true, true, true, false, true]
-- all : List(Prop) -> Prop
-- all ps = reduce(~/\~, true, ps)
--
-- This test is false, and the TestReason ends up with a bunch of
-- nested TestBin LAnd.  However, the proposition is literally a
-- function application so we cannot see that it matches the structure
-- of the test result.  So we just give up and decline to print a
-- reason.

prettyTestResult' ::
  Members '[Input TyDefCtx, LFresh, Reader PA] r =>
  Bool ->
  AProperty ->
  TestResult ->
  Sem r (Doc ann)
prettyTestResult' _ prop (TestResult bool tr _) =
  prettyResultCertainty tr prop (map toLower (show bool))
    $+$ prettyTestReason bool prop tr

prettyTestResult ::
  Members '[Input TyDefCtx, LFresh, Reader PA] r =>
  AProperty ->
  TestResult ->
  Sem r (Doc ann)
prettyTestResult prop (TestResult b r env) = prettyTestResult' b prop (TestResult b r env)

prettyTestEnv ::
  Members '[Input TyDefCtx, LFresh, Reader PA] r =>
  String ->
  TestEnv ->
  Sem r (Doc ann)
prettyTestEnv _ (TestEnv []) = empty
prettyTestEnv s (TestEnv vs) = nest 2 $ text s $+$ vcat (map prettyBind vs)
 where
  maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
  prettyBind (x, ty, v) =
    text x <> text (replicate (maxNameLen - length x) ' ') <+> "=" <+> prettyValue ty v

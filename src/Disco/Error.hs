{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Disco.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Top-level utilities for dealing with Disco errors and warnings.
module Disco.Error (
    DiscoErrorKind (..),
    FurtherReading (..),
    DiscoError (..),
    panic,
    outputDiscoErrors,
) where

import Prelude hiding ((<>))

import Disco.Effects.LFresh
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader

import Disco.Messages
import Disco.Names (ModuleName)
import Disco.Pretty
import Error.Diagnose (Report)

type DiscoError = Report Text

-- | Sources for further reading.
data FurtherReading
    = -- | Link to documentation page on readthedocs.
      RTD String
    | -- | Link to a GitHub issue.
      Issue Int
    | -- | Free-form.
      OtherReading Doc

reportFurtherReading :: FurtherReading -> Sem r Doc
reportFurtherReading (RTD _) = undefined
reportFurtherReading (Issue i) = "If you are curious to read more about this bug, visit" <+> issue i <> "."
reportFurtherReading (OtherReading r) = return r

panic :: Member (Error DiscoError) r => Maybe Int -> String -> Sem r a
panic issueNum panicMsg = do
  expl <- text panicMsg
  reading <- case issueNum of
    Nothing -> OtherReading <$> "It would be a huge help to Disco development if you visit" <+> newIssue <> ", create a new issue, paste this error message, and explain what you were doing when you got this message."
    Just i -> return $ Issue i
  throw $
    DiscoError
      { errHeadline = "Disco bug!"
      , errKind = Panic
      , errExplanation = expl
      , errHints = ["This error is not your fault!  It is a bug in Disco itself."]
      , errReading = [reading]
      }

outputDiscoErrors :: Member (Output Message) r => Sem (Error DiscoError ': r) () -> Sem r ()
outputDiscoErrors m = do
  e <- runError m
  either (err . pretty') return e

-- | Final formatting of a top-level Disco error.
instance Pretty DiscoError where
  pretty = return . errHeadline -- TODO: include more info!
  -- pretty = \case
  --   ModuleNotFound m -> "Error: couldn't find a module named '" <> text m <> "'."
  --   CyclicImport ms -> cyclicImportError ms
  --   TypeCheckErr (LocTCError Nothing te) -> prettyTCError te
  --   TypeCheckErr (LocTCError (Just n) te) ->
  --     vcat
  --       [ "While checking " <> pretty' n <> ":"
  --       , nest 2 $ prettyTCError te
  --       ]
  --   ParseErr pe -> text (errorBundlePretty pe)
  --   EvalErr ee -> prettyEvalError ee
  --   Panic s ->
  --     vcat
  --       [ "Bug! " <> text s
  --       , "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
  --       ]

rtd :: String -> Sem r Doc
rtd page = "https://disco-lang.readthedocs.io/en/latest/reference/" <> text page <> ".html"

issue :: Int -> Sem r Doc
issue n = issueTracker <> "/" <> text (show n)

issueTracker :: Sem r Doc
issueTracker = "https://github.com/disco-lang/disco/issues"

newIssue :: Sem r Doc
newIssue = issueTracker <> "/new/choose"

cyclicImportError ::
  Members '[Reader PA, LFresh] r =>
  [ModuleName] ->
  Sem r Doc
cyclicImportError ms =
  vcat
    [ "Error: module imports form a cycle:"
    , nest 2 $ intercalate " ->" (map pretty ms)
    ]

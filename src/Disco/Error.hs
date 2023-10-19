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

import Disco.Effects.LFresh
import Disco.Messages
import Disco.Names (ModuleName)
import Disco.Pretty
import Error.Diagnose (Diagnostic, Note (..), Report (..))
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Prelude hiding ((<>))

type DiscoDiagnostic = Diagnostic Doc
type DiscoReport = Report Doc

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

panic :: Member (Error DiscoReport) r => Maybe Int -> String -> Sem r a
panic issueNum panicMsg = do
    expl <- text panicMsg
    reading <- case issueNum of
        Nothing -> OtherReading <$> "It would be a huge help to Disco development if you visit" <+> newIssue <> ", create a new issue, paste this error message, and explain what you were doing when you got this message."
        Just i -> return $ Issue i
    throw
        $ Err
            Nothing
            expl
            []
            [ Note "This error is not your fault!  It is a bug in Disco itself."
            , Note (reportFurtherReading reading)
            ]

outputDiscoErrors :: Member (Output (Message ann)) r => Sem (Error DiscoError ': r) () -> Sem r ()
outputDiscoErrors m = do
    e <- runError m
    either (err . pretty') return e

-- | Final formatting of a top-level Disco report.
instance Pretty DiscoReport where
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

rtd :: String -> Sem r (Doc ann)
rtd page = "https://disco-lang.readthedocs.io/en/latest/reference/" <> text page <> ".html"

-- issue :: Int -> Sem r (Doc ann)
-- issue n = issueTracker <> "/" <> text (show n)

squote :: String -> String
squote x = "'" ++ x ++ "'"

issueTracker :: Sem r Doc
issueTracker = "https://github.com/disco-lang/disco/issues"

newIssue :: Sem r Doc
newIssue = issueTracker <> "/new/choose"

cyclicImportError ::
  Members '[Reader PA, LFresh] r =>
  [ModuleName] ->
  Sem r (Doc ann)
cyclicImportError ms =
  nest 2 $ vcat
    [ "Error: module imports form a cycle:"
    , intercalate " ->" (map pretty ms)
    ]

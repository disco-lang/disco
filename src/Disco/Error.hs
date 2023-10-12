{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Disco.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type for collecting all potential Disco errors at the top level,
-- and a type for runtime errors.
module Disco.Error (
  DiscoErrorKind (..),
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
import Disco.Typecheck.Solve
import Disco.Types.Qualifiers

-- | Top-level error type for Disco.
--
--   Got the basic idea for top-level error type from
--
--   <https://skillsmatter.com/skillscasts/9879-an-informal-guide-to-better-compiler-errors-jasper-van-der-jeugt>
--
--   The idea is that each subsystem uses a sum type to represent the
--   kinds of errors that can occur; then at the top level instead of
--   using an even bigger sum type, like we used to, just use a
--   generic type with pretty-printed information.
--
--   Also took ideas from <https://elm-lang.org/news/the-syntax-cliff> in terms of
--   what information we store here.
data DiscoError = DiscoError
  { errHeadline :: Doc
  -- ^ The error "headline".
  , errKind :: DiscoErrorKind
  -- ^ What kind of error is it?
  , errExplanation :: Doc
  -- ^ A summary/explanation of the error. TODO: include source
  -- location info?
  , errHints :: [Doc]
  -- ^ Things to try, examples, etc. that might help.
  , errReading :: [FurtherReading]
  -- ^ References to further reading.
  }

-- | Enumeration of general categories of errors that can occur.
data DiscoErrorKind
  = ResolverErr
  | TypeCheckErr
  | ParseErr
  | EvalErr
  | Panic

-- | Sources for further reading.
data FurtherReading
  = -- | Link to documentation page on readthedocs.
    RTD String
  | -- | Link to a GitHub issue.
    Issue Int
  | -- | Free-form.
    OtherReading Doc

panic :: Member (Error DiscoError) r => Maybe Int -> String -> Sem r a
panic issueNum panicMsg = do
  expl <- text panicMsg
  reading <- case issueNum of
    Nothing -> "It would be a huge help to Disco development if you visit" <+> newIssue <> ", create a new issue, paste this error message, and explain what you were doing when you got this message."
    Just i -> "If you are curious to read more about this bug, visit" <+> issue i <> "."
  throw
    $ DiscoError
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

prettySolveError :: Members '[Reader PA, LFresh] r => SolveError -> Sem r Doc
prettySolveError = \case
  -- XXX say which types!
  NoWeakUnifier ->
    vcat
      [ "Error: the shape of two types does not match."
      , rtd "shape-mismatch"
      ]
  -- XXX say more!  XXX HIGHEST PRIORITY!
  NoUnify ->
    vcat
      [ "Error: typechecking failed."
      , rtd "typecheck-fail"
      ]
  UnqualBase q b ->
    vcat
      [ "Error: values of type" <+> pretty' b <+> qualPhrase False q <> "."
      , rtd "not-qual"
      ]
  Unqual q ty ->
    vcat
      [ "Error: values of type" <+> pretty' ty <+> qualPhrase False q <> "."
      , rtd "not-qual"
      ]
  QualSkolem q a ->
    vcat
      [ "Error: type variable" <+> pretty' a <+> "represents any type, so we cannot assume values of that type"
      , nest 2 (qualPhrase True q) <> "."
      , rtd "qual-skolem"
      ]

qualPhrase :: Bool -> Qualifier -> Sem r Doc
qualPhrase b q
  | q `elem` [QBool, QBasic, QSimple] = "are" <+> (if b then empty else "not") <+> qualAction q
  | otherwise = "can" <> (if b then empty else "not") <+> "be" <+> qualAction q

qualAction :: Qualifier -> Sem r Doc
qualAction = \case
  QNum -> "added and multiplied"
  QSub -> "subtracted"
  QDiv -> "divided"
  QCmp -> "compared"
  QEnum -> "enumerated"
  QBool -> "boolean"
  QBasic -> "basic"
  QSimple -> "simple"

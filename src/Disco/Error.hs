{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type for collecting all potential Disco errors at the top level,
-- and a type for runtime errors.
--
-----------------------------------------------------------------------------

module Disco.Error (DiscoError(..), EvalError(..), panic, outputDiscoErrors) where

import           Prelude                          hiding ((<>))

import           Text.Megaparsec                  (ParseErrorBundle,
                                                   errorBundlePretty)
import           Unbound.Generics.LocallyNameless (Name)

import           Disco.Effects.LFresh
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader

import           Disco.Messages
import           Disco.Names                      (ModuleName)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Typecheck.Util             (TCError (..))
import           Disco.Types

-- | Top-level error type for Disco.
data DiscoError where

  -- | Module not found.
  ModuleNotFound :: String -> DiscoError

  -- | Cyclic import encountered.
  CyclicImport :: [ModuleName] -> DiscoError

  -- | Error encountered during typechecking.
  TypeCheckErr :: TCError -> DiscoError

  -- | Error encountered during parsing.
  ParseErr :: ParseErrorBundle String DiscoParseError -> DiscoError

  -- | Error encountered at runtime.
  EvalErr :: EvalError -> DiscoError

  -- | Something that shouldn't happen; indicates the presence of a
  --   bug.
  Panic         :: String    -> DiscoError

  deriving Show

-- | Errors that can be generated at runtime.
data EvalError where

  -- | An unbound name.  This shouldn't happen.
  UnboundError  :: Name core  -> EvalError

  -- | Division by zero.
  DivByZero     ::              EvalError

  -- | Overflow, e.g. (2^66)!
  Overflow      ::              EvalError

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              EvalError

  -- | Infinite loop detected via black hole.
  InfiniteLoop  ::              EvalError

  -- | User-generated crash.
  Crash         :: String    -> EvalError

deriving instance Show EvalError

panic :: Member (Error DiscoError) r => String -> Sem r a
panic = throw . Panic

outputDiscoErrors :: Member (Output Message) r => Sem (Error DiscoError ': r) () -> Sem r ()
outputDiscoErrors m = do
  e <- runError m
  either (err . pretty') return e

instance Pretty DiscoError where
  pretty = \case
    ModuleNotFound m -> "Error: couldn't find a module named '" <> text m <> "'."
    CyclicImport ms  -> cyclicImportError ms
    TypeCheckErr te  -> prettyTCError te
    ParseErr pe      -> text (errorBundlePretty pe)
    EvalErr ee       -> prettyEvalError ee
    Panic s          ->
      hcat
        [ "Bug! " <> text s
        , "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
        ]

cyclicImportError
  :: Members '[Reader PA, LFresh] r
  => [ModuleName] -> Sem r Doc
cyclicImportError ms =
  vcat
    [ "Error: module imports form a cycle:"
    , nest 2 $ intercalate " ->" (map pretty ms)
    ]

prettyEvalError :: Members '[Reader PA, LFresh] r => EvalError -> Sem r Doc
prettyEvalError = \case
   UnboundError x ->
     ("Bug! No variable found named" <+> pretty' x <> ".")
     $+$
     "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
   DivByZero      -> "Error: division by zero."
   Overflow       -> "Error: that number would not even fit in the universe!"
   NonExhaustive  -> "Error: value did not match any of the branches in a case expression."
   InfiniteLoop   -> "Error: infinite loop detected!"
   Crash s        -> "User crash:" <+> text s

-- Step 1: nice error messages, make sure all are tested
-- Step 2: get it to return multiple error messages
-- Step 3: save parse locations, display with errors
prettyTCError :: Members '[Reader PA, LFresh] r => TCError -> Sem r Doc
prettyTCError = \case

  -- XXX include some potential misspellings along with Unbound
  Unbound x      -> "Error: there is nothing named" <+> pretty' x <> "."

  Ambiguous x ms ->
    ("Error: the name" <+> pretty' x <+> "is ambiguous. It could refer to:")
    $+$
    nest 2 (vcat . map (\m -> pretty' m <> "." <> pretty' x) $ ms)

  NoType x ->
    "Error: the definition of" <+> pretty' x <+> "must have an accompanying type signature."
    $+$
    "Try writing something like '" <> pretty' x <+> ": Int' (or whatever the type of"
      <+> pretty' x <+> "should be) first."

  NotCon c t ty ->
    vcat
      [ "Error: the term"
      , nest 2 $ pretty' t
      , "must have both a" <+> conWord c <+> "type and also the incompatible type"
      , nest 2 $ pretty' ty <> "."
      ]

  EmptyCase -> "Error: empty case expressions {? ?} are not allowed."

  PatternType c pat ty ->
    vcat
    [ "Error: the pattern"
    , nest 2 $ pretty' pat
    , "is supposed to have type"
    , nest 2 $ pretty' ty <> ","
    , "but instead it has a" <+> conWord c <+> "type."
    ]

  DuplicateDecls x -> "Error: duplicate type signature for" <+> pretty' x <> "."

  DuplicateDefns x -> "Error: duplicate definition for" <+> pretty' x <> "."

  DuplicateTyDefns s -> "Error: duplicate definition for type" <+> text s <> "."

  e              -> text . show . TypeCheckErr $ e

  -- CyclicTyDef s -> _
  -- NumPatterns -> _
  -- NoLub ty ty' -> _
  -- NoNeg ty -> _
  -- NoSearch ty -> _
  -- Unsolvable se -> _
  -- NotTyDef s -> _
  -- NoTWild -> _
  -- CantInferPrim pr -> _
  -- NotEnoughArgs con -> _
  -- TooManyArgs con -> _
  -- UnboundTyVar na -> _
  -- NoPolyRec s ss tys -> _
  -- Failure s -> _
  -- NoError -> _

conWord :: Con -> Sem r Doc
conWord = \case
  CArr         -> "function"
  CProd        -> "product"
  CSum         -> "sum"
  CSet         -> "set"
  CBag         -> "bag"
  CList        -> "list"
  CContainer _ -> "container"
  CMap         -> "map"
  CGraph       -> "graph"
  CUser s      -> text s

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
import           Disco.Names                      (ModuleName, QName)
import           Disco.Parser                     (DiscoParseError)
import           Disco.Pretty
import           Disco.Typecheck.Solve
import           Disco.Typecheck.Util             (LocTCError (..),
                                                   TCError (..))
import           Disco.Types
import           Disco.Types.Qualifiers

-- | Top-level error type for Disco.
data DiscoError where

  -- | Module not found.
  ModuleNotFound :: String -> DiscoError

  -- | Cyclic import encountered.
  CyclicImport :: [ModuleName] -> DiscoError

  -- | Error encountered during typechecking.
  TypeCheckErr :: LocTCError -> DiscoError

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

  -- | An unbound name was encountered.
  UnboundError  :: QName core  -> EvalError

  -- | An unbound name that really shouldn't happen, coming from some
  --   kind of internal name generation scheme.
  UnboundPanic  :: Name core   -> EvalError

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
    ModuleNotFound m  -> "Error: couldn't find a module named '" <> text m <> "'."
    CyclicImport ms   -> cyclicImportError ms
    TypeCheckErr (LocTCError Nothing te) -> prettyTCError te
    TypeCheckErr (LocTCError (Just n) te) ->
      vcat
        [ "While checking " <> pretty' n <> ":"
        , nest 2 $ prettyTCError te
        ]
    ParseErr pe       -> text (errorBundlePretty pe)
    EvalErr ee        -> prettyEvalError ee
    Panic s           ->
      vcat
        [ "Bug! " <> text s
        , "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
        ]

rtd :: String -> Sem r Doc
rtd page = "https://disco-lang.readthedocs.io/en/latest/reference/" <> text page <> ".html"

issue :: Int -> Sem r Doc
issue n = "See https://github.com/disco-lang/disco/issues/" <> text (show n)

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
   UnboundPanic x ->
     ("Bug! No variable found named" <+> pretty' x <> ".")
     $+$
     "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
   UnboundError x -> "Error: encountered undefined name" <+> pretty' x <> ". Maybe you haven't defined it yet?"
   DivByZero      -> "Error: division by zero."
   Overflow       -> "Error: that number would not even fit in the universe!"
   NonExhaustive  -> "Error: value did not match any of the branches in a case expression."
   InfiniteLoop   -> "Error: infinite loop detected!"
   Crash s        -> "User crash:" <+> text s

-- [X] Step 1: nice error messages, make sure all are tested
-- [ ] Step 2: link to wiki/website with more info on errors!
-- [ ] Step 3: improve error messages according to notes below
-- [ ] Step 4: get it to return multiple error messages
-- [ ] Step 5: save parse locations, display with errors
prettyTCError :: Members '[Reader PA, LFresh] r => TCError -> Sem r Doc
prettyTCError = \case

  -- XXX include some potential misspellings along with Unbound
  --   see https://github.com/disco-lang/disco/issues/180
  Unbound x      -> vcat
    [ "Error: there is nothing named" <+> pretty' x <> "."
    , rtd "unbound"
    ]

  Ambiguous x ms -> vcat
    [ "Error: the name" <+> pretty' x <+> "is ambiguous. It could refer to:"
    , nest 2 (vcat . map (\m -> pretty' m <> "." <> pretty' x) $ ms)
    , rtd "ambiguous"
    ]

  NoType x -> vcat
    [ "Error: the definition of" <+> pretty' x <+> "must have an accompanying type signature."
    , "Try writing something like '" <> pretty' x <+> ": Int' (or whatever the type of"
      <+> pretty' x <+> "should be) first."
    , rtd "missingtype"
    ]

  NotCon c t ty -> vcat
    [ "Error: the expression"
    , nest 2 $ pretty' t
    , "must have both a" <+> conWord c <+> "type and also the incompatible type"
    , nest 2 $ pretty' ty <> "."
    , rtd "notcon"
    ]

  EmptyCase -> vcat
    [ "Error: empty case expressions {? ?} are not allowed."
    , rtd "empty-case"
    ]

  PatternType c pat ty -> vcat
    [ "Error: the pattern"
    , nest 2 $ pretty' pat
    , "is supposed to have type"
    , nest 2 $ pretty' ty <> ","
    , "but instead it has a" <+> conWord c <+> "type."
    , rtd "pattern-type"
    ]

  DuplicateDecls x -> vcat
    [ "Error: duplicate type signature for" <+> pretty' x <> "."
    , rtd "dup-sig"
    ]

  DuplicateDefns x -> vcat
    [ "Error: duplicate definition for" <+> pretty' x <> "."
    , rtd "dup-def"
    ]

  DuplicateTyDefns s -> vcat
    [ "Error: duplicate definition for type" <+> text s <> "."
    , rtd "dup-tydef"
    ]

  -- XXX include all types involved in the cycle.
  CyclicTyDef s -> vcat
    [ "Error: cyclic type definition for" <+> text s <> "."
    , rtd "cyc-ty"
    ]

  -- XXX lots more info!  & Split into several different errors.
  NumPatterns -> vcat
    [ "Error: number of arguments does not match."
    , rtd "num-args"
    ]

  NonlinearPattern p x  -> vcat
    [ "Error: pattern" <+> pretty' p <+> "contains duplicate variable" <+> pretty' x <> "."
    , rtd "nonlinear"
    ]

  NoSearch ty ->
    vcat
    [ "Error: the type"
    , nest 2 $ pretty' ty
    , "is not searchable (i.e. it cannot be used in a forall)."
    , rtd "no-search"
    ]

  Unsolvable solveErr -> prettySolveError solveErr

  -- XXX maybe include close edit-distance alternatives?
  NotTyDef s -> vcat
    [ "Error: there is no built-in or user-defined type named '" <> text s <> "'."
    , rtd "no-tydef"
    ]

  NoTWild -> vcat
    [ "Error: wildcards (_) are not allowed in expressions."
    , rtd "wildcard-expr"
    ]

  -- XXX say how many are expected, how many there were, what the actual arguments were?
  -- XXX distinguish between built-in and user-supplied type constructors in the error
  --     message?
  NotEnoughArgs con -> vcat
    [ "Error: not enough arguments for the type '" <> pretty' con <> "'."
    , rtd "num-args-type"
    ]

  TooManyArgs con -> vcat
    [ "Error: too many arguments for the type '" <> pretty' con <> "'."
    , rtd "num-args-type"
    ]

  -- XXX Mention the definition in which it was found, suggest adding the variable
  --     as a parameter
  UnboundTyVar v -> vcat
    [ "Error: Unknown type variable '" <> pretty' v <> "'."
    , rtd "unbound-tyvar"
    ]

  NoPolyRec s ss tys -> vcat
    [ "Error: in the definition of " <> text s <> parens (intercalate "," (map text ss)) <> ": recursive occurrences of" <+> text s <+> "may only have type variables as arguments."
    , nest 2 (
        text s <> parens (intercalate "," (map pretty' tys)) <+> "does not follow this rule."
      )
    , rtd "no-poly-rec"
    ]

  NoError -> empty

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

prettySolveError :: Members '[Reader PA, LFresh] r => SolveError -> Sem r Doc
prettySolveError = \case

  -- XXX say which types!
  NoWeakUnifier -> vcat
    [ "Error: the shape of two types does not match."
    , rtd "shape-mismatch"
    ]

  -- XXX say more!  XXX HIGHEST PRIORITY!
  NoUnify       -> vcat
    [ "Error: typechecking failed."
    , rtd "typecheck-fail"
    ]

  UnqualBase q b -> vcat
    [ "Error: values of type" <+> pretty' b <+> qualPhrase False q <> "."
    , rtd "not-qual"
    ]

  Unqual q ty -> vcat
    [ "Error: values of type" <+> pretty' ty <+> qualPhrase False q <> "."
    , rtd "not-qual"
    ]

  QualSkolem q a -> vcat
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
  QNum    -> "added and multiplied"
  QSub    -> "subtracted"
  QDiv    -> "divided"
  QCmp    -> "compared"
  QEnum   -> "enumerated"
  QBool   -> "boolean"
  QBasic  -> "basic"
  QSimple -> "simple"


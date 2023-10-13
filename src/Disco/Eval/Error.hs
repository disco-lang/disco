{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Disco.Eval.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime errors.
module Disco.Eval.Error (
  EvalError (..),
  reportEvalError,
  prettyEvalError,
) where

import Disco.Effects.LFresh (LFresh)
import Disco.Error (DiscoError (..), DiscoErrorKind (EvalErr))
import Disco.Names (QName)
import Disco.Pretty
import Polysemy (Members, Sem)
import Polysemy.Reader (Reader)
import Unbound.Generics.LocallyNameless (Name)
import Prelude hiding ((<>))

------------------------------------------------------------
-- Runtime errors
------------------------------------------------------------

-- | Errors that can be generated at runtime.
data EvalError where
  -- | An unbound name was encountered.
  UnboundError :: QName core -> EvalError
  -- | An unbound name that really shouldn't happen, coming from some
  --   kind of internal name generation scheme.
  UnboundPanic :: Name core -> EvalError
  -- | Division by zero.
  DivByZero :: EvalError
  -- | Overflow, e.g. (2^66)!
  Overflow :: EvalError
  -- | Non-exhaustive case analysis.
  NonExhaustive :: EvalError
  -- | Infinite loop detected via black hole.
  InfiniteLoop :: EvalError
  -- | User-generated crash.
  Crash :: String -> EvalError

deriving instance Show EvalError

-- XXX update this.  Run reader, LFresh locally.
reportEvalError :: EvalError -> Sem r DiscoError
reportEvalError e = runPretty $ do
  explain <- prettyEvalError e
  return $
    DiscoError
      { errHeadline = "Runtime error"
      , errKind = EvalErr
      , errExplanation = explain
      , errHints = []
      , errReading = []
      }

prettyEvalError :: Members '[Reader PA, LFresh] r => EvalError -> Sem r Doc
prettyEvalError = \case
  UnboundPanic x ->
    ("Bug! No variable found named" <+> pretty' x <> ".")
      $+$ "Please report this as a bug at https://github.com/disco-lang/disco/issues/ ."
  UnboundError x -> "Error: encountered undefined name" <+> pretty' x <> ". Maybe you haven't defined it yet?"
  DivByZero -> "Error: division by zero."
  Overflow -> "Error: that number would not even fit in the universe!"
  NonExhaustive -> "Error: value did not match any of the branches in a case expression."
  InfiniteLoop -> "Error: infinite loop detected!"
  Crash s -> "User crash:" <+> text s

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

module Disco.Error (DiscoError(..), EvalError(..)) where

import qualified Data.Void
import           Disco.AST.Core                   (Core)
import           Disco.AST.Surface                (ModName)
import           Disco.Typecheck.Monad            (TCError)
import           Text.Megaparsec                  (ParseErrorBundle)
import           Unbound.Generics.LocallyNameless (Name)

-- | Top-level error type for Disco.
data DiscoError where

  -- | Module not found.
  ModuleNotFound :: ModName -> DiscoError

  -- | Cyclic import encountered.
  CyclicImport :: ModName -> DiscoError

  -- | Error encountered during typechecking.
  TypeCheckErr :: TCError -> DiscoError

  -- | Error encountered during parsing.
  ParseErr :: ParseErrorBundle String Data.Void.Void -> DiscoError

  -- | Error encountered at runtime.
  EvalErr :: EvalError -> DiscoError

  -- | Something that shouldn't happen; indicates the presence of a
  --   bug.
  Panic         :: String    -> DiscoError

  deriving Show

-- | Errors that can be generated at runtime.
data EvalError where

  -- | An unbound name.
  UnboundError  :: Name Core -> EvalError

  -- | Division by zero.
  DivByZero     ::              EvalError

  -- | Underflow, e.g. (2 - 3 : Nat)
  Underflow     ::              EvalError

  -- | Overflow, e.g. (2^66)!
  Overflow      ::              EvalError

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              EvalError

  -- | User-generated crash.
  Crash         :: String    -> EvalError

  deriving Show

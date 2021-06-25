{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Error
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type for collecting all potential Disco errors at the top level.
--
-----------------------------------------------------------------------------

module Disco.Error (IErr(..)) where

import           Control.Exception                (Exception)
import qualified Data.Void
import           Disco.AST.Core                   (Core)
import           Disco.AST.Surface
import           Disco.Typecheck.Monad            (TCError)
import           Disco.Types                      (Type)
import           Text.Megaparsec                  (ParseErrorBundle)
import           Unbound.Generics.LocallyNameless

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Errors that can be generated during interpreting.
data IErr where

  -- | Module not found.
  ModuleNotFound :: ModName -> IErr

  -- | Cyclic import encountered.
  CyclicImport :: ModName -> IErr

  -- | Error encountered during typechecking.
  TypeCheckErr :: TCError -> IErr

  -- | Error encountered during parsing.
  ParseErr :: ParseErrorBundle String Data.Void.Void -> IErr

  -- | An unbound name.
  UnboundError  :: Name Core -> IErr

  -- | Division by zero.
  DivByZero     ::              IErr

  -- | Underflow, e.g. (2 - 3 : Nat)
  Underflow     ::              IErr

  -- | Overflow, e.g. (2^66)!
  Overflow      ::              IErr

  -- | Non-exhaustive case analysis.
  NonExhaustive ::              IErr

  -- | Trying to count an infinite type.
  InfiniteTy    :: Type      -> IErr

  -- | User-generated crash.
  Crash         :: String    -> IErr

  deriving Show
  deriving anyclass Exception

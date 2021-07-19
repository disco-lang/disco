-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Types
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Disco.Interactive.Types
  ( CmdTag(..), REPLExpr(..), SomeREPLExpr(..), REPLCommand(..), SomeREPLCommand(..)
  , REPLCommandCategory(..), REPLCommandType(..),
  withoutBuiltins)
  where

import           Polysemy
import           Polysemy.Error
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.Eval
import           Disco.Extensions
import           Disco.Parser

import           Data.Typeable

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

data REPLExpr :: CmdTag -> * where
  Using     :: Ext -> REPLExpr 'CUsing             -- Enable an extension
  Let       :: Name Term -> Term -> REPLExpr 'CLet -- Toplevel let-expression: for the REPL
  TypeCheck :: Term -> REPLExpr 'CTypeCheck        -- Typecheck a term
  Eval      :: Term -> REPLExpr 'CEval             -- Evaluate a term
  TestProp  :: Term -> REPLExpr 'CTestProp         -- Run a property test
  ShowDefn  :: Name Term -> REPLExpr 'CShowDefn    -- Show a variable's definition
  Parse     :: Term -> REPLExpr 'CParse            -- Show the parsed AST
  Pretty    :: Term -> REPLExpr 'CPretty           -- Pretty-print a term
  Ann       :: Term -> REPLExpr 'CAnn              -- Show type-annotated typechecked term
  Desugar   :: Term -> REPLExpr 'CDesugar          -- Show a desugared term
  Compile   :: Term -> REPLExpr 'CCompile          -- Show a compiled term
  Import    :: String -> REPLExpr 'CImport         -- Import a library module.
  Load      :: FilePath -> REPLExpr 'CLoad         -- Load a file.
  Reload    :: REPLExpr 'CReload                   -- Reloads the most recently loaded file.
  Doc       :: Name Term -> REPLExpr 'CDoc         -- Show documentation.
  Nop       :: REPLExpr 'CNop                      -- No-op, e.g. if the user
                                                   -- just enters a comment
  Help      :: REPLExpr 'CHelp
  Names     :: REPLExpr 'CNames


deriving instance Show (REPLExpr c)

data SomeREPLExpr where
  SomeREPL :: Typeable c => REPLExpr c -> SomeREPLExpr

------------------------------------------------------------
-- REPL command types
------------------------------------------------------------

data REPLCommandCategory =
    User    -- for everyday users
  | Dev     -- for developers working on Disco
  deriving (Eq, Show)

data REPLCommandType =
    BuiltIn   -- let, import, using, eval
  | ShellCmd  -- :help, :names, :load...
  deriving (Eq, Show)

------------------------------------------------------------
-- Commands
------------------------------------------------------------

data REPLCommand (c :: CmdTag) = REPLCommand
  { name      :: String
  , helpcmd   :: String
  , shortHelp :: String
  , category  :: REPLCommandCategory
  , cmdtype   :: REPLCommandType
  , action    :: REPLExpr c -> (forall r. Members (Error IErr ': DiscoEffects) r => Sem r ())
  , parser    :: Parser (REPLExpr c)
  }

data CmdTag = CUsing | CLet | CTypeCheck | CEval | CShowDefn
  | CParse | CPretty | CAnn | CDesugar | CCompile | CImport | CLoad
  | CReload | CDoc | CNop | CHelp | CNames | CTestProp
  deriving (Show, Eq, Typeable)

data SomeREPLCommand where
  SomeCmd :: Typeable c => REPLCommand c -> SomeREPLCommand


-- | Filter out commands that don't start with ':' (ex: "let" vs ":load")
withoutBuiltins :: [SomeREPLCommand] -> [SomeREPLCommand]
withoutBuiltins = filter (\(SomeCmd rc) -> cmdtype rc /= BuiltIn)

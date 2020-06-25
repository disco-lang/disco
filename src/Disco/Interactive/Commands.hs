-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Comands
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for things entered at an interactive disco prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.Commands (discoCommands) where

import Disco.Interactive.Parser (REPLExpr)
import           Disco.Parser (sc, ident)

data ReplCommandType =
    User
  | Dev
  | Advanced
  deriving Show
------------------------------------------------------------
-- Commands
------------------------------------------------------------
data ReplCommand = ReplCommand
  { name  :: String 
  , shortHelp :: String
  , longHelp :: String
  , cmdType :: ReplCommandType
  , cmdAction :: String -- REPLExpr -> Disco IErr ()
  , cmdParser :: Parser REPLExpr
  }

discoCommands :: [ReplCommand]
discoCommands = 
  [
  ReplCommand {
      name = "help",
      shortHelp = "Show help",
      longHelp = "Show help",
      cmdType = User,
      cmdAction = "foo",
      cmdParser = return Help
    },
    ReplCommand {
      name = "type",
      shortHelp = "Typecheck a term",
      longHelp = "Typecheck a term",
      cmdType = User,
      cmdAction = "foo", -- REPLExpr -> Disco IErr ()
      cmdParser = TypeCheck <$> parseTypeTarget
    },
    ReplCommand {
      name = "names",
      shortHelp = "Show all names in current scope",
      longHelp = "Show all names in current scope",
      cmdType = User,
      cmdAction = "foo",
      cmdParser = return Names
    },
    ReplCommand {
      name = "defn",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = "foo",
      cmdParser = ShowDefn  <$> (sc *> ident)
    }
  ]
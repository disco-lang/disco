-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Parser
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Expression type and parser for things entered at an interactive disco
-- prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.REPLCommand where

import           Disco.Interactive.Parser (Parser, REPLExpr)

data ReplCommandType =
    User
  | Dev
  | Advanced
  deriving Show
------------------------------------------------------------
-- Commands
------------------------------------------------------------
data ReplCommand = ReplCommand
  { name      :: String
  , shortHelp :: String
  , longHelp  :: String
  , cmdType   :: ReplCommandType
  , cmdAction :: String -- REPLExpr -> Disco IErr ()
  , cmdParser :: Parser REPLExpr
  }

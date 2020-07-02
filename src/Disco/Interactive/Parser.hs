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

module Disco.Interactive.Parser
  ( REPLExpr(..)
  , letParser, commandParser, parseCommandArgs, fileParser, lineParser, parseLine,
    parseTypeTarget
  ) where

import           Data.Char               (isSpace)
import           Data.List               (find, isPrefixOf)

import           Text.Megaparsec         hiding (runParser)
import qualified Text.Megaparsec.Char    as C

import           Disco.AST.Surface
import           Disco.Extensions
import           Disco.Parser
-- import           Disco.Syntax.Operators  -- needed for #185
import           Disco.Interactive.Types

------------------------------------------------------------
-- Parser
------------------------------------------------------------

letParser :: Parser REPLExpr
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: [REPLCommand] -> Parser REPLExpr
commandParser allCommands = (symbol ":" *> many C.lowerChar) >>= (parseCommandArgs allCommands)

parseCommandArgs ::  [REPLCommand] -> String -> Parser REPLExpr
parseCommandArgs allCommands cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    parsers = map (\rc -> (name rc, parser rc)) allCommands

parseTypeTarget :: Parser Term
parseTypeTarget =
      (try term <?> "expression")

-- Can't do this until we get rid of TUn and TBin, represent operator
-- applications as just normal function application.

--   <|> (parseNakedOp <?> "naked operator?")

-- parseNakedOp :: Parser Term
-- parseNakedOp = oneOf (map mkOpParser (concat opTable))
--   where
--     mkOpParser :: OpInfo -> Parser Term
--     mkOpParser (OpInfo (UOpF _ op) syns _) = oneOf (map ((_ op <$) . reservedOp) syns)
--     mkOpParser (OpInfo (BOpF _ op) syns _) = oneOf (map ((_ op <$) . reservedOp) syns)

--     oneOf :: [Parser a] -> Parser a
--     oneOf = foldr (<|>) empty

fileParser :: Parser FilePath
fileParser = many C.spaceChar *> many (satisfy (not . isSpace))

lineParser :: [REPLCommand] -> Parser REPLExpr
lineParser allCommands
  =   (commandParser allCommands)
  <|> try (Nop <$ (sc <* eof))
  <|> try (Using <$> (reserved "using" *> parseExtName))
  <|> try (Import <$> parseImport)
  <|> try (Eval <$> term)
  <|> letParser

parseLine :: [REPLCommand] -> ExtSet -> String -> Either String REPLExpr
parseLine allCommands exts s =
  case (runParser (withExts exts (lineParser allCommands)) "" s) of
    Left  e -> Left $ errorBundlePretty e
    Right l -> Right l

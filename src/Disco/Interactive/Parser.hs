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

{-# LANGUAGE DataKinds #-}

module Disco.Interactive.Parser
  ( REPLExpr(..)
  , letParser, commandParser, parseCommandArgs, fileParser, lineParser, parseLine
  ) where

import           Data.Char               (isSpace)
import           Data.List               (find, isPrefixOf)

import           Text.Megaparsec         hiding (runParser)
import qualified Text.Megaparsec.Char    as C

import           Disco.Extensions
import           Disco.Interactive.Types
import           Disco.Parser

------------------------------------------------------------
-- Parser
------------------------------------------------------------

letParser :: Parser (REPLExpr 'CLet)
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: [SomeREPLCommand] -> Parser SomeREPLExpr
commandParser allCommands = (symbol ":" *> many C.lowerChar) >>= (parseCommandArgs allCommands)

parseCommandArgs ::  [SomeREPLCommand] -> String -> Parser SomeREPLExpr
parseCommandArgs allCommands cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    -- filter out commands that don't start with ':' (ex: "let" vs ":load")
    parsers = map (\(SomeCmd rc) -> (name rc, SomeREPL <$> parser rc)) $ withoutBuiltins allCommands

fileParser :: Parser FilePath
fileParser = many C.spaceChar *> many (satisfy (not . isSpace))

lineParser :: [SomeREPLCommand] -> Parser SomeREPLExpr
lineParser allCommands
  =   (commandParser allCommands)
  <|> try (SomeREPL Nop <$ (sc <* eof))
  <|> try ((SomeREPL . Using) <$> (reserved "using" *> parseExtName))
  <|> try ((SomeREPL . Import) <$> parseImport)
  <|> try ((SomeREPL . Eval) <$> term)
  <|> (SomeREPL <$> letParser)

parseLine :: [SomeREPLCommand] -> ExtSet -> String -> Either String SomeREPLExpr
parseLine allCommands exts s =
  case (runParser (withExts exts (lineParser allCommands)) "" s) of
    Left  e -> Left $ errorBundlePretty e
    Right l -> Right l

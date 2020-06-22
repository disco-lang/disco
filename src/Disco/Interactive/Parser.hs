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
  , letParser, commandParser, parseCommandArgs, fileParser, lineParser, parseLine
  ) where

import           Data.Char                        (isSpace)
import           Data.List                        (find, isPrefixOf)

import           Text.Megaparsec                  hiding (runParser)
import qualified Text.Megaparsec.Char             as C
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.Extensions
import           Disco.Parser
-- import           Disco.Syntax.Operators  -- needed for #185

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

data REPLExpr =
   Using Ext                    -- Enable an extension
 | Let (Name Term) Term         -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | Eval Term                    -- Evaluate a term
 | ShowDefn (Name Term)         -- Show a variable's definition
 | Parse Term                   -- Show the parsed AST
 | Pretty Term                  -- Pretty-print a term
 | Ann Term                     -- Show type-annotated typechecked term
 | Desugar Term                 -- Show a desugared term
 | Compile Term                 -- Show a compiled term
 | Import String                -- Import a library module
 | Load FilePath                -- Load a file
 | Reload                       -- Reloads the most recently loaded file
 | Doc (Name Term)              -- Show documentation
 | Nop                          -- No-op, e.g. if the user just enters a comment
 | Help                         -- Show repl help
 | Names                        -- Show all names currently bound
 deriving Show

------------------------------------------------------------
-- Parser
------------------------------------------------------------

letParser :: Parser REPLExpr
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: Parser REPLExpr
commandParser = (symbol ":" *> many C.lowerChar) >>= parseCommandArgs

parseCommandArgs :: String -> Parser REPLExpr
parseCommandArgs cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    parsers =
      [ ("type",    TypeCheck <$> parseTypeTarget)
      , ("defn",    ShowDefn  <$> (sc *> ident))
      , ("parse",   Parse     <$> term)
      , ("pretty",  Pretty    <$> term)
      , ("ann",     Ann       <$> term)
      , ("desugar", Desugar   <$> term)
      , ("compile", Compile   <$> term)
      , ("load",    Load      <$> fileParser)
      , ("reload",  return Reload)
      , ("doc",     Doc       <$> (sc *> ident))
      , ("help",    return Help)
      , ("names",  return Names)
      ]

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

lineParser :: Parser REPLExpr
lineParser
  =   commandParser
  <|> try (Nop <$ (sc <* eof))
  <|> try (Using <$> (reserved "using" *> parseExtName))
  <|> try (Import <$> parseImport)
  <|> try (Eval <$> term)
  <|> letParser

parseLine :: ExtSet -> String -> Either String REPLExpr
parseLine exts s =
  case (runParser (withExts exts lineParser) "" s) of
    Left  e -> Left $ errorBundlePretty e
    Right l -> Right l

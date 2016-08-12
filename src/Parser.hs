{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- Simple proof of concept parser using Parsec, since I know it well
-- and can throw something together quickly.  Later we will probably
-- want to redo this using e.g. trifecta.

module Parser where

import           Unbound.LocallyNameless

import           Text.Parsec             hiding (Error, many, (<|>))
import           Text.Parsec.Expr        hiding (Operator)
import           Text.Parsec.Language    (haskellStyle)
import           Text.Parsec.String      (Parser)
import qualified Text.Parsec.Token       as P

import           Control.Applicative     ((<|>))

import           Types

------------------------------------------------------------
-- Parser

lexer :: P.TokenParser u
lexer = P.makeTokenParser $
  haskellStyle
  { P.reservedNames   = [ "true", "false", "inl", "inr", "let", "in", "case", "if", "where"
                        , "()"
                        , "Void", "Unit", "Bool", "Nat", "Integer", "Rational", "N", "Z", "Q"]
  , P.reservedOpNames = [ "|->", "+", "-", "*", "/"
                        , "->" ]
  }

parens :: Parser a -> Parser a
parens     = P.parens lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

ident :: Parser String
ident = P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseAtom :: Parser Term
parseAtom
  =   (TVar . string2Name) <$> ident
  <|> TUnit       <$ reserved "()"
  <|> TBool True  <$ reserved "true"
  <|> TBool False <$ reserved "false"

  <|> try (TPair <$> (symbol "(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")"))

  <|> parens parseTerm

parseTerm :: Parser Term
parseTerm = undefined

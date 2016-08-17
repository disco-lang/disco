{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- Simple proof of concept parser using Parsec, since I know it well
-- and can throw something together quickly.  Later we will probably
-- want to redo this using e.g. trifecta.

module Parser where

import           Unbound.LocallyNameless (Name, bind, string2Name)

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

ident :: Parser (Name Term)
ident = string2Name <$> P.identifier lexer

natural, integer :: Parser Integer
natural = P.natural lexer
integer = P.integer lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseAtom :: Parser Term
parseAtom
  =   TUnit       <$ reserved "()"
  <|> TBool True  <$ reserved "true"
  <|> TBool False <$ reserved "false"
  <|> TVar <$> ident
  <|> TInt <$> natural
  <|> try (TPair <$> (symbol "(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")"))

  <|> parens parseTerm

parseInj :: Parser Side
parseInj =
  L <$ reserved "inl" <|> R <$ reserved "inr"

parseTerm :: Parser Term
parseTerm =
      TAbs <$> try (bind <$> ident <*> (reservedOp "|->" *> parseTerm))
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseLet
  <|> parseCase
  <|> parseExpr
  <|> parseAtom

parseLet :: Parser Term
parseLet = fail ""

parseCase :: Parser Term
parseCase = fail ""

parseExpr :: Parser Term
parseExpr = buildExpressionParser table parseAtom <?> "expression"
  where
    table = [ [ binary ""  TApp AssocLeft ]
            , [ unary  "-" (TUn Neg) ]
            , [ binary "*" (TBin Mul) AssocLeft
              , binary "/" (TBin Div) AssocLeft
              ]
            , [ binary "+" (TBin Add) AssocLeft
              , binary "-" (TBin Sub) AssocLeft
              ]
            , [ binary "==" (TBin Equals) AssocNone
              , binary "<" (TBin Less) AssocNone
              ]
            , [ binary "&&" (TBin And) AssocRight ]
            , [ binary "||" (TBin Or)  AssocRight ]
            ]

    unary  name fun       = Prefix (reservedOp name >> return fun)
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

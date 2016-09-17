{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- Simple proof of concept parser using Parsec, since I know it well
-- and can throw something together quickly.  Later we will probably
-- want to redo this using e.g. trifecta.

module Parser where

import           Unbound.LocallyNameless (Name, bind, embed, rec, string2Name)

import           Text.Parsec             hiding (Error, many, (<|>))
import           Text.Parsec.Expr        hiding (Operator)
import           Text.Parsec.Language    (haskellStyle)
import           Text.Parsec.String      (Parser)
import qualified Text.Parsec.Token       as P

import           Control.Applicative     (many, (<|>))

import           Types

------------------------------------------------------------
-- Lexer

lexer :: P.TokenParser u
lexer = P.makeTokenParser $
  haskellStyle
  { P.reservedNames   = [ "true", "false", "inl", "inr", "let", "in", "case", "if", "where"
                        , "()"
                        , "Void", "Unit", "Bool", "Nat", "Natural", "Int", "Integer", "Rational"
                        , "N", "Z", "Q", "ℕ", "ℤ", "ℚ"
                        ]
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

------------------------------------------------------------
-- Parser

parseProg :: Parser Prog
parseProg = parseDecl `sepEndBy` symbol ";"

parseDecl :: Parser Decl
parseDecl =
      try (DType <$> ident <*> (symbol ":" *> parseType))
  <|>      DDefn <$> ident <*> (symbol "=" *> parseTerm)

parseAtom :: Parser Term
parseAtom
  =   TUnit       <$ reserved "()"
  <|> TBool True  <$ reserved "true"
  <|> TBool False <$ reserved "false"
  <|> TVar <$> ident
  <|> TNat <$> natural
  <|> try (TPair <$> (symbol "(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")"))

  <|> parens parseTerm

parseInj :: Parser Side
parseInj =
  L <$ reserved "inl" <|> R <$ reserved "inr"

parseTerm :: Parser Term
parseTerm = ascribe <$> parseTerm' <*> optionMaybe (symbol ":" *> parseType)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty

parseTerm' :: Parser Term
parseTerm' =
      TAbs <$> try (bind <$> ident <*> (reservedOp "|->" *> parseTerm'))
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseLet
  <|> parseCase
  <|> parseExpr
  <|> parseAtom

parseLet :: Parser Term
parseLet =
  TLet <$>
    (reserved "let" *>
      (bind
        <$> (rec <$> ((,) <$> ident <*> (symbol "=" *> (embed <$> parseTerm))))
        <*> (reserved "in" *> parseTerm)))

parseCase :: Parser Term
parseCase = TCase <$> (reserved "case" *> many parseBranch)

parseBranch :: Parser Branch
parseBranch = many1 (symbol "{") *> (flip bind <$> parseTerm <*> many parseGuard)

parseGuard :: Parser Guard
parseGuard =
      GIf    <$> (embed <$> (reserved "if" *> parseTerm))
  <|> GWhere <$> (embed <$> (reserved "where" *> parseTerm)) <*> (symbol "=" *> parsePattern)

parseAtomicPattern :: Parser Pattern
parseAtomicPattern =
      PVar <$> ident
  <|> PWild <$ symbol "_"
  <|> PUnit <$ reserved "()"
  <|> PBool True  <$ reserved "true"
  <|> PBool False <$ reserved "false"
  <|> PNat <$> natural
  <|> try (PPair <$> (symbol "(" *> parsePattern) <*> (symbol "," *> parsePattern <* symbol ")"))
  <|> parens parsePattern

parsePattern :: Parser Pattern
parsePattern =
      PInj <$> parseInj <*> parseAtomicPattern
  <|> PSucc <$> (symbol "S" *> parseAtomicPattern)
  <|> parseAtomicPattern

parseExpr :: Parser Term
parseExpr = buildExpressionParser table parseAtom <?> "expression"
  where
    table = [ [ binary ""  TJuxt AssocLeft ]
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

parseAtomicType :: Parser Type
parseAtomicType =
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ reserved "Bool"
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
  <|> parens parseType

parseType :: Parser Type
parseType = parseTypeExpr <|> parseAtomicType

parseTypeExpr :: Parser Type
parseTypeExpr = buildExpressionParser table parseAtomicType <?> "type expression"
  where
    table = [ [ binary "*" TyPair AssocRight ]
            , [ binary "+" TySum AssocRight ]
            , [ binary "->" TyArr AssocRight ]
            ]

    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

parseTermStr :: String -> Term
parseTermStr s = case (parse parseTerm "" s) of
                   Left e -> error.show $ e
                   Right t -> t


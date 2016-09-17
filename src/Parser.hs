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
                        , "otherwise"
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

-- | Parse an entire program (a list of declarations ended by
--   semicolons).
parseProg :: Parser Prog
parseProg = parseDecl `sepEndBy` symbol ";"

-- | Parse a single declaration (either a type declaration or
--   definition).
parseDecl :: Parser Decl
parseDecl =
      try (DType <$> ident <*> (symbol ":" *> parseType))
  <|>      DDefn <$> ident <*> (symbol "=" *> parseTerm)

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom
  =   TUnit       <$ reserved "()"
  <|> TBool True  <$ reserved "true"
  <|> TBool False <$ reserved "false"
  <|> TVar <$> ident
  <|> TNat <$> natural
  <|> try (TPair <$> (symbol "(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")"))

  <|> parens parseTerm

-- | Parse an injection, i.e. either @inl@ or @inr@.
parseInj :: Parser Side
parseInj =
  L <$ reserved "inl" <|> R <$ reserved "inr"

-- | Parse a term, consisting of a @parseTerm'@ optionally
--   followed by an ascription.
parseTerm :: Parser Term
parseTerm = ascribe <$> parseTerm' <*> optionMaybe (symbol ":" *> parseType)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty

-- | Parse a non-atomic, non-ascribed term.
parseTerm' :: Parser Term
parseTerm' =
      TAbs <$> try (bind <$> ident <*> (reservedOp "|->" *> parseTerm'))
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseLet
  <|> parseCase
  <|> parseExpr
  <|> parseAtom

-- | Parse a let expression (@let x = t1 in t2@).
parseLet :: Parser Term
parseLet =
  TLet <$>
    (reserved "let" *>
      (bind
        <$> (rec <$> ((,) <$> ident <*> (symbol "=" *> (embed <$> parseTerm))))
        <*> (reserved "in" *> parseTerm)))

-- | Parse a case expression.
parseCase :: Parser Term
parseCase = TCase <$> (reserved "case" *> many parseBranch)

-- | Parse one branch of a case expression.  Note a branch can start
--   with many curly braces since we allow empty lines just for
--   aesthetics, e.g.
--
--   @
--     case {  3    where t = (false, _)
--          {  5    where t = (true, 16)
--          {
--          {  7    otherwise
--   @
parseBranch :: Parser Branch
parseBranch = many1 (symbol "{") *> (flip bind <$> parseTerm <*> parseGuards)

-- | Parse the list of guards in a branch.  @otherwise@ can be used
--   interchangeably with an empty list of guards.
parseGuards :: Parser [Guard]
parseGuards = ([] <$ reserved "otherwise") <|> many parseGuard

-- | Parse a single guard (either @if@ or @where@)
parseGuard :: Parser Guard
parseGuard =
      GIf    <$> (embed <$> (reserved "if" *> parseTerm))
  <|> GWhere <$> (embed <$> (reserved "where" *> parseTerm)) <*> (symbol "=" *> parsePattern)

-- | Parse an atomic pattern.
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

-- | Parse a complex pattern.
parsePattern :: Parser Pattern
parsePattern =
      PInj <$> parseInj <*> parseAtomicPattern
  <|> PSucc <$> (symbol "S" *> parseAtomicPattern)
  <|> parseAtomicPattern

-- | Parse an expression built out of unary and binary operators.
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

-- | Parse an atomic type.
parseAtomicType :: Parser Type
parseAtomicType =
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ reserved "Bool"
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
  <|> parens parseType

-- | Parse a complex type.
parseType :: Parser Type
parseType = parseTypeExpr <|> parseAtomicType

-- | Parse a type expression built out of binary operators.
parseTypeExpr :: Parser Type
parseTypeExpr = buildExpressionParser table parseAtomicType <?> "type expression"
  where
    table = [ [ binary "*" TyPair AssocRight ]
            , [ binary "+" TySum AssocRight ]
            , [ binary "->" TyArr AssocRight ]
            ]

    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

-- | For convenience/testing
parseTermStr :: String -> Term
parseTermStr s = case (parse parseTerm "" s) of
                   Left e -> error.show $ e
                   Right t -> t


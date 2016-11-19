{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- Simple proof of concept parser using Parsec, since I know it well
-- and can throw something together quickly.  Later we will probably
-- want to redo this using e.g. trifecta.

module Disco.Parser where

import           Unbound.LocallyNameless (Name, bind, embed, rebind,
                                          string2Name)

import           Text.Parsec             hiding (Error, many, (<|>))
import           Text.Parsec.Expr        hiding (Operator)
import           Text.Parsec.Language    (haskellStyle)
import           Text.Parsec.String      (Parser, parseFromFile)
import qualified Text.Parsec.Token       as P

import           Control.Applicative     (many, (<|>))

import           Disco.Types

------------------------------------------------------------
-- Lexer

lexer :: P.TokenParser u
lexer = P.makeTokenParser $
  haskellStyle
  { P.reservedNames   = [ "true", "false", "True", "False", "inl", "inr", "let", "in"
                        , "if", "when"
                        , "otherwise", "and", "or", "not", "mod"
                        , "()"
                        , "Void", "Unit", "Bool", "Nat", "Natural", "Int", "Integer", "Rational"
                        , "N", "Z", "Q", "ℕ", "ℤ", "ℚ"
                        ]
  , P.reservedOpNames = [ "|->", "+", "-", "*", "/", "&&", "||", "∧", "∨", "^"
                        , "->", "<", ">", "<=", ">=", "/=", "==", "%", "|", "#" ]
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
  <|>      DDefn <$> ident <*> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm))

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom
  =   TUnit       <$ reserved "()"
  <|> TBool True  <$ (reserved "true" <|> reserved "True")
  <|> TBool False <$ (reserved "false" <|> reserved "False")
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
      TAbs <$> try (bind <$> ident <*> (parseMapsTo *> parseTerm'))
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseLet
  <|> parseCase
  <|> parseExpr
  <|> parseAtom

parseMapsTo :: Parser ()
parseMapsTo = (reservedOp "↦" <|> reservedOp "->" <|> reservedOp "|->")

-- | Parse a let expression (@let x = t1 in t2@).
parseLet :: Parser Term
parseLet =
  TLet <$>
    (reserved "let" *>
      (bind
        <$> ((,) <$> ident <*> (symbol "=" *> (embed <$> parseTerm)))
        <*> (reserved "in" *> parseTerm)))

-- | Parse a case expression.
parseCase :: Parser Term
parseCase = TCase <$> many1 parseBranch

-- | Parse one branch of a case expression.  Note a branch can start
--   with many curly braces since we allow empty lines just for
--   aesthetics, e.g.
--
--   @
--     {  3    when t = (false, _)
--     {  5    when t = (true, 16)
--     {
--     {  7    otherwise
--   @
parseBranch :: Parser Branch
parseBranch = many1 (symbol "{") *> (flip bind <$> parseTerm <*> parseGuards)

-- | Parse the list of guards in a branch.  @otherwise@ can be used
--   interchangeably with an empty list of guards.
parseGuards :: Parser Guards
parseGuards = (GEmpty <$ reserved "otherwise") <|> (guards <$> many parseGuard)
  where
    guards = foldr (\g gs -> GCons $ rebind g gs) GEmpty

-- | Parse a single guard (either @if@ or @when@)
parseGuard :: Parser Guard
parseGuard =
      GIf   <$> (embed <$> (reserved "if" *> parseTerm))
  <|> GWhen <$> (embed <$> (reserved "when" *> parseTerm)) <*> (symbol "=" *> parsePattern)

-- | Parse an atomic pattern.
parseAtomicPattern :: Parser Pattern
parseAtomicPattern =
      PVar <$> ident
  <|> PWild <$ symbol "_"
  <|> PUnit <$ reserved "()"
  <|> PBool True  <$ (reserved "true" <|> reserved "True")
  <|> PBool False <$ (reserved "false" <|> reserved "False")
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
    table = [ [ binary ""  TJuxt AssocLeft
              , unary "not" (TUn Not)
              ]
            , [ unary  "-" (TUn Neg) ]
            , [ binary "^" (TBin Exp) AssocRight ]
            , [ binary "*" (TBin Mul) AssocLeft
              , binary "/" (TBin Div) AssocLeft
              , binary "%" (TBin Mod) AssocLeft
              , binary "mod" (TBin Mod) AssocLeft
              ]
            , [ binary "+" (TBin Add) AssocLeft
              , binary "-" (TBin Sub) AssocLeft
              ]
            , [ binary "==" (TBin Eq)  AssocNone
              , binary "/=" (TBin Neq) AssocNone
              , binary "<"  (TBin Lt)  AssocNone
              , binary ">"  (TBin Gt)  AssocNone
              , binary "<=" (TBin Leq) AssocNone
              , binary ">=" (TBin Geq) AssocNone
              , binary "|"  (TBin Divides) AssocNone
              , binary "#"  (TBin RelPm) AssocNone
              ]
            , [ binary "&&"  (TBin And) AssocRight
              , binary "and" (TBin And) AssocRight
              , binary "∧"   (TBin And) AssocRight
              ]
            , [ binary "||" (TBin Or)  AssocRight
              , binary "or" (TBin Or) AssocRight
              , binary "∨"  (TBin Or) AssocRight
              ]
            ]

    unary  name fun       = Prefix (reservedOp name >> return fun)
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

-- | Parse an atomic type.
parseAtomicType :: Parser Type
parseAtomicType =
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ (reserved "Bool" <|> reserved "B")
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
parseTermStr s = case (parse (whiteSpace *> parseTerm <* eof) "" s) of
                   Left e -> error.show $ e
                   Right t -> t


parseTypeStr :: String -> Type
parseTypeStr s = case (parse parseType "" s) of
                   Left e -> error.show $ e
                   Right t -> t

parseFile :: FilePath -> IO (Either ParseError Prog)
parseFile = parseFromFile (whiteSpace *> parseProg <* eof)

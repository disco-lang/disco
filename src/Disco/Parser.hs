{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Parser
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Parser to convert concrete Disco syntax into an (untyped, surface
-- language) AST.
--
-----------------------------------------------------------------------------

module Disco.Parser
       ( -- * Lexer

         -- ** Basic lexemes
         whiteSpace, lexeme, symbol, reservedOp
       , natural, integer, reserved, reservedWords, identifier, ident

         -- ** Punctuation
       , parens, braces, angles, brackets
       , semi, comma, colon, dot
       , mapsTo

         -- * Parser
         -- ** Programs
       , prog, parseProg, parseDecl

         -- ** Terms
       , term, parseTerm, parseTerm', parseExpr, parseAtom
       , parseInj, parseLet

         -- ** Case and patterns
       , parseCase, parseBranch, parseGuards, parseGuard
       , parsePattern, parseAtomicPattern

         -- ** Types
       , parseType, parseTypeExpr, parseAtomicType
       )
       where

import           Unbound.LocallyNameless (Name, bind, embed, rebind,
                                          string2Name)

import           Text.Megaparsec
import qualified Text.Megaparsec.Char    as C
import           Text.Megaparsec.Expr    hiding (Operator)
import qualified Text.Megaparsec.Lexer   as L
import           Text.Megaparsec.String  (Parser)

import           Control.Applicative     (many, (<|>))

import           Disco.AST.Surface
import           Disco.Types

------------------------------------------------------------
-- Lexer


-- | Consume whitespace, including line and block comments.
whiteSpace :: Parser ()
whiteSpace = L.space (C.spaceChar *> pure ()) lineComment blockComment
  -- XXX should we use C.spaceChar here, or a variant that does not consume newlines?
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

-- | Parse a lexeme, that is, a parser followed by consuming whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

-- | Parse a given string as a lexeme.
symbol :: String -> Parser String
symbol = L.symbol whiteSpace

-- | Like 'symbol', but discard the result.
reservedOp :: String -> Parser ()
reservedOp s = symbol s *> pure ()

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semi      = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

-- | The symbol that separates the variable binder from the body of a
--   lambda (either @↦@, @->@, or @|->@).
mapsTo :: Parser ()
mapsTo = reservedOp "↦" <|> reservedOp "->" <|> reservedOp "|->"

-- | Parse a natural number.
natural = lexeme L.integer

-- | Parse a signed integer.
integer = L.signed whiteSpace natural

-- | Parse a reserved word.
reserved :: String -> Parser ()
reserved w = lexeme $ C.string w *> notFollowedBy alphaNumChar

-- | The list of all reserved words.
reservedWords :: [String]
reservedWords =
  [ "true", "false", "True", "False", "inl", "inr", "let", "in"
  , "if", "when"
  , "otherwise", "and", "or", "not", "mod", "choose"
  , "Void", "Unit", "Bool", "Nat", "Natural", "Int", "Integer", "Rational"
  , "N", "Z", "Q", "ℕ", "ℤ", "ℚ"
  ]

-- | Parse an identifier, i.e. any non-reserved string beginning with
--   a letter and continuing with alphanumerics, underscores, and
--   apostrophes.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_'")
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                else return x

-- | Parse an 'identifier' and turn it into a 'Name'.
ident :: Parser (Name Term)
ident = string2Name <$> identifier

------------------------------------------------------------
-- Parser

-- | Parse the entire input as a program (with leading whitespace and
--   no leftovers).
prog :: Parser Prog
prog = whiteSpace *> parseProg <* eof

-- | Parse an entire program (a list of declarations ended by
--   semicolons).
parseProg :: Parser Prog
parseProg = parseDecl `sepEndBy` semi

-- | Parse a single declaration (either a type declaration or
--   definition).
parseDecl :: Parser Decl
parseDecl =
      try (DType <$> ident <*> (colon *> parseType))
  <|>      DDefn <$> ident <*> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm))

-- | Parse the entire input as a term (with leading whitespace and
--   no leftovers).
term :: Parser Term
term = whiteSpace *> parseTerm <* eof

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
parseTerm = ascribe <$> parseTerm' <*> optionMaybe (colon *> parseType)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty
    optionMaybe p = (Just <$> p) <|> pure Nothing

-- | Parse a non-atomic, non-ascribed term.
parseTerm' :: Parser Term
parseTerm' =
      TAbs <$> try (bind <$> ident <*> (mapsTo *> parseTerm'))
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
        <$> ((,) <$> ident <*> (symbol "=" *> (embed <$> parseTerm)))
        <*> (reserved "in" *> parseTerm)))

-- | Parse a case expression.
parseCase :: Parser Term
parseCase = TCase <$> some parseBranch

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
parseBranch = some (symbol "{") *> (flip bind <$> parseTerm <*> parseGuards)

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
  <|> PUnit <$ symbol "()"
  <|> PBool True  <$ (reserved "true" <|> reserved "True")
  <|> PBool False <$ (reserved "false" <|> reserved "False")
  <|> PNat <$> natural
  <|> try (PPair <$ symbol "("
                 <*> parsePattern
                 <*  symbol ","
                 <*> parsePattern
                 <*  symbol ")"
          )
  <|> parens parsePattern

-- | Parse a pattern.
parsePattern :: Parser Pattern
parsePattern =
      PInj <$> parseInj <*> parseAtomicPattern
  <|> PSucc <$ symbol "S" <*> parseAtomicPattern
  <|> parseAtomicPattern

-- | Parse an expression built out of unary and binary operators.
parseExpr :: Parser Term
parseExpr = makeExprParser parseAtom table <?> "expression"
  where
    table = [ [ infixL ""  TJuxt
              , unary "not" (TUn Not)
              ]
            , [ unary  "-" (TUn Neg)
              ]
            , [ post   "!" (TUn Fact)
              ]
            , [ infixR "^" (TBin Exp)
              ]
            , [ infixN "choose" (TBin Binom)
              ]
            , [ infixL "*" (TBin Mul)
              , infixL "/" (TBin Div)
              , infixL "%" (TBin Mod)
              , infixL "mod" (TBin Mod)
              ]
            , [ infixL "+" (TBin Add)
              , infixL "-" (TBin Sub)
              ]
            , [ infixN "==" (TBin Eq)
              , infixN "/=" (TBin Neq)
              , infixN "<"  (TBin Lt)
              , infixN ">"  (TBin Gt)
              , infixN "<=" (TBin Leq)
              , infixN ">=" (TBin Geq)
              , infixN "|"  (TBin Divides)
              , infixN "#"  (TBin RelPm)
              ]
            , [ infixR "&&"  (TBin And)
              , infixR "and" (TBin And)
              , infixR "∧"   (TBin And)
              ]
            , [ infixR "||" (TBin Or)
              , infixR "or" (TBin Or)
              , infixR "∨"  (TBin Or)
              ]
            ]

    unary  name fun = Prefix (reservedOp name >> return fun)
    post   name fun = Postfix (reservedOp name >> return fun)
    infixL name fun = InfixL (reservedOp name >> return fun)
    infixR name fun = InfixR (reservedOp name >> return fun)
    infixN name fun = InfixN (reservedOp name >> return fun)

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

-- | Parse a type.
parseType :: Parser Type
parseType = parseTypeExpr <|> parseAtomicType

-- | Parse a type expression built out of binary operators.
parseTypeExpr :: Parser Type
parseTypeExpr = makeExprParser parseAtomicType table <?> "type expression"
  where
    table = [ [ infixR "*" TyPair
              , infixR "×" TyPair ]
            , [ infixR "+" TySum
              , infixR "⊎" TySum
              ]
            , [ infixR "->" TyArr
              , infixR "→"  TyArr
              ]
            ]

    infixR name fun = InfixR (reservedOp name >> return fun)

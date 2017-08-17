{-# LANGUAGE GADTs           #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}

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
       ( -- * Parser type
         Parser, runParser

         -- * Lexer

         -- ** Basic lexemes
       , sc, lexeme, symbol, reservedOp
       , natural, reserved, reservedWords, ident

         -- ** Punctuation
       , parens, braces, angles, brackets
       , semi, comma, colon, dot, pipe
       , mapsTo

         -- * Disco parser

         -- ** Modules
       , wholeModule, parseModule, parseTopLevel, parseDecl

         -- ** Terms
       , term, parseTerm, parseTerm', parseExpr, parseAtom
       , parseList, parseEllipsis, parseListComp, parseQual
       , parseInj, parseLet, parseTypeOp

         -- ** Case and patterns
       , parseCase, parseBranch, parseGuards, parseGuard
       , parsePattern, parseAtomicPattern

         -- ** Types
       , parseType, parseAtomicType
       )
       where

import           Unbound.Generics.LocallyNameless (Embed, Name, bind, embed,
                                                   string2Name)

import           Text.Megaparsec                  hiding (runParser)
import qualified Text.Megaparsec                  as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Text.Megaparsec.Expr

import           Control.Applicative              (many, (<|>))
import           Control.Lens                     (makeLenses, use, (.=))
import           Control.Monad.State
import           Data.Char                        (isDigit)
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes)
import           Data.Ratio
import           Data.Void

import           Disco.AST.Surface
import           Disco.Syntax.Operators
import           Disco.Types

------------------------------------------------------------
-- Lexer

-- Some of the basic setup code for the parser taken from
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

-- | Extra custom state for the parser.
data ParserState = ParserState
  { _indentLevel :: Maybe Pos  -- ^ When this is @Just p@, everything
                               --   should be indented more than column
                               --   @p@.
  }

makeLenses ''ParserState

initParserState :: ParserState
initParserState = ParserState Nothing

-- | A parser is just a megaparsec parser of strings.  Megaparsec can
--   keep track of indentation.  For now we have no custom errors.
type Parser = StateT ParserState (MP.Parsec Void String)

-- | Run a parser from the initial state.
runParser :: Parser a -> FilePath -> String -> Either (ParseError Char Void) a
runParser = MP.runParser . flip evalStateT initParserState

-- | @indented p@ is just like @p@, except that every token must not
--   start in the first column.
indented :: Parser a -> Parser a
indented p = do
  indentLevel .= Just pos1
  res <- p
  indentLevel .= Nothing
  return res

-- | @requireIndent p@ possibly requires @p@ to be indented, depending
--   on the current '_indentLevel'.  Used in the definition of
--   'lexeme' and 'symbol'.
requireIndent :: Parser a -> Parser a
requireIndent p = do
  l <- use indentLevel
  case l of
    Just pos -> L.indentGuard sc GT pos >> p
    _        -> p

-- | Generically consume whitespace, including comments.
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

-- | Parse a lexeme, that is, a parser followed by consuming
--   whitespace.
lexeme :: Parser a -> Parser a
lexeme p = requireIndent $ L.lexeme sc p

-- | Parse a given string as a lexeme.
symbol :: String -> Parser String
symbol s = requireIndent $ L.symbol sc s

-- | Parse a reserved operator.
reservedOp :: String -> Parser ()
reservedOp s = (lexeme . try) (string s *> notFollowedBy (oneOf opChar))

-- | Characters that can occur in an operator symbol.
opChar :: [Char]
opChar = "!@#$%^&*~-+=|<>?/\\."

parens, braces, angles, brackets, fbrack, cbrack :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
fbrack    = between (symbol "⌊") (symbol "⌋")
cbrack    = between (symbol "⌈") (symbol "⌉")

semi, comma, colon, dot, pipe :: Parser String
semi      = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
pipe      = symbol "|"

ellipsis :: Parser String
ellipsis  = concat <$> ((:) <$> dot <*> some dot)

-- | The symbol that separates the variable binder from the body of a
--   lambda (either @↦@, @->@, or @|->@).
mapsTo :: Parser ()
mapsTo = reservedOp "↦" <|> reservedOp "->" <|> reservedOp "|->"

-- | Parse a natural number.
natural :: Parser Integer
natural = lexeme L.decimal

-- | Parse a nonnegative decimal of the form @xxx.yyyy[zzz]@, where
--   the @y@s and bracketed @z@s are optional.  For example, this
--   parser accepts all of the following:
--
--   > 2.
--   > 2.0
--   > 2.333
--   > 2.33[45]
--   > 2.[45]
--
--   The idea is that brackets surround an infinitely repeating
--   sequence of digits.
decimal :: Parser Rational
decimal = lexeme (readDecimal <$> some digit <* char '.'
                              <*> many digit
                              <*> optionMaybe (brackets (some digit))
                 )
  where
    digit = satisfy isDigit
    readDecimal a b mrep = read a % 1   -- integer part

                           -- next part is just b/10^n
                         + (if null b then 0 else read b) % (10^(length b))

                           -- repeating part
                         + readRep (length b) mrep
    readRep _      Nothing    = 0
    readRep offset (Just rep) = read rep % (10^offset * (10^(length rep) - 1))
      -- If s = 0.[rep] then 10^(length rep) * s = rep.[rep], so
      -- 10^(length rep) * s - s = rep, so
      --
      --   s = rep/(10^(length rep) - 1).
      --
      -- We also have to divide by 10^(length b) to shift it over
      -- past any non-repeating prefix.

-- | Parse a reserved word.
reserved :: String -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

-- | The list of all reserved words.
reservedWords :: [String]
reservedWords =
  [ "true", "false", "True", "False", "left", "right", "let", "in", "is"
  , "if", "when"
  , "otherwise", "and", "or", "not", "mod", "choose", "sqrt", "lg"
  , "enumerate", "count", "floor", "ceiling", "divides"
  , "Void", "Unit", "Bool"
  , "Nat", "Natural", "Int", "Integer", "Rational", "Fin"
  , "N", "Z", "Q", "ℕ", "ℤ", "ℚ", "QP", "ℚ⁺"
  , "forall"
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

-- | Optionally parse, succesfully returning 'Nothing' if the parse
--   fails.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = (Just <$> p) <|> pure Nothing

------------------------------------------------------------
-- Parser

-- | Parse the entire input as a module (with leading whitespace and
--   no leftovers).
wholeModule :: Parser Module
wholeModule = between sc eof parseModule

-- | Parse an entire module (a list of declarations ended by
--   semicolons).
parseModule :: Parser Module
parseModule = do
  topLevel <- many parseTopLevel
  let theMod = mkModule topLevel
  return theMod
  where
    groupTLs _ [] = []
    groupTLs revDocs (TLDoc doc : rest)
      = groupTLs (doc : revDocs) rest
    groupTLs revDocs (TLDecl decl@(DType{}) : rest)
      = (decl, Just (declName decl, reverse revDocs)) : groupTLs [] rest
    groupTLs _ (TLDecl defn : rest)
      = (defn, Nothing) : groupTLs [] rest

    defnGroups []                = []
    defnGroups (d@DType{}  : ds)  = d : defnGroups ds
    defnGroups (DDefn x bs : ds)  = DDefn x (bs ++ concatMap getClauses grp) : defnGroups rest
      where
        (grp, rest) = span matchDefn $ ds
        matchDefn (DDefn x' _) = x == x'
        matchDefn _ = False
        getClauses (DDefn _ cs) = cs
        getClauses _ = error "Impossible!"
          -- Impossible since we only call getClauses on things that
          -- passed matchDefn

    mkModule tls = Module (defnGroups decls) (M.fromList (catMaybes docs))
      where
        (decls, docs) = unzip $ groupTLs [] tls

-- | Parse a top level item (either documentation or a declaration).
parseTopLevel :: Parser TopLevel
parseTopLevel = TLDoc <$> parseDocThing <|> TLDecl <$> parseDecl

-- | Parse a documentation item: either a group of lines beginning
--   with @|||@ (text documentation), or a group beginning with @!!!@
--   (checked examples/properties).
parseDocThing :: Parser DocThing
parseDocThing
  =   DocString   <$> some parseDocString
  <|> DocProperty <$> parseProperty

-- | Parse one line of documentation beginning with @|||@.
parseDocString :: Parser String
parseDocString = L.nonIndented sc $
  string "|||"
  *> takeWhileP Nothing (`elem` " \t")
  *> takeWhileP Nothing (`notElem` "\r\n") <* sc

  -- Note we use string "|||" rather than symbol "|||" because we
  -- don't want it to consume whitespace afterwards (in particular a
  -- line with ||| by itself would cause symbol "|||" to consume the
  -- newline).

-- | Parse a top-level property/unit test, of the form
--
--   @!!! forall x1 : ty1, ..., xn : tyn. term@.
--
--   The forall is optional.
parseProperty :: Parser Property
parseProperty = L.nonIndented sc $ do
  _ <- symbol "!!!"
  indented $ do
    bind
      <$> (parseUniversal <|> return [])
      <*> parseTerm
  where
    parseUniversal =
         (() <$ symbol "∀" <|> reserved "forall")
      *> ((,) <$> ident <*> (colon *> parseType)) `sepBy` comma
      <* dot

-- | Parse a single top-level declaration (either a type declaration
--   or single definition clause).
parseDecl :: Parser Decl
parseDecl =
      try (DType <$> ident <*> (indented $ colon *> parseType))
  <|>      DDefn
           <$> ident
           <*> (indented $ (:[]) <$> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm)))

-- | Parse the entire input as a term (with leading whitespace and
--   no leftovers).
term :: Parser Term
term = between sc eof parseTerm

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom =
      brackets parseList
  <|> TBool True  <$ (reserved "true" <|> reserved "True")
  <|> TBool False <$ (reserved "false" <|> reserved "False")
  <|> TVar <$> ident
  <|> TRat <$> try decimal
  <|> TNat <$> natural
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseTypeOp
  <|> (TUn Floor . TParens) <$> fbrack parseTerm
  <|> (TUn Ceil . TParens) <$> cbrack parseTerm
  <|> parseCase
  <|> tuple <$> (parens (parseTerm `sepBy` comma))

-- | Parse a list-ish thing, like a literal list or a list
--   comprehension (not including the square brackets).
parseList :: Parser Term
parseList = nonEmptyList <|> return (TList [] Nothing)
  -- Careful to do this without backtracking, since backtracking can
  -- lead to bad performance in certain pathological cases (for
  -- example, a very deeply nested list).

  where
    -- Any non-empty list starts with a term, followed by some
    -- remainder (which could either be the rest of a literal list, or
    -- a list comprehension).  If there is no remainder just return a
    -- singleton list.
    nonEmptyList = do
      t <- parseTerm
      (listRemainder t <|> singletonList t)

    singletonList t = TList [t] <$> optionMaybe parseEllipsis

    -- The remainder of a list after the first term starts with either
    -- a pipe (for a comprehension) or a comma (for a literal list).
    listRemainder t = do
      s <- pipe <|> comma
      case s of
        "|" -> parseListComp t
        "," -> do
          -- Parse the rest of the terms in a literal list after the
          -- first, then an optional ellipsis, and return everything together.
          ts <- parseTerm `sepBy` comma
          e  <- optionMaybe parseEllipsis
          return $ TList (t:ts) e
        _   -> error "Impossible, got a symbol other than '|' or ',' in listRemainder"

parseEllipsis :: Parser (Ellipsis Term)
parseEllipsis = do
  _ <- ellipsis
  maybe Forever Until <$> optionMaybe parseTerm

{-

list          ::= '[' listContents ']'
listContents  ::= nonEmptyList | <empty>
nonEmptyList  ::= t [ell] | t listRemainder
ell           ::= '..' [t]
listRemainder ::= '|' listComp | ',' [t (,t)*] [ell]

-}

-- | Parse the part of a list comprehension after the | (without
--   square brackets), i.e. a list of qualifiers.
--
--   @q [,q]*@
parseListComp :: Term -> Parser Term
parseListComp t = do
  qs <- toTelescope <$> (parseQual `sepBy` comma)
  return (TListComp $ bind qs t)

parseQual :: Parser Qual
parseQual =
      try (QBind <$> ident <*> (selector *> (embed <$> parseTerm)))
  <|> QGuard <$> embed <$> parseTerm
  where
    selector = reservedOp "<-" <|> reserved "in"

tuple :: [Term] -> Term
tuple []  = TUnit
tuple [x] = TParens x
tuple t   = TTup t

-- | Parse an injection, i.e. either @left@ or @right@.
parseInj :: Parser Side
parseInj =
  L <$ reserved "left" <|> R <$ reserved "right"

-- | Parse a term, consisting of a @parseTerm'@ optionally
--   followed by an ascription.
parseTerm :: Parser Term
parseTerm = -- trace "parseTerm" $
  (ascribe <$> parseTerm' <*> optionMaybe (colon *> parseType))
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty

-- | Parse a non-atomic, non-ascribed term.
parseTerm' :: Parser Term
parseTerm' =
      TAbs <$> try (bind <$> some parseLambdaArg <*> (mapsTo *> parseTerm'))
  <|> parseLet
  <|> parseExpr
  <|> parseAtom

-- | Parse an argument to a lambda, either a variable or a binding of
--   the form @(x:ty)@.
parseLambdaArg :: Parser (Name Term, Embed (Maybe Type))
parseLambdaArg =
      parens ((,) <$> ident <*> (symbol ":" *> ((embed . Just) <$> parseType)))
  <|> (, embed Nothing) <$> ident

-- | Parse a let expression (@let x1 = t1, x2 = t2, ... in t@).
parseLet :: Parser Term
parseLet =
  TLet <$>
    (reserved "let" *>
      (bind
        <$> (toTelescope <$> (parseBinding `sepBy` comma))
        <*> (reserved "in" *> parseTerm)))

-- | Parse a single binding (@x [ : ty ] = t@).
parseBinding :: Parser Binding
parseBinding = do
  x   <- ident
  mty <- optionMaybe (colon *> parseType)
  t   <- symbol "=" *> (embed <$> parseTerm)
  return $ Binding mty x t

-- | Parse a case expression.
parseCase :: Parser Term
parseCase = between (symbol "{?") (symbol "?}") $
  TCase <$> parseBranch `sepBy` comma

-- | Parse one branch of a case expression.
parseBranch :: Parser Branch
parseBranch = flip bind <$> parseTerm <*> parseGuards

-- | Parse the list of guards in a branch.  @otherwise@ can be used
--   interchangeably with an empty list of guards.
parseGuards :: Parser (Telescope Guard)
parseGuards = (TelEmpty <$ reserved "otherwise") <|> (toTelescope <$> many parseGuard)

-- | Parse a single guard (either @if@ or @when@)
parseGuard :: Parser Guard
parseGuard =
  mkGuard <$> (embed <$> (guardWord *> parseTerm))
          <*> optionMaybe (reserved "is" *> parsePattern)
  where
    guardWord = reserved "when" <|> reserved "if"
    mkGuard t Nothing  = GBool t
    mkGuard t (Just p) = GPat  t p

-- | Parse an atomic pattern.
parseAtomicPattern :: Parser Pattern
parseAtomicPattern =
      PVar <$> ident
  <|> PWild <$ symbol "_"
  <|> PBool True  <$ (reserved "true" <|> reserved "True")
  <|> PBool False <$ (reserved "false" <|> reserved "False")
  <|> PNat <$> natural
  <|> PList <$> brackets (parsePattern `sepBy` comma)
  <|> tuplePat <$> (parens (parsePattern `sepBy` comma))

tuplePat :: [Pattern] -> Pattern
tuplePat []  = PUnit
tuplePat [x] = x
tuplePat t   = PTup t

-- | Parse a pattern.
parsePattern :: Parser Pattern
parsePattern = makeExprParser parseAtomicPattern table <?> "pattern"
  where
    table = [ [ prefix "left" (PInj L)
              , prefix "right" (PInj R)
              , prefix "S"   PSucc
              ]
            , [ infixR "::" PCons ]
            ]
    prefix name fun = Prefix (reserved name >> return fun)
    infixR name fun = InfixR (reservedOp name >> return fun)

-- | Parse an expression built out of unary and binary operators.
parseExpr :: Parser Term
parseExpr = (fixJuxtMul . fixChains) <$> (makeExprParser parseAtom table <?> "expression")
  where
    table
        -- Special case for function application, with highest
        -- precedence.  Note that we parse all juxtaposition as
        -- function application first; we later go through and turn
        -- some into multiplication (fixing up the precedence
        -- appropriately) based on a syntactic analysis.
      = [ InfixL (TApp <$ reservedOp "") ]

        -- get all other operators from the opTable
      : (map . concatMap) mkOpParser opTable

    mkOpParser :: OpInfo -> [Operator Parser Term]
    mkOpParser (OpInfo op syns _) = map (withOpFixity op) syns

    withOpFixity (UOpF fx op) syn = (ufxParser fx) (reservedOp syn >> return (TUn op))
    withOpFixity (BOpF fx op) syn = (bfxParser fx) (reservedOp syn >> return (TBin op))

    ufxParser Pre  = Prefix
    ufxParser Post = Postfix

    bfxParser InL  = InfixL
    bfxParser InR  = InfixR
    bfxParser In   = InfixN

    isChainable op = op `elem` [Eq, Neq, Lt, Gt, Leq, Geq, Divides]

    fixChains (TUn op t) = TUn op (fixChains t)
    fixChains (TBin op t1 (TBin op' t21 t22))
      | isChainable op && isChainable op' = TChain t1 (TLink op t21 : getLinks op' t22)
    fixChains (TBin op t1 t2) = TBin op (fixChains t1) (fixChains t2)
    fixChains (TApp t1 t2) = TApp (fixChains t1) (fixChains t2)

    -- Only recurse as long as we see TUn, TBin, or TApp which could
    -- have been generated by the expression parser.  If we see
    -- anything else we can stop.
    fixChains e = e

    getLinks op (TBin op' t1 t2)
      | isChainable op' = TLink op t1 : getLinks op' t2
    getLinks op e = [TLink op (fixChains e)]

    -- Find juxtapositions (parsed as function application) which
    -- syntactically have either a literal Nat or a parenthesized
    -- expression as the LHS, and turn them into multiplications.
    -- Then fix up the parse tree appropriately by rotating newly
    -- created multiplications up until their precedence is higher
    -- than the thing above them.

    fixJuxtMul :: Term -> Term

    -- Just recurse through TUn or TBin.  Fix up precedence on the way back up.
    fixJuxtMul (TUn op t)      = fixPrec $ TUn op (fixJuxtMul t)
    fixJuxtMul (TBin op t1 t2) = fixPrec $ TBin op (fixJuxtMul t1) (fixJuxtMul t2)

    -- Possibly turn a TApp into a multiplication, if the LHS looks
    -- like a multiplicative term.
    fixJuxtMul (TApp t1 t2)
      | isMultiplicativeTerm t1 = fixPrec $ TBin Mul (fixJuxtMul t1) (fixJuxtMul t2)
      | otherwise               = fixPrec $ TApp (fixJuxtMul t1) (fixJuxtMul t2)
    fixJuxtMul t = t

    -- A multiplicative term is either a Nat literal, or an explicitly
    -- parenthesized unary or binary operation.
    isMultiplicativeTerm :: Term -> Bool
    isMultiplicativeTerm (TNat _)            = True
    isMultiplicativeTerm (TParens (TUn  {})) = True
    isMultiplicativeTerm (TParens (TBin {})) = True
    isMultiplicativeTerm _                   = False

    -- Fix precedence by bubbling up any new TBin terms whose
    -- precedence is less than that of the operator above them.  We
    -- don't worry at all about fixing associativity, just precedence.

    fixPrec :: Term -> Term

    -- e.g.  2y! --> (2@y)! --> fixup --> 2 * (y!)
    fixPrec (TUn uop (TBin bop t1 t2))
      | bPrec bop < uPrec uop = case uopMap M.! uop of
          OpInfo (UOpF Pre  _) _ _ -> TBin bop (TUn uop t1) t2
          OpInfo (UOpF Post _) _ _ -> TBin bop t1 (TUn uop t2)
          _ -> error "Impossible! In fixPrec, uopMap contained OpInfo (BOpF ...)"

    fixPrec (TBin bop1 (TBin bop2 t1 t2) t3)
      | bPrec bop2 < bPrec bop1 = TBin bop2 t1 (fixPrec $ TBin bop1 t2 t3)

    -- e.g. x^2y --> x^(2@y) --> x^(2*y) --> (x^2) * y
    fixPrec (TBin bop1 t1 (TBin bop2 t2 t3))
      | bPrec bop2 < bPrec bop1 = TBin bop2 (fixPrec $ TBin bop1 t1 t2) t3

    fixPrec (TApp (TBin bop t1 t2) t3)
      | bPrec bop < funPrec = TBin bop t1 (fixPrec $ TApp t2 t3)

    fixPrec (TApp t1 (TBin bop t2 t3))
      | bPrec bop < funPrec = TBin bop (fixPrec $ TApp t1 t2) t3

    fixPrec t = t

-- | Parse an atomic type.
parseAtomicType :: Parser Type
parseAtomicType = (
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ (reserved "Bool" <|> reserved "B")
  <|> try parseTyFin
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyQP   <$ (reserved "QP" <|> reserved "ℚ⁺") -- TODO: come up with more/better ways to
                                                  --       represent nonegative rationals.
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
    -- This explicitly allows "List List N" to parse as List (List N).
    -- Since we don't have arbitrary application of higher-kinded type
    -- expressions, only application of an explicit set of
    -- right-associative single-argument type formers (e.g. List, and
    -- eventually things like Set), this can't cause any ambiguity.
  <|> TyList <$> (reserved "List" *> parseAtomicType)
  <|> parens parseType

  ) <?> "type"

parseTyFin :: Parser Type
parseTyFin = TyFin  <$> (reserved "Fin" *> natural)
         <|> TyFin  <$> (lexeme (string "Z" <|> string "ℤ") *> natural)

-- | Parse a type expression built out of binary operators.
parseType :: Parser Type
parseType = makeExprParser parseAtomicType table
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

parseTyOp :: Parser TyOp
parseTyOp =
        Enumerate <$ reserved "enumerate"
    <|> Count     <$ reserved "count"

parseTypeOp :: Parser Term
parseTypeOp = TTyOp <$> parseTyOp <*> parseAtomicType

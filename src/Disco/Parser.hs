{-# LANGUAGE GADTs           #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}

-----------------------------------------------------------------------------
--
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
       , lambda

         -- * Disco parser

         -- ** Modules
       , wholeModule, parseModule, parseTopLevel, parseDecl

         -- ** Terms
       , term, parseTerm, parseTerm', parseExpr, parseAtom
       , parseContainer, parseEllipsis, parseContainerComp, parseQual
       , parseInj, parseLet, parseTypeOp

         -- ** Case and patterns
       , parseCase, parseBranch, parseGuards, parseGuard
       , parsePattern, parseAtomicPattern

         -- ** Types
       , parseType, parseAtomicType
       , parseSigma
       )
       where

import           Unbound.Generics.LocallyNameless (Embed, Name, bind, embed,
                                                   fvAny, string2Name)

import           Text.Megaparsec                  hiding (runParser)
import qualified Text.Megaparsec                  as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Text.Megaparsec.Expr

import           Control.Applicative              (many, (<|>))
import           Control.Lens                     (makeLenses, toListOf, use,
                                                   (.=))
import           Control.Monad.State
import           Data.Char                        (isDigit)
import           Data.List                        (intercalate)
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
sc = L.space space1 lineComment empty {- no block comments in disco -}
  where
    lineComment  = L.skipLineComment "--"

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

-- | A literal ellipsis of two or more dots, @..@
ellipsis :: Parser String
ellipsis  = label "ellipsis (..)" $ concat <$> ((:) <$> dot <*> some dot)

-- | The symbol that starts an anonymous function (either a backslash
--   or a Greek λ).
lambda :: Parser String
lambda = symbol "\\" <|> symbol "λ"

-- | Parse a natural number.
natural :: Parser Integer
natural = lexeme L.decimal <?> "natural number"

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
--
--   We are careful to only parse a decimal terminated by a period if
--   there is not another period immediately following.  This way
--   something like @[1..]@ parses properly as the number 1 with an
--   ellipsis.
decimal :: Parser Rational
decimal = lexeme (readDecimal <$> some digit <* char '.' <* notFollowedBy (char '.')
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
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

-- | The list of all reserved words.
reservedWords :: [String]
reservedWords =
  [ "true", "false", "True", "False", "left", "right", "let", "in", "is"
  , "if", "when"
  , "otherwise", "and", "or", "not", "mod", "choose", "sqrt", "lg", "implies"
  , "size", "union", "U", "∪", "intersect", "∩"
  , "enumerate", "count", "floor", "ceiling", "divides"
  , "Void", "Unit", "Bool", "Boolean", "B", "Char", "C"
  , "Nat", "Natural", "Int", "Integer", "Frac", "Fractional", "Rational", "Fin"
  , "N", "Z", "F", "Q", "ℕ", "ℤ", "𝔽", "ℚ"
  , "forall", "type"
  , "import"
  ]

-- | Parse an identifier, i.e. any non-reserved string beginning with
--   a letter and continuing with alphanumerics, underscores, and
--   apostrophes.
identifier :: Parser Char -> Parser String
identifier begin = (lexeme . try) (p >>= check) <?> "variable name"
  where
    p       = (:) <$> begin <*> many (alphaNumChar <|> oneOf "_'")
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                else return x

-- | Parse an 'identifier' and turn it into a 'Name'.
ident :: Parser (Name Term)
ident = string2Name <$> (identifier letterChar)

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
  imports <- many parseImport
  topLevel <- many parseTopLevel
  let theMod = mkModule imports topLevel
  return theMod
  where
    groupTLs :: [DocThing] -> [TopLevel] -> ([(Decl, Maybe (Name Term, [DocThing]))])
    groupTLs _ [] = []
    groupTLs revDocs (TLDoc doc : rest)
      = groupTLs (doc : revDocs) rest
    groupTLs revDocs (TLDecl decl@(DType (TypeDecl x _)) : rest)
      = (decl, Just (x, reverse revDocs)) : groupTLs [] rest
    groupTLs _ (TLDecl defn : rest)
      = (defn, Nothing) : groupTLs [] rest

    defnGroups :: [Decl] -> [Decl]
    defnGroups []                = []
    defnGroups (d@DType{}  : ds)  = d : defnGroups ds
    defnGroups (d@DTyDef{} : ds)  = d : defnGroups ds
    defnGroups (DDefn (TermDefn x bs) : ds)  = DDefn (TermDefn x (bs ++ concatMap (\(TermDefn _ cs) -> cs) grp)) : defnGroups rest
      where
        (grp, rest) = matchDefn ds
        matchDefn :: [Decl] -> ([TermDefn], [Decl])
        matchDefn (DDefn t@(TermDefn x' _) : ds2) | x == x' = (t:ts, ds2')
          where
            (ts, ds2') = matchDefn ds2
        matchDefn ds2 = ([], ds2)

    mkModule imps tls = Module imps (defnGroups decls) (M.fromList (catMaybes docs))
      where
        (decls, docs) = unzip $ groupTLs [] tls

-- | Parse a top level item (either documentation or a declaration),
--   which must start at the left margin.
parseTopLevel :: Parser TopLevel
parseTopLevel = L.nonIndented sc $
      TLDoc  <$> parseDocThing
  <|> TLDecl <$> parseDecl

parseImport :: Parser ModName
parseImport = L.nonIndented sc $
  reserved "import" *> moduleName
  where
    moduleName = lexeme $
      intercalate "/" <$> (some alphaNumChar `sepBy` char '/') <* optional (string ".disco")

-- | Parse a documentation item: either a group of lines beginning
--   with @|||@ (text documentation), or a group beginning with @!!!@
--   (checked examples/properties).
parseDocThing :: Parser DocThing
parseDocThing
  =   DocString   <$> some parseDocString
  <|> DocProperty <$> parseProperty

-- | Parse one line of documentation beginning with @|||@.
parseDocString :: Parser String
parseDocString = label "documentation" $ L.nonIndented sc $
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
parseProperty = label "property" $ L.nonIndented sc $ do
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
parseDecl = try (DType <$> parseTyDecl) <|> DDefn <$> parseDefn <|> DTyDef <$> parseTyDefn

-- | Parse a top-level type declaration of the form @x : ty@.
parseTyDecl :: Parser TypeDecl
parseTyDecl = label "type declaration" $
  TypeDecl <$> ident <*> (indented $ colon *> parseSigma)

-- | Parse a definition of the form @x pat1 .. patn = t@.
parseDefn :: Parser TermDefn
parseDefn = label "definition" $
  TermDefn
  <$> ident
  <*> (indented $ (:[]) <$> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm)))

-- | Parse the definition of a user-defined algebraic data type.
parseTyDefn :: Parser TypeDefn
parseTyDefn = label "type defintion" $
  TypeDefn
  <$> (reserved "type" *> (parseTyDef)) <*> ((symbol "=") *> parseType)

-- | Parse the entire input as a term (with leading whitespace and
--   no leftovers).
term :: Parser Term
term = between sc eof parseTerm

-- | Parse a term, consisting of a @parseTerm'@ optionally
--   followed by an ascription.
parseTerm :: Parser Term
parseTerm = -- trace "parseTerm" $
  (ascribe <$> parseTerm' <*> optionMaybe (label "type annotation" $ colon *> parseSigma))
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty

-- | Parse a non-atomic, non-ascribed term.
parseTerm' :: Parser Term
parseTerm' = label "expression" $
      parseLambda
  <|> parseLet
  <|> parseExpr
  <|> parseAtom

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom = label "expression" $
       TBool True  <$ (reserved "true" <|> reserved "True")
  <|> TBool False <$ (reserved "false" <|> reserved "False")
  <|> TChar <$> lexeme (between (char '\'') (char '\'') L.charLiteral)
  <|> TString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  <|> TWild <$ symbol "_"
  <|> TVar <$> ident
  <|> TRat <$> try decimal
  <|> TNat <$> natural
  <|> TInj <$> parseInj <*> parseAtom
  <|> parseTypeOp
  <|> (TUn Floor . TParens) <$> fbrack parseTerm
  <|> (TUn Ceil . TParens) <$> cbrack parseTerm
  <|> parseCase
  <|> brackets (parseContainer ListContainer)
  <|> braces (parseContainer SetContainer)
  <|> tuple <$> (parens (parseTerm `sepBy` comma))

-- | Parse a container, like a literal list or set, or a
--   comprehension (not including the square brackets).
--
--
--   > list          ::= '[' listContents ']'
--   > listContents  ::= nonEmptyList | <empty>
--   > nonEmptyList  ::= t [ell] | t listRemainder
--   > ell           ::= '..' [t]
--   > listRemainder ::= '|' listComp | ',' [t (,t)*] [ell]

parseContainer :: Container -> Parser Term
parseContainer c = nonEmptyList <|> return (TContainer c [] Nothing)
  -- Careful to do this without backtracking, since backtracking can
  -- lead to bad performance in certain pathological cases (for
  -- example, a very deeply nested list).

  where
    -- Any non-empty list starts with a term, followed by some
    -- remainder (which could either be the rest of a literal list, or
    -- a list comprehension).  If there is no remainder just return a
    -- singleton list, optionally with an ellipsis.
    nonEmptyList = do
      t <- parseTerm
      (listRemainder t <|> singletonList t)

    singletonList t = TContainer c [t] <$> optionMaybe parseEllipsis

    -- The remainder of a list after the first term starts with either
    -- a pipe (for a comprehension) or a comma (for a literal list).
    listRemainder t = do
      s <- pipe <|> comma
      case s of
        "|" -> parseContainerComp c t
        "," -> do
          -- Parse the rest of the terms in a literal list after the
          -- first, then an optional ellipsis, and return everything together.
          ts <- parseTerm `sepBy` comma
          e  <- optionMaybe parseEllipsis
          return $ TContainer c (t:ts) e
        _   -> error "Impossible, got a symbol other than '|' or ',' in listRemainder"

-- | Parse an ellipsis at the end of a literal list, of the form
--   @.. [t]@.  Any number > 1 of dots may be used, just for fun.
parseEllipsis :: Parser (Ellipsis Term)
parseEllipsis = do
  _ <- ellipsis
  maybe Forever Until <$> optionMaybe parseTerm

-- | Parse the part of a list comprehension after the | (without
--   square brackets), i.e. a list of qualifiers.
--
--   @q [,q]*@
parseContainerComp :: Container -> Term -> Parser Term
parseContainerComp c t = do
  qs <- toTelescope <$> (parseQual `sepBy` comma)
  return (TContainerComp c $ bind qs t)

-- | Parse a qualifier in a comprehension: either a binder @x in t@ or
--   a guard @t@.
parseQual :: Parser Qual
parseQual = try parseSelection <|> parseQualGuard
  where
    parseSelection = label "membership expression (x in ...)" $
      QBind <$> ident <*> (selector *> (embed <$> parseTerm))
    selector = reservedOp "<-" <|> reserved "in"

    parseQualGuard = label "boolean expression" $
      QGuard <$> embed <$> parseTerm

-- | Turn a parenthesized list of zero or more terms into the
--   appropriate syntax node: zero terms @()@ is a TUnit; one term
--   @(t)@ is just the term itself (but we record the fact that it was
--   parenthesized, in order to correctly turn juxtaposition into
--   multiplication); two or more terms @(t1,t2,...)@ are a tuple.
tuple :: [Term] -> Term
tuple []  = TUnit
tuple [x] = TParens x
tuple t   = TTup t

-- | Parse an injection, i.e. either @left@ or @right@.
parseInj :: Parser Side
parseInj =
  L <$ reserved "left" <|> R <$ reserved "right"

-- | Parse an anonymous function.
parseLambda :: Parser Term
parseLambda =
  TAbs <$> (bind <$> (lambda *> some parseLambdaArg) <*> (dot *> parseTerm'))

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
  mty <- optionMaybe (colon *> parseSigma)
  t   <- symbol "=" *> (embed <$> parseTerm)
  return $ Binding (embed <$> mty) x t

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
parseGuard = parseGBool <|> parseGPat <|> parseGLet
  where
    parseGBool = GBool <$> (embed <$> (reserved "if" *> parseTerm))
    parseGPat  = GPat <$> (embed <$> (reserved "when" *> parseTerm))
                      <*> (reserved "is" *> parsePattern)
    parseGLet  = GLet <$> (reserved "let" *> parseBinding)

-- | Parse an atomic pattern, by parsing a term and then attempting to
--   convert it to a pattern.
parseAtomicPattern :: Parser Pattern
parseAtomicPattern = label "pattern" $ do
  t <- parseAtom
  case termToPattern t of
    Nothing -> fail $ "Invalid pattern: " ++ show t
    Just p  -> return p

-- | Parse a pattern, by parsing a term and then attempting to convert
--   it to a pattern.
parsePattern :: Parser Pattern
parsePattern = label "pattern" $ do
  t <- parseTerm
  case termToPattern t of
    Nothing -> fail $ "Invalid pattern: " ++ show t
    Just p  -> return p

-- | Attempt converting a term to a pattern.
termToPattern :: Term -> Maybe Pattern
termToPattern TWild       = Just $ PWild
termToPattern (TVar x)    = Just $ PVar x
termToPattern (TParens t) = termToPattern t
termToPattern TUnit       = Just $ PUnit
termToPattern (TBool b)   = Just $ PBool b
termToPattern (TNat n)    = Just $ PNat n
termToPattern (TChar c)   = Just $ PChar c
termToPattern (TString s) = Just $ PString s
termToPattern (TTup ts)   = PTup <$> mapM termToPattern ts
termToPattern (TInj s t)  = PInj s <$> termToPattern t

termToPattern (TBin Cons t1 t2)
  = PCons <$> termToPattern t1 <*> termToPattern t2

termToPattern (TBin Add t1 t2)
  = case (termToPattern t1, termToPattern t2) of
      (Just p, _)
        |  length (toListOf fvAny p) == 1
        && length (toListOf fvAny t2) == 0
        -> Just $ PAdd L p t2
      (_, Just p)
        |  length (toListOf fvAny p) == 1
        && length (toListOf fvAny t1) == 0
        -> Just $ PAdd R p t1
      _ -> Nothing
      -- If t1 is a pattern binding one variable, and t2 has no fvs,
      -- this can be a PAdd L.  Also vice versa for PAdd R.

termToPattern (TBin Mul t1 t2)
  = case (termToPattern t1, termToPattern t2) of
      (Just p, _)
        |  length (toListOf fvAny p) == 1
        && length (toListOf fvAny t2) == 0
        -> Just $ PMul L p t2
      (_, Just p)
        |  length (toListOf fvAny p) == 1
        && length (toListOf fvAny t1) == 0
        -> Just $ PMul R p t1
      _ -> Nothing
      -- If t1 is a pattern binding one variable, and t2 has no fvs,
      -- this can be a PMul L.  Also vice versa for PMul R.

termToPattern (TBin Sub t1 t2)
  = case termToPattern t1 of
      Just p
        |  length (toListOf fvAny p) == 1
        && length (toListOf fvAny t2) == 0
        -> Just $ PSub p t2
      _ -> Nothing
      -- If t1 is a pattern binding one variable, and t2 has no fvs,
      -- this can be a PSub.

      -- For now we don't handle the case of t - p, since it seems
      -- less useful (and desugaring it would require extra code since
      -- subtraction is not commutative).

termToPattern (TBin Div t1 t2)
  = PFrac <$> termToPattern t1 <*> termToPattern t2

termToPattern (TUn Neg t) = PNeg <$> termToPattern t

termToPattern (TContainer ListContainer ts Nothing)
  = PList <$> mapM termToPattern ts

termToPattern _           = Nothing

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

    bfxParser InL = InfixL
    bfxParser InR = InfixR
    bfxParser In  = InfixN

    isChainable op = op `elem` [Eq, Neq, Lt, Gt, Leq, Geq, Divides]

    -- Comparison chains like 3 < x < 5 first get parsed as 3 < (x <
    -- 5), which does not make sense.  This function looks for such
    -- nested comparison operators and turns them into a TChain.
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
    -- expression containing an operator as the LHS, and turn them
    -- into multiplications.  Then fix up the parse tree by rotating
    -- newly created multiplications up until their precedence is
    -- higher than the thing above them.

    fixJuxtMul :: Term -> Term

    -- Just recurse through TUn or TBin and fix precedence on the way back up.
    fixJuxtMul (TUn op t)      = fixPrec $ TUn op (fixJuxtMul t)
    fixJuxtMul (TBin op t1 t2) = fixPrec $ TBin op (fixJuxtMul t1) (fixJuxtMul t2)

    -- Possibly turn a TApp into a multiplication, if the LHS looks
    -- like a multiplicative term.  However, we must be sure to
    -- *first* recursively fix the subterms (particularly the
    -- left-hand one) *before* doing this analysis.  See
    -- https://github.com/disco-lang/disco/issues/71 .
    fixJuxtMul (TApp t1 t2)
      | isMultiplicativeTerm t1' = fixPrec $ TBin Mul t1' t2'
      | otherwise                = fixPrec $ TApp     t1' t2'
      where
        t1' = fixJuxtMul t1
        t2' = fixJuxtMul t2

    -- Otherwise we can stop recursing, since anything other than TUn,
    -- TBin, or TApp could not have been produced by the expression
    -- parser.
    fixJuxtMul t = t

    -- A multiplicative term is one that looks like either a natural
    -- number literal, or a unary or binary operation (optionally
    -- parenthesized).  For example, 3, (-2), and (x + 5) are all
    -- multiplicative terms, so 3x, (-2)x, and (x + 5)x all get parsed
    -- as multiplication.  On the other hand, (x y) is always parsed
    -- as function application, even if x and y both turn out to have
    -- numeric types; a variable like x does not count as a
    -- multiplicative term.  Likewise, (x y) z is parsed as function
    -- application, since (x y) is not a multiplicative term: it is
    -- parenthezised, but contains a TApp rather than a TBin or TUn.
    isMultiplicativeTerm :: Term -> Bool
    isMultiplicativeTerm (TNat _)    = True
    isMultiplicativeTerm (TUn {})    = True
    isMultiplicativeTerm (TBin {})   = True
    isMultiplicativeTerm (TParens t) = isMultiplicativeTerm t
    isMultiplicativeTerm _           = False

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
parseAtomicType = label "type" $
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ (reserved "Boolean" <|> reserved "Bool" <|> reserved "B")
  <|> TyC    <$ (reserved "Char" <|> reserved "C")
  <|> try parseTyFin
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyF    <$ (reserved "Fractional" <|> reserved "Frac" <|> reserved "F" <|> reserved "𝔽")
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
    -- This explicitly allows "List List N" to parse as List (List N).
    -- Since we don't have arbitrary application of higher-kinded type
    -- expressions, only application of an explicit set of
    -- right-associative single-argument type formers (e.g. List, and
    -- eventually things like Set), this can't cause any ambiguity.
  <|> TyList <$> (reserved "List" *> parseAtomicType)
  <|> TySet <$> (reserved "Set" *> parseAtomicType)
  <|> TyDef <$> parseTyDef
  <|> TyVar <$> parseTyVar
  <|> parens parseType

parseTyFin :: Parser Type
parseTyFin = TyFin  <$> (reserved "Fin" *> natural)
         <|> TyFin  <$> (lexeme (string "Z" <|> string "ℤ") *> natural)

parseTyDef :: Parser String
parseTyDef =  identifier upperChar

parseTyVar :: Parser (Name Type)
parseTyVar = string2Name <$> (identifier lowerChar)

parseSigma :: Parser Sigma
parseSigma = closeSigma <$> parseType

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

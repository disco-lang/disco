{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Parser
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser to convert concrete Disco syntax into an (untyped, surface
-- language) AST.
--
-----------------------------------------------------------------------------

module Disco.Parser
       ( -- * Parser type and utilities
         DiscoParseError(..), Parser, runParser, withExts, indented, thenIndented

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
       , wholeModule, parseModule, parseExtName, parseTopLevel, parseDecl
       , parseImport, parseModuleName

         -- ** Terms
       , term, parseTerm, parseTerm', parseExpr, parseAtom
       , parseContainer, parseEllipsis, parseContainerComp, parseQual
       , parseLet, parseTypeOp

         -- ** Case and patterns
       , parseCase, parseBranch, parseGuards, parseGuard
       , parsePattern, parseAtomicPattern

         -- ** Types
       , parseType, parseAtomicType
       , parsePolyTy
       )
       where

import           Unbound.Generics.LocallyNameless        (Name, bind, embed,
                                                          fvAny, string2Name)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                         hiding (runParser)
import qualified Text.Megaparsec                         as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer              as L

import           Control.Lens                            (makeLenses, toListOf,
                                                          use, (%=), (%~), (&),
                                                          (.=))
import           Control.Monad.State
import           Data.Char                               (isDigit)
import           Data.Foldable                           (asum)
import           Data.List                               (find, intercalate)
import qualified Data.Map                                as M
import           Data.Maybe                              (fromMaybe)
import           Data.Ratio
import           Data.Set                                (Set)
import qualified Data.Set                                as S

import           Disco.AST.Surface
import           Disco.Extensions
import           Disco.Module
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Types

------------------------------------------------------------
-- Lexer

-- Some of the basic setup code for the parser taken from
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

-- | Currently required indent level.
data IndentMode where
  NoIndent   :: IndentMode   -- ^ Don't require indent.
  ThenIndent :: IndentMode   -- ^ Parse one token without
                             --   indent, then switch to @Indent@.
  Indent     :: IndentMode   -- ^ Require everything to be indented at
                             --   least one space.

-- | Extra custom state for the parser.
data ParserState = ParserState
  { _indentMode  :: IndentMode  -- ^ Currently required level of indentation.
  , _enabledExts :: Set Ext     -- ^ Set of enabled language extensions
                                --   (some of which may affect parsing).
  }

makeLenses ''ParserState

initParserState :: ParserState
initParserState = ParserState NoIndent S.empty

data DiscoParseError
  = ReservedVarName String
  deriving (Show, Eq, Ord)

instance ShowErrorComponent DiscoParseError where
  showErrorComponent (ReservedVarName x) = "keyword \"" ++ x ++ "\" cannot be used as a variable name"
  errorComponentLen (ReservedVarName x) = length x

-- | A parser is a megaparsec parser of strings, with an extra layer
--   of state to keep track of the current indentation level and
--   language extensions, and some custom error messages.
type Parser = StateT ParserState (MP.Parsec DiscoParseError String)

-- | Run a parser from the initial state.
runParser :: Parser a -> FilePath -> String -> Either (ParseErrorBundle String DiscoParseError) a
runParser = MP.runParser . flip evalStateT initParserState

-- | Run a parser under a specified 'IndentMode'.
withIndentMode :: IndentMode -> Parser a -> Parser a
withIndentMode m p = do
  indentMode .= m
  res <- p
  indentMode .= NoIndent
  return res

-- | @indented p@ is just like @p@, except that every token must not
--   start in the first column.
indented :: Parser a -> Parser a
indented = withIndentMode Indent

-- | @indented p@ is just like @p@, except that every token after the
--   first must not start in the first column.
thenIndented :: Parser a -> Parser a
thenIndented = withIndentMode ThenIndent

-- | @requireIndent p@ possibly requires @p@ to be indented, depending
--   on the current '_indentMode'.  Used in the definition of
--   'lexeme' and 'symbol'.
requireIndent :: Parser a -> Parser a
requireIndent p = do
  l <- use indentMode
  case l of
    ThenIndent -> do
      a <- p
      indentMode .= Indent
      return a
    Indent     -> L.indentGuard sc GT pos1 >> p
    NoIndent   -> p

-- | Locally set the enabled extensions within a subparser.
withExts :: Set Ext -> Parser a -> Parser a
withExts exts p = do
  oldExts <- use enabledExts
  enabledExts .= exts
  a <- p
  enabledExts .= oldExts
  return a

-- | Locally enable some additional extensions within a subparser.
withAdditionalExts :: Set Ext -> Parser a -> Parser a
withAdditionalExts exts p = do
  oldExts <- use enabledExts
  enabledExts %= S.union exts
  a <- p
  enabledExts .= oldExts
  return a

-- | Ensure that a specific extension is enabled, fail if not.
ensureEnabled :: Ext -> Parser ()
ensureEnabled e = do
  exts <- use enabledExts
  guard $ e `S.member` exts

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
opChar = "~!@#$%^&*-+=|<>?/\\."

parens, braces, angles, brackets, bagdelims, fbrack, cbrack :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
bagdelims = between (symbol "⟅") (symbol "⟆")
fbrack    = between (symbol "⌊") (symbol "⌋")
cbrack    = between (symbol "⌈") (symbol "⌉")

semi, comma, colon, dot, pipe, hash :: Parser String
semi      = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
pipe      = symbol "|"
hash      = symbol "#"

-- | A literal ellipsis of two or more dots, @..@
ellipsis :: Parser String
ellipsis  = label "ellipsis (..)" $ concat <$> ((:) <$> dot <*> some dot)

-- | The symbol that starts an anonymous function (either a backslash
--   or a Greek λ).
lambda :: Parser String
lambda = symbol "\\" <|> symbol "λ"

forall :: Parser ()
forall = () <$ symbol "∀" <|> reserved "forall"

exists :: Parser ()
exists = () <$ symbol "∃" <|> reserved "exists"

-- | Parse a natural number.
natural :: Parser Integer
natural = lexeme L.decimal <?> "natural number"

-- | Parse a nonnegative decimal of the form @xxx.yyyy[zzz]@, where
--   the @y@s and bracketed @z@s are both optional as long as the
--   other is present.  (In other words, there must be something after
--   the period.) For example, this parser accepts all of the
--   following:
--
--   > 2.0
--   > 2.333
--   > 2.33[45]
--   > 2.[45]
--
--   The idea is that brackets surround an infinitely repeating
--   sequence of digits.
--
--   We used to accept @2.@ with no trailing digits, but no longer do.
--   See https://github.com/disco-lang/disco/issues/245 and Note
--   [Trailing period].
decimal :: Parser Rational
decimal = lexeme (readDecimal <$> some digit <* char '.'
                              <*> fractionalPart
                 )
  where
    digit = satisfy isDigit
    fractionalPart =
          -- either some digits optionally followed by bracketed digits...
          (,) <$> some digit <*> optional (brackets (some digit))
          -- ...or just bracketed digits.
      <|> ([],) <$> (Just <$> brackets (some digit))

    readDecimal a (b, mrep)
      = read a % 1   -- integer part

        -- next part is just b/10^n
        + (if null b then 0 else read b) % (10^length b)

        -- repeating part
        + readRep (length b) mrep

    readRep _      Nothing    = 0
    readRep offset (Just rep) = read rep % (10^offset * (10^length rep - 1))
      -- If s = 0.[rep] then 10^(length rep) * s = rep.[rep], so
      -- 10^(length rep) * s - s = rep, so
      --
      --   s = rep/(10^(length rep) - 1).
      --
      -- We also have to divide by 10^(length b) to shift it over
      -- past any non-repeating prefix.

-- ~~~~ Note [Trailing period]
--
-- We used to accept numbers with nothing after the trailing period,
-- such as @2.@. However, this caused some problems with parsing:
--
--   - First, https://github.com/disco-lang/disco/issues/99 which we
--     solved by making sure there was not another period after the
--     trailing period.
--   - Next, https://github.com/disco-lang/disco/issues/245.
--
-- I first tried solving #245 by disallowing *any* operator character
-- after the trailing period, but then some tests in the test suite
-- started failing, where we had written things like @1./(10^5)@.  The
-- problem is that when a period is followed by another operator
-- symbol, sometimes we might want them to be parsed as an operator
-- (as in @2.-4@, #245), and sometimes we might not (as in
-- @1./(10^5)@).  So in the end it seems simpler and cleaner to
-- require at least a 0 digit after the period --- just like pretty
-- much every other programming language and just like standard
-- mathematical practice.

-- | Parse a reserved word.
reserved :: String -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

-- | The list of all reserved words.
reservedWords :: [String]
reservedWords =
  [ "unit", "true", "false", "True", "False", "let", "in", "is"
  , "if", "when"
  , "otherwise", "and", "or", "mod", "choose", "implies"
  , "min", "max"
  , "union", "∪", "intersect", "∩", "subset", "⊆", "elem", "∈"
  , "enumerate", "count", "divides"
  , "Void", "Unit", "Bool", "Boolean", "Proposition", "Prop", "Char"
  , "Nat", "Natural", "Int", "Integer", "Frac", "Fractional", "Rational", "Fin"
  , "List", "Bag", "Set", "Graph", "Map"
  , "N", "Z", "F", "Q", "ℕ", "ℤ", "𝔽", "ℚ"
  , "∀", "forall", "∃", "exists", "type"
  , "import", "using"
  ]

-- | Parse an identifier, i.e. any non-reserved string beginning with
--   a given type of character and continuing with alphanumerics,
--   underscores, and apostrophes.
identifier :: Parser Char -> Parser String
identifier begin = (lexeme . try) (p >>= check) <?> "variable name"
  where
    p       = (:) <$> begin <*> many identChar
    identChar = alphaNumChar <|> oneOf "_'"
    check x
      | x `elem` reservedWords = do
          -- back up to beginning of bad token to report correct position
          updateParserState (\s -> s { stateOffset = stateOffset s - length x })
          customFailure $ ReservedVarName x
      | otherwise = return x

-- | Parse an 'identifier' and turn it into a 'Name'.
ident :: Parser (Name Term)
ident = string2Name <$> identifier letterChar

------------------------------------------------------------
-- Parser

-- | Results from parsing a block of top-level things.
data TLResults = TLResults
  { _tlDecls :: [Decl]
  , _tlDocs  :: [(Name Term, [DocThing])]
  , _tlTerms :: [Term]
  }

emptyTLResults :: TLResults
emptyTLResults = TLResults [] [] []

makeLenses ''TLResults

-- | Parse the entire input as a module (with leading whitespace and
--   no leftovers).
wholeModule :: LoadingMode -> Parser Module
wholeModule = between sc eof . parseModule

-- | Parse an entire module (a list of declarations ended by
--   semicolons).  The 'LoadingMode' parameter tells us whether to
--   include or replace any language extensions enabled at the top
--   level.  We include them when parsing a module entered at the
--   REPL, and replace them when parsing a standalone module.
parseModule :: LoadingMode -> Parser Module
parseModule mode = do
  exts     <- S.fromList <$> many parseExtension
  let extFun = case mode of
        Standalone -> withExts
        REPL       -> withAdditionalExts

  extFun exts $ do
    imports  <- many parseImport
    topLevel <- many parseTopLevel
    let theMod = mkModule exts imports topLevel
    return theMod
    where
      groupTLs :: [DocThing] -> [TopLevel] -> TLResults
      groupTLs _ [] = emptyTLResults
      groupTLs revDocs (TLDoc doc : rest)
        = groupTLs (doc : revDocs) rest
      groupTLs revDocs (TLDecl decl@(DType (TypeDecl x _)) : rest)
        = groupTLs [] rest
          & tlDecls %~ (decl :)
          & tlDocs  %~ ((x, reverse revDocs) :)
      groupTLs _ (TLDecl defn : rest)
        = groupTLs [] rest
          & tlDecls %~ (defn :)
      groupTLs _ (TLExpr t : rest)
        = groupTLs [] rest & tlTerms %~ (t:)

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

      mkModule exts imps tls = Module exts imps (defnGroups decls) docs terms
        where
          TLResults decls docs terms = groupTLs [] tls

-- | Parse an extension.
parseExtension :: Parser Ext
parseExtension = L.nonIndented sc $
  reserved "using" *> parseExtName

-- | Parse the name of a language extension (case-insensitive).
parseExtName :: Parser Ext
parseExtName = choice (map parseOneExt allExtsList) <?> "language extension name"
  where
    parseOneExt ext = ext <$ lexeme (string' (show ext) :: Parser String)

-- | Parse an import, of the form @import <modulename>@.
parseImport :: Parser String
parseImport = L.nonIndented sc $
  reserved "import" *> parseModuleName

-- | Parse the name of a module.
parseModuleName :: Parser String
parseModuleName = lexeme $
  intercalate "/" <$> (some (alphaNumChar <|> oneOf "_-") `sepBy` char '/') <* optional (string ".disco")

-- | Parse a top level item (either documentation or a declaration),
--   which must start at the left margin.
parseTopLevel :: Parser TopLevel
parseTopLevel = L.nonIndented sc $
      TLDoc  <$> parseDocThing
  <|> TLDecl <$> try parseDecl
  <|> TLExpr <$> thenIndented parseTerm

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
parseProperty :: Parser Term
parseProperty = label "property" $ L.nonIndented sc $ do
  _ <- symbol "!!!"
  indented parseTerm

-- | Parse a single top-level declaration (either a type declaration
--   or single definition clause).
parseDecl :: Parser Decl
parseDecl = try (DType <$> parseTyDecl) <|> DDefn <$> parseDefn <|> DTyDef <$> parseTyDefn

-- | Parse a top-level type declaration of the form @x : ty@.
parseTyDecl :: Parser TypeDecl
parseTyDecl = label "type declaration" $
  TypeDecl <$> ident <*> indented (colon *> parsePolyTy)

-- | Parse a definition of the form @x pat1 .. patn = t@.
parseDefn :: Parser TermDefn
parseDefn = label "definition" $
  TermDefn
  <$> ident
  <*> indented ((:[]) <$> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm)))

-- | Parse the definition of a user-defined algebraic data type.
parseTyDefn :: Parser TypeDefn
parseTyDefn = label "type defintion" $ do
  reserved "type"
  indented $ do
    name <- parseTyDef
    args <- fromMaybe [] <$> optional (parens $ parseTyVarName `sepBy1` comma)
    _ <- symbol "="
    TypeDefn name args <$> parseType

-- | Parse the entire input as a term (with leading whitespace and
--   no leftovers).
term :: Parser Term
term = between sc eof parseTerm

-- | Parse a term, consisting of a @parseTerm'@ optionally
--   followed by an ascription.
parseTerm :: Parser Term
parseTerm = -- trace "parseTerm" $
  ascribe <$> parseTerm' <*> optional (label "type annotation" $ colon *> parsePolyTy)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = TAscr t ty

-- | Parse a non-atomic, non-ascribed term.
parseTerm' :: Parser Term
parseTerm' = label "expression" $
      parseQuantified
  <|> parseLet
  <|> parseExpr
  <|> parseAtom

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom = label "expression" $
      parseUnit
  <|> TBool True  <$ (reserved "true" <|> reserved "True")
  <|> TBool False <$ (reserved "false" <|> reserved "False")
  <|> TChar <$> lexeme (between (char '\'') (char '\'') L.charLiteral)
  <|> TString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  <|> TWild <$ try parseWild
  <|> TPrim <$> try parseStandaloneOp

  -- Note primitives are NOT reserved words, so they are just parsed
  -- as identifiers.  This means that it is possible to shadow a
  -- primitive in a local context, as it should be.  Vars are turned
  -- into prims at scope-checking time: if a var is not in scope but
  -- there is a prim of that name then it becomes a TPrim.  See the
  -- 'typecheck Infer (TVar x)' case in Disco.Typecheck.
  <|> TVar <$> ident
  <|> TPrim <$> (ensureEnabled Primitives *> parsePrim)
  <|> TRat <$> try decimal
  <|> TNat <$> natural
  <|> parseTypeOp
  <|> TApp (TPrim PrimFloor) . TParens <$> fbrack parseTerm
  <|> TApp (TPrim PrimCeil)  . TParens <$> cbrack parseTerm
  <|> parseCase
  <|> bagdelims (parseContainer BagContainer)
  <|> braces    (parseContainer SetContainer)
  <|> brackets  (parseContainer ListContainer)
  <|> tuple <$> parens (parseTerm `sepBy1` comma)

parseUnit :: Parser Term
parseUnit = TUnit <$ (reserved "unit" <|> void (symbol "■"))

-- | Parse a wildcard, which is an underscore that isn't the start of
--   an identifier.
parseWild :: Parser ()
parseWild = (lexeme . try . void) $
  string "_" <* notFollowedBy (alphaNumChar <|> oneOf "_'")

-- | Parse a standalone operator name with tildes indicating argument
--   slots, e.g. ~+~ for the addition operator.
parseStandaloneOp :: Parser Prim
parseStandaloneOp = asum $ concatMap mkStandaloneOpParsers (concat opTable)
  where
    mkStandaloneOpParsers :: OpInfo -> [Parser Prim]
    mkStandaloneOpParsers (OpInfo (UOpF Pre uop) syns _)
      = map (\syn -> PrimUOp uop <$ try (lexeme (string syn >> char '~'))) syns
    mkStandaloneOpParsers (OpInfo (UOpF Post uop) syns _)
      = map (\syn -> PrimUOp uop <$ try (lexeme (char '~' >> string syn))) syns
    mkStandaloneOpParsers (OpInfo (BOpF _ bop) syns _)
      = map (\syn -> PrimBOp bop <$ try (lexeme (char '~' >> string syn >> char '~'))) syns

    -- XXX TODO: improve the above so it first tries to parse a ~,
    --   then parses any postfix or infix thing; or else it looks for
    --   a prefix thing followed by a ~.  This will get rid of the
    --   need for 'try' and also potentially improve error messages.
    --   The below may come in useful.

    -- flatOpTable = concat opTable

    -- prefixOps  = [ (uop, syns) | (OpInfo (UOpF Pre uop) syns _)  <- flatOpTable ]
    -- postfixOps = [ (uop, syns) | (OpInfo (UOpF Post uop) syns _) <- flatOpTable ]
    -- infixOps   = [ (bop, syns) | (OpInfo (BOpF _ bop) syns _)    <- flatOpTable ]

-- | Parse a primitive name starting with a $.
parsePrim :: Parser Prim
parsePrim = do
  void (char '$')
  x <- identifier letterChar
  case find ((==x) . primSyntax) primTable of
    Just (PrimInfo p _ _) -> return p
    Nothing               -> fail ("Unrecognized primitive $" ++ x)

-- | Parse a container, like a literal list, set, bag, or a
--   comprehension (not including the square or curly brackets).
--
-- @
-- <container>
--   ::= '[' <container-contents> ']'
--     | '{' <container-contents> '}'
--
-- <container-contents>
--   ::= empty | <nonempty-container>
--
-- <nonempty-container>
--   ::= <term> [ <ellipsis> ]
--     | <term> <container-end>
--
-- <container-end>
--   ::= '|' <comprehension>
--     | ',' [ <term> (',' <item>)* ] [ <ellipsis> ]
--
-- <comprehension> ::= <qual> [ ',' <qual> ]*
--
-- <qual>
--   ::= <ident> 'in' <term>
--     | <term>
--
-- <ellipsis> ::= '..' [ <term> ]
-- @

parseContainer :: Container -> Parser Term
parseContainer c = nonEmptyContainer <|> return (TContainer c [] Nothing)
  -- Careful to do this without backtracking, since backtracking can
  -- lead to bad performance in certain pathological cases (for
  -- example, a very deeply nested list).

  where
    -- Any non-empty container starts with a term, followed by some
    -- remainder (which could either be the rest of a literal
    -- container, or a container comprehension).  If there is no
    -- remainder just return a singleton container, optionally with an
    -- ellipsis.
    nonEmptyContainer = do
      t <- parseRepTerm

      containerRemainder t <|> singletonContainer t

    parseRepTerm = do
      t <- parseTerm
      n <- optional $ do
        guard (c == BagContainer)
        void hash
        parseTerm
      return (t, n)

    singletonContainer t = TContainer c [t] <$> optional parseEllipsis

    -- The remainder of a container after the first term starts with
    -- either a pipe (for a comprehension) or a comma (for a literal
    -- container).
    containerRemainder :: (Term, Maybe Term) -> Parser Term
    containerRemainder (t,n) = do
      s <- pipe <|> comma
      case (s, n) of
        ("|", Nothing) -> parseContainerComp c t
        ("|", Just _)  -> fail "no comprehension with bag repetition syntax"
        (",", _)       -> do
          -- Parse the rest of the terms in a literal container after
          -- the first, then an optional ellipsis, and return
          -- everything together.
          ts <- parseRepTerm `sepBy` comma
          e  <- optional parseEllipsis

          return $ TContainer c ((t,n):ts) e
        _   -> error "Impossible, got a symbol other than '|' or ',' in containerRemainder"

-- | Parse an ellipsis at the end of a literal list, of the form
--   @.. t@.  Any number > 1 of dots may be used, just for fun.
parseEllipsis :: Parser (Ellipsis Term)
parseEllipsis = do
  _ <- ellipsis
  Until <$> parseTerm

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
      QGuard . embed <$> parseTerm

-- | Turn a parenthesized list of zero or more terms into the
--   appropriate syntax node: one term @(t)@ is just the term itself
--   (but we record the fact that it was parenthesized, in order to
--   correctly turn juxtaposition into multiplication); two or more
--   terms @(t1,t2,...)@ are a tuple.
tuple :: [Term] -> Term
tuple [x] = TParens x
tuple t   = TTup t

-- | Parse a quantified abstraction (λ, ∀, ∃).
parseQuantified :: Parser Term
parseQuantified =
  TAbs <$> parseQuantifier
       <*> (bind <$> parsePattern `sepBy` comma <*> (dot *> parseTerm))

-- | Parse a quantifier symbol (lambda, forall, or exists).
parseQuantifier :: Parser Quantifier
parseQuantifier =
      Lam <$ lambda
  <|> All <$ forall
  <|> Ex  <$ exists

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
  mty <- optional (colon *> parsePolyTy)
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
termToPattern TWild       = Just PWild
termToPattern (TVar x)    = Just $ PVar x
termToPattern (TParens t) = termToPattern t
termToPattern TUnit       = Just PUnit
termToPattern (TBool b)   = Just $ PBool b
termToPattern (TNat n)    = Just $ PNat n
termToPattern (TChar c)   = Just $ PChar c
termToPattern (TString s) = Just $ PString s
termToPattern (TTup ts)   = PTup <$> mapM termToPattern ts
termToPattern (TApp (TVar i) t)
  | i == string2Name "left"  = PInj L <$> termToPattern t
  | i == string2Name "right" = PInj R <$> termToPattern t
-- termToPattern (TInj s t)  = PInj s <$> termToPattern t

termToPattern (TAscr t s) = case s of
  Forall (unsafeUnbind -> ([], s')) -> PAscr <$> termToPattern t <*> pure s'
  _                                 -> Nothing

termToPattern (TBin Cons t1 t2)
  = PCons <$> termToPattern t1 <*> termToPattern t2

termToPattern (TBin Add t1 t2)
  = case (termToPattern t1, termToPattern t2) of
      (Just p, _)
        |  length (toListOf fvAny p) == 1
        && null (toListOf fvAny t2)
        -> Just $ PAdd L p t2
      (_, Just p)
        |  length (toListOf fvAny p) == 1
        && null (toListOf fvAny t1)
        -> Just $ PAdd R p t1
      _ -> Nothing
      -- If t1 is a pattern binding one variable, and t2 has no fvs,
      -- this can be a PAdd L.  Also vice versa for PAdd R.

termToPattern (TBin Mul t1 t2)
  = case (termToPattern t1, termToPattern t2) of
      (Just p, _)
        |  length (toListOf fvAny p) == 1
        && null (toListOf fvAny t2)
        -> Just $ PMul L p t2
      (_, Just p)
        |  length (toListOf fvAny p) == 1
        && null (toListOf fvAny t1)
        -> Just $ PMul R p t1
      _ -> Nothing
      -- If t1 is a pattern binding one variable, and t2 has no fvs,
      -- this can be a PMul L.  Also vice versa for PMul R.

termToPattern (TBin Sub t1 t2)
  = case termToPattern t1 of
      Just p
        |  length (toListOf fvAny p) == 1
        && null (toListOf fvAny t2)
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
  = PList <$> mapM (termToPattern . fst) ts

termToPattern _           = Nothing

-- | Parse an expression built out of unary and binary operators.
parseExpr :: Parser Term
parseExpr = fixJuxtMul . fixChains <$> (makeExprParser parseAtom table <?> "expression")
  where
    table
        -- Special case for function application, with highest
        -- precedence.  Note that we parse all juxtaposition as
        -- function application first; we later go through and turn
        -- some into multiplication (fixing up the precedence
        -- appropriately) based on a syntactic analysis.
      = [ InfixL (TApp <$ string "") ]

        -- get all other operators from the opTable
      : (map . concatMap) mkOpParser opTable

    mkOpParser :: OpInfo -> [Operator Parser Term]
    mkOpParser (OpInfo op syns _) = map (withOpFixity op) syns

    withOpFixity (UOpF fx op) syn
      = ufxParser fx ((reservedOp syn <?> "operator") >> return (TUn op))

    withOpFixity (BOpF fx op) syn
      = bfxParser fx ((reservedOp syn <?> "operator") >> return (TBin op))

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
    -- <https://github.com/disco-lang/disco/issues/71> .
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
    isMultiplicativeTerm TUn{}       = True
    isMultiplicativeTerm TBin{}      = True
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

    fixPrec t = t

-- | Parse an atomic type.
parseAtomicType :: Parser Type
parseAtomicType = label "type" $
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ (reserved "Boolean" <|> reserved "Bool")
  <|> TyProp <$ (reserved "Proposition" <|> reserved "Prop")
  <|> TyC    <$ reserved "Char"
  -- <|> try parseTyFin
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyF    <$ (reserved "Fractional" <|> reserved "Frac" <|> reserved "F" <|> reserved "𝔽")
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
  <|> TyCon  <$> parseCon <*> (fromMaybe [] <$> optional (parens (parseType `sepBy1` comma)))
  <|> TyVar  <$> parseTyVar
  <|> parens parseType

-- parseTyFin :: Parser Type
-- parseTyFin = TyFin  <$> (reserved "Fin" *> natural)
--          <|> TyFin  <$> (lexeme (string "Z" <|> string "ℤ") *> natural)

parseCon :: Parser Con
parseCon =
      CList  <$  reserved "List"
  <|> CBag   <$  reserved "Bag"
  <|> CSet   <$  reserved "Set"
  <|> CGraph <$  reserved "Graph"
  <|> CMap   <$  reserved "Map"
  <|> CUser  <$> parseTyDef

parseTyDef :: Parser String
parseTyDef =  identifier upperChar

parseTyVarName :: Parser String
parseTyVarName = identifier lowerChar

parseTyVar :: Parser (Name Type)
parseTyVar = string2Name <$> parseTyVarName

parsePolyTy :: Parser PolyType
parsePolyTy = closeType <$> parseType

-- | Parse a type expression built out of binary operators.
parseType :: Parser Type
parseType = makeExprParser parseAtomicType table
  where
    table = [ [ infixR "*" (:*:)
              , infixR "×" (:*:) ]
            , [ infixR "+" (:+:)
              , infixR "⊎" (:+:)
              ]
            , [ infixR "->" (:->:)
              , infixR "→"  (:->:)
              ]
            ]

    infixR name fun = InfixR (reservedOp name >> return fun)

parseTyOp :: Parser TyOp
parseTyOp =
        Enumerate <$ reserved "enumerate"
    <|> Count     <$ reserved "count"

parseTypeOp :: Parser Term
parseTypeOp = TTyOp <$> parseTyOp <*> parseAtomicType

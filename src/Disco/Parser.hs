{-# LANGUAGE GADTs           #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
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
         ParserState(..), initParserState, Parser, runParser

         -- ** Parser utilities

       , tabStack, numActive, isSep
       , getColumn, pushTab

         -- * Lexer

         -- ** Layout
       , open, sep, close, block
       , requireSep, requireClose, requireVirtual

         -- ** Basic lexemes
       , consumeWhitespace
       , whitespace, lexeme, symbol, reservedOp
       , natural, reserved, reservedWords, identifier, ident

         -- ** Punctuation
       , parens, braces, angles, brackets
       , semi, comma, colon, dot
       , mapsTo

         -- * Disco parser

         -- ** Modules
       , wholeModule, parseModule, parseDecl

         -- ** Terms
       , term, parseTerm, parseTerm', parseExpr, parseAtom
       , parseInj, parseLet, parseTypeOp

         -- ** Case and patterns
       , parseCase, parseBranch, parseGuards, parseGuard
       , parsePattern, parseAtomicPattern

         -- ** Types
       , parseType, parseTypeExpr, parseAtomicType
       )
       where

-- import           Debug.Trace

import           Unbound.LocallyNameless (Name, bind, embed, rebind,
                                          string2Name)

import           Text.Megaparsec         hiding (runParser)
import qualified Text.Megaparsec         as MP
import qualified Text.Megaparsec.Char    as C
import           Text.Megaparsec.Expr    hiding (Operator)
import qualified Text.Megaparsec.Lexer   as L
import qualified Text.Megaparsec.String  as MP

import           Control.Applicative     (many, (<|>))
import           Control.Lens            hiding (op)
import           Control.Monad.State
import           Data.Char               (isDigit, isSpace)
import           Data.Either             (isRight)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import           Data.Ratio
import           Text.Printf

import           Disco.AST.Surface
import           Disco.Types

------------------------------------------------------------
-- Lexer

-- | Custom parser state.
data ParserState = ParserState
  { _tabStack  :: [(Pos, String)]   -- ^ Stack of alignment columns.
                                    --   Each column has a position
                                    --   and a delimiter which should
                                    --   always appear in that column.
                                    --   The delimiter may be the
                                    --   empty string.
  , _numActive :: Int               -- ^ Number of currently active
                                    --   open blocks. May be larger
                                    --   than the size of the
                                    --   tabStack, in which case we
                                    --   expect to encounter some
                                    --   virtual 'close' tokens.
  , _isSep     :: Bool              -- ^ Whether we are expecting to
                                    --   see a virtual separator
                                    --   token.
  }

makeLenses ''ParserState

-- | The initial parser state: empty tab stack, no active blocks, and
--   not expecting a separator.
initParserState :: ParserState
initParserState = ParserState [] 0 False

-- | A parser is a megaparsec parser with some custom state added.
type Parser a = StateT ParserState MP.Parser a

-- | Run a parser from the initial state.
runParser :: Parser a -> FilePath -> String -> Either (ParseError Char Dec) a
runParser = MP.runParser . flip evalStateT initParserState

-- | Get the current column.
getColumn :: Parser Pos
getColumn = sourceColumn <$> getPosition

-- | Push a new alignment column on the stack.
pushTab :: (Pos, String) -> Parser ()
pushTab t = tabStack %= (t:)

-- | Open a new block corresponding to a new alignment column at the
--   current column.  The string argument is the delimiter we expect
--   to always see at this column for the duration of the block (which
--   may be empty).
open :: String -> Parser ()
open delim = do
  col <- getColumn      -- remember the current column
  _ <- string delim     -- parse the delimiter
  pushTab (col, delim)  -- push the new column on the stack
  numActive += 1        -- increment the number of active blocks
  -- trace ("open " ++ delim) $ return ()

-- | A virtual separator token.  Occurs between subsequent things
--   which both start in the same alignment column.
sep :: Parser ()
sep = do

  -- The implementation is actually simple: make sure we are expecting
  -- a separator here.  If so, reset the separator flag; if not, fail.
  s <- use isSep
  when (not s) $
    fail
      (concat
      [ "The parser required a separator but none was found.\n"
      , "Please report this as a bug: https://github.com/disco-lang/disco/issues"
      ])

  isSep .= False
  -- pos <- getPosition
  -- trace ("sep " ++ show pos) $ return ()

-- | A virutal close token, used at the end of a block.
close :: Parser ()
close = eof <|> do
  -- pos <- getPosition
  n   <- use numActive
  stk <- use tabStack
  case (length stk < n) of
    True  -> do numActive -= 1
                -- trace ("close " ++ show pos) (return ())
    False -> fail $ concat
               [ "The parser required a close but the stack is empty.\n"
               , "Please report this as a bug: https://github.com/disco-lang/disco/issues"
               ]
      -- I hope this failure mode will only occur if there is a bug in
      -- the parser, not with incorrect user syntax.

-- | Parse an indented block, with individual items parsed by the
--   given parser.  For example, something like
--
--   @
--      item1
--      item2 item2
--        item2 item2 item2
--      item3
--   @
--
--   The current column when 'block' is first invoked is taken as the
--   alignment column for the block.
block :: Parser a -> Parser [a]
block item = open "" *> (item `sepBy` sep) <* close

-- | See if a virtual separator token is required, and fail if so.
--   This is called right before parsing any other (non-virtual)
--   token, which means that when isSep is set to true, the parser is
--   /forced/ to choose a virtual separator token.
requireSep :: Parser ()
requireSep = do
  s <- use isSep
  when s $ fail "Missing separator"  -- XXX better error message.  Can this happen?

-- | See if a virtual close token is required, and fail if so.  This
--   is called right before parsing any other (non-virtual) token,
--   which means that when the indentation has decreased, the parser
--   is /forced/ to choose the required number of virtual close
--   tokens.
requireClose :: Parser ()
requireClose = do
  n   <- use numActive
  stk <- use tabStack
  -- trace ("requireClose: " ++ show n ++ " " ++ show stk) $ return ()
  when (n > length stk) $
    fail "Encountered a dedented token, but the block is not ready to close."
      -- XXX better error message

-- | Possibly require virtual separator and close tokens.
requireVirtual :: Parser ()
requireVirtual = requireSep >> requireClose

-- | Consume whitespace and deal with indentation and layout.  This is
--   where the magic happens, and needs to be invoked after every
--   token.
whitespace :: Parser ()
whitespace = do

  -- Eat up actual whitespace (including newlines) and comments
  consumeWhitespace

  -- Find out what column we ended up in, and the current stack.
  col <- getColumn
  stk <- use tabStack

  -- pos <- getPosition
  -- trace ("whitespace: " ++ show pos ++ " " ++ show stk) $ return ()

  -- Get the rightmost alignment column.
  let rightmost = case stk of { ((p,_):_) -> p; _ -> unsafePos 1 }

  -- If current column is greater than the rightmost alignment, do
  -- nothing; if <=, try to consume alignment delimiters, adjust the
  -- stack of alignments appropriately, require separators and/or
  -- block closes, etc.
  when (col <= rightmost) $ do
    let tabs = reverse stk         -- Get the alignment stack in
                                   -- reverse so we can process it
                                   -- L->R
    go tabs tabs rightmost
  where

    -- If there are no more alignments to consume, we are either at
    -- the rightmost alignment right before some non-whitespace (if
    -- the delimiter is the empty string), or right before the EOL
    go orig [] rm
      = try (eol >> consumeWhitespace >> go orig orig rm)
                                      -- Try consuming the EOL and any
                                      -- subsequent WS chars and
                                      -- starting over on the next
                                      -- line with non-whitespace
        <|>
        (do col <- getColumn          -- Otherwise, require a separator
            isSep .= (col == rm))     -- if we are in the rightmost
                                      -- alignment column (but
                                      -- otherwise we are past the
                                      -- rightmost alignment column
                                      -- and this is just a
                                      -- continuation of the previous
                                      -- line)

    go orig ts@((tabcol, delim) : moreTabs) rm = do
      col <- getColumn

      -- Check if we are at the end of the line, without consuming anything.
      atEOL <- isRight <$> observing (lookAhead eol)

      -- Check our relationship to the next alignment column.
      if
         -- If all remaining delimiters are empty, it's OK to be at
         -- EOL, even if it comes in a column before the next
         -- alignment. Just consume it and start again on the next
         -- line.
         | all (\(_,d) -> null d) ts && atEOL -> eol >> consumeWhitespace >> go orig orig rm

         -- If we're past it but the delimiter for that column was empty,
         -- no problem: just keep processing the next alignment column
         | (col > tabcol && null delim) -> go orig moreTabs rm

         -- Otherwise, there should have been a delimiter in that column
         -- but there wasn't, so fail.
         | col > tabcol                 ->
             fail $
             printf
               (concat
                  [ "There should be a '%s' in column %d to match the previous line, "
                  , "but I didn't find one.\nPlease check your indentation."
                  ]
               )
               delim
               (unPos tabcol)

         -- If we're at the next alignment column, try to consume the delimiter
         -- followed by whitespace on the same line, then continue processing columns.
         -- If we don't find the delimiter, consider that block closed.
         | col == tabcol                ->
             (do _ <- try (string delim)
                 consumeLineWhitespace
                 go orig moreTabs rm
             )
             <|>
             closeBlocks col

         -- If we're before the next alignment column, then we need to
         -- close some blocks, and possibly require a separator.
         | col < tabcol                 -> closeBlocks col

      where
        closeBlocks col = do

          -- Drop any remaining alignment columns.  Some 'close'
          -- operations will be required to get the number of
          -- active blocks equal to the size of the stack again.
          tabStack %= drop (length ts)

          -- We should require a separator if the current column is
          -- equal to the column now on top of the stack.
          stk <- use tabStack
          let acol = case stk of
                       []        -> unsafePos 1
                       ((p,_):_) -> p
          isSep .= (col == acol)

-- | Generically consume whitespace, including comments.  The first
--   argument specifies which actual whitespace characters to consume.
consumeWhitespace' :: Parser Char -> Parser ()
consumeWhitespace' sc = L.space (sc *> pure ()) lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

-- | Consume whitespace, including comments, but do not consume
--   newline characters.
consumeLineWhitespace :: Parser ()
consumeLineWhitespace = consumeWhitespace' (satisfy (\c -> isSpace c && c `notElem` "\r\n"))

-- | Consume whitespace, including comments and newlines.
consumeWhitespace :: Parser ()
consumeWhitespace = consumeWhitespace' C.spaceChar

-- | Parse a lexeme, that is, a parser followed by consuming
--   whitespace (and dealing with layout/indentation appropriately).
lexeme :: Parser a -> Parser a
lexeme p = requireVirtual >> L.lexeme whitespace p

-- | Parse a given string as a lexeme.
symbol :: String -> Parser String
symbol s = requireVirtual >> L.symbol whitespace s

-- | Like 'symbol', but discard the result.
reservedOp :: String -> Parser ()
reservedOp s = try (symbol s *> notFollowedBy (oneOf opChar))

-- | Characters that can occur in an operator symbol.
opChar :: [Char]
opChar = "!@#$%^&*~-+=|<>?/\\."

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

semi, comma, colon, dot :: Parser String
semi      = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

-- | The symbol that separates the variable binder from the body of a
--   lambda (either @↦@, @->@, or @|->@).
mapsTo :: Parser ()
mapsTo = reservedOp "↦" <|> reservedOp "->" <|> reservedOp "|->"

-- | Parse a natural number.
natural :: Parser Integer
natural = lexeme L.integer

-- | Parse a nonnegative decimal of the form @xxx.yyyy@.
decimal :: Parser Rational
decimal = lexeme (readDecimal <$> some digit <* char '.' <*> some digit)
  where
    digit = satisfy isDigit
    readDecimal a b = (read a % 1) + (read b % (10^(length b)))

-- | Parse a reserved word.
reserved :: String -> Parser ()
reserved w = lexeme $ C.string w *> notFollowedBy alphaNumChar

-- | The list of all reserved words.
reservedWords :: [String]
reservedWords =
  [ "true", "false", "True", "False", "inl", "inr", "let", "in", "is"
  , "if", "when"
  , "otherwise", "and", "or", "not", "mod", "choose", "sqrt", "lg"
  , "enumerate", "count"
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

-- | Optionally parse, succesfully returning 'Nothing' if the parse
--   fails.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = (Just <$> p) <|> pure Nothing

------------------------------------------------------------
-- Parser

-- | Parse the entire input as a module (with leading whitespace and
--   no leftovers).
wholeModule :: Parser Module
wholeModule = whitespace *> L.nonIndented consumeWhitespace parseModule <* eof

-- | Parse an entire module (a list of declarations ended by
--   semicolons).
parseModule :: Parser Module
parseModule = do
  sep
  topLevel <- parseTopLevel `sepBy` sep
  -- traceShow topLevel $ return ()
  let theMod = mkModule topLevel
  --traceShow theMod $
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
  =   DocString     <$> parseDocString
  <|> DocProperties <$> parseProperties

-- | Parse a documentation group beginning with @|||@.
parseDocString :: Parser [String]
parseDocString = do
  -- trace ("start docString") $ return ()
  open "|||"
  whitespace
  ss <- block (requireVirtual *> manyTill anyChar eol <* whitespace)
  close
  -- trace ("end docString") $ return ()
  return ss

-- | Parse a group of examples/properties beginning with @!!!@.
parseProperties :: Parser [Property]
parseProperties = do
  -- trace ("start properties") $ return ()
  open "!!!"
  whitespace
  ps <- block (bind [] <$> parseTerm)
  close
  -- trace ("end properties") $ return ()
  return ps

-- | Parse a single declaration (either a type declaration or
--   single definition clause).
parseDecl :: Parser Decl
parseDecl =
      try (DType <$> ident <*> (colon *> parseType))
  <|>      DDefn
           <$> ident
           <*> ((:[]) <$> (bind <$> many parseAtomicPattern <*> (symbol "=" *> parseTerm)))

-- | Parse the entire input as a term (with leading whitespace and
--   no leftovers).
term :: Parser Term
term = whitespace *> parseTerm <* eof

-- | Parse an atomic term.
parseAtom :: Parser Term
parseAtom = -- trace "parseAtom" $
      TUnit       <$ reserved "()"
  <|> TList       <$> brackets (parseTerm `sepBy` comma)
  <|> TBool True  <$ (reserved "true" <|> reserved "True")
  <|> TBool False <$ (reserved "false" <|> reserved "False")
  <|> TVar <$> ident
  <|> TRat <$> try decimal
  <|> TNat <$> natural
  <|> TInj <$> parseInj <*> parseAtom
  <|> try (TPair <$> (symbol "(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")"))
  <|> parseTypeOp

  <|> parens parseTerm

-- | Parse an injection, i.e. either @inl@ or @inr@.
parseInj :: Parser Side
parseInj =
  L <$ reserved "inl" <|> R <$ reserved "inr"

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
      TAbs <$> try (bind <$> ident <*> (mapsTo *> parseTerm'))
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
parseCase = do
  open "{"
  whitespace
  open ""
  c <- TCase <$> parseBranch `sepBy` sep
  close
  close
  return c

-- | Parse one branch of a case expression.
parseBranch :: Parser Branch
parseBranch = flip bind <$> parseTerm <*> parseGuards

-- | Parse the list of guards in a branch.  @otherwise@ can be used
--   interchangeably with an empty list of guards.
parseGuards :: Parser Guards
parseGuards = (GEmpty <$ reserved "otherwise") <|> (guards <$> many parseGuard)
  where
    guards = foldr (\g gs -> GCons $ rebind g gs) GEmpty

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
  <|> PList <$> brackets (parsePattern `sepBy` comma)
  <|> parens parsePattern

-- | Parse a pattern.
parsePattern :: Parser Pattern
parsePattern = makeExprParser parseAtomicPattern table <?> "pattern"
  where
    table = [ [ prefix "inl" (PInj L)
              , prefix "inr" (PInj R)
              , prefix "S"   PSucc
              ]
            , [ infixR "::" PCons ]
            ]
    prefix name fun = Prefix (reserved name >> return fun)
    infixR name fun = InfixR (reservedOp name >> return fun)

-- | Parse an expression built out of unary and binary operators.
parseExpr :: Parser Term
parseExpr = fixChains <$> (makeExprParser parseAtom table <?> "expression")
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
            , [ unary "sqrt" (TUn Sqrt)
              ]
            , [ unary "lg" (TUn Lg)
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
            , [ infixR "::" (TBin Cons)
              ]
            , [ infixR "="  (TBin Eq)
              , infixR "/=" (TBin Neq)
              , infixR "<"  (TBin Lt)
              , infixR ">"  (TBin Gt)
              , infixR "<=" (TBin Leq)
              , infixR ">=" (TBin Geq)
              , infixR "|"  (TBin Divides)
              , infixR "#"  (TBin RelPm)
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

    isChainable op = op `elem` [Eq, Neq, Lt, Gt, Leq, Geq, Divides, RelPm]

    fixChains (TUn op t) = TUn op (fixChains t)
    fixChains (TBin op t1 (TBin op' t21 t22))
      | isChainable op && isChainable op' = TChain t1 (TLink op t21 : getLinks op' t22)
    fixChains (TBin op t1 t2) = TBin op (fixChains t1) (fixChains t2)
    fixChains (TJuxt t1 t2) = TJuxt (fixChains t1) (fixChains t2)

    -- Only recurse as long as we see TUn, TBin, or TJuxt which could
    -- have been generated by the expression parser.  If we see
    -- anything else we can stop.
    fixChains e = e

    getLinks op (TBin op' t1 t2)
      | isChainable op' = TLink op t1 : getLinks op' t2
    getLinks op e = [TLink op (fixChains e)]

-- | Parse an atomic type.
parseAtomicType :: Parser Type
parseAtomicType =
      TyVoid <$ reserved "Void"
  <|> TyUnit <$ reserved "Unit"
  <|> TyBool <$ (reserved "Bool" <|> reserved "B")
  <|> TyN    <$ (reserved "Natural" <|> reserved "Nat" <|> reserved "N" <|> reserved "ℕ")
  <|> TyZ    <$ (reserved "Integer" <|> reserved "Int" <|> reserved "Z" <|> reserved "ℤ")
  <|> TyQ    <$ (reserved "Rational" <|> reserved "Q" <|> reserved "ℚ")
    -- This explicitly allows "List List N" to parse as List (List N).
    -- Since we don't have arbitrary application of higher-kinded type
    -- expressions, only application of an explicit set of
    -- right-associative single-argument type formers (e.g. List, and
    -- eventually things like Set), this can't cause any ambiguity.
  <|> TyList <$> (reserved "List" *> parseAtomicType)
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

parseTyOp :: Parser TyOp
parseTyOp =
        Enumerate <$ reserved "enumerate"
    <|> Count     <$ reserved "count"

parseTypeOp :: Parser Term
parseTypeOp = TTyOp <$> parseTyOp <*> parseAtomicType

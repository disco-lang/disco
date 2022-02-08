{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Commands
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Defining and dispatching all commands/functionality available at
-- the REPL prompt.
-----------------------------------------------------------------------------

module Disco.Interactive.Commands
  ( dispatch,
    discoCommands,
    handleLoad,
    loadFile,
    parseLine
  ) where

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (to, view, (%~), (.~), (?~),
                                                   (^.))
import           Control.Monad.Except
import           Data.Char                        (isSpace)
import           Data.Coerce
import           Data.List                        (find, isPrefixOf, sortBy)
import           Data.Map                         ((!))
import qualified Data.Map                         as M
import           Data.Typeable
import           Prelude                          as P
import           System.FilePath                  (splitFileName)

import           Text.Megaparsec                  hiding (State, runParser)
import qualified Text.Megaparsec.Char             as C
import           Unbound.Generics.LocallyNameless (Name, name2String,
                                                   string2Name)

import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Effects.State
import           Polysemy
import           Polysemy.Error                   hiding (try)
import           Polysemy.Output
import           Polysemy.Reader

import           Data.Maybe                       (maybeToList)
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context                    as Ctx
import           Disco.Desugar
import           Disco.Doc
import           Disco.Error
import           Disco.Eval
import           Disco.Extensions
import           Disco.Interpret.CESK
import           Disco.Messages
import           Disco.Module
import           Disco.Names
import           Disco.Parser                     (Parser, ident, reservedOp,
                                                   runParser, sc, symbol, term,
                                                   wholeModule, withExts)
import           Disco.Pretty                     hiding (empty, (<>))
import qualified Disco.Pretty                     as Pretty
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims               (Prim (PrimBOp, PrimUOp),
                                                   toPrim)
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Types                      (pattern TyString, toPolyType)
import           Disco.Value

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

-- | Data type to represent things typed at the Disco REPL.  Each
--   constructor has a singleton type to facilitate dispatch.
data REPLExpr :: CmdTag -> * where
  TypeCheck :: Term      -> REPLExpr 'CTypeCheck -- Typecheck a term
  Eval      :: Module    -> REPLExpr 'CEval      -- Evaluate a block
  TestProp  :: Term      -> REPLExpr 'CTestProp  -- Run a property test
  ShowDefn  :: Name Term -> REPLExpr 'CShowDefn  -- Show a variable's definition
  Parse     :: Term      -> REPLExpr 'CParse     -- Show the parsed AST
  Pretty    :: Term      -> REPLExpr 'CPretty    -- Pretty-print a term
  Print     :: Term      -> REPLExpr 'CPrint     -- Print a string
  Ann       :: Term      -> REPLExpr 'CAnn       -- Show type-annotated term
  Desugar   :: Term      -> REPLExpr 'CDesugar   -- Show a desugared term
  Compile   :: Term      -> REPLExpr 'CCompile   -- Show a compiled term
  Load      :: FilePath  -> REPLExpr 'CLoad      -- Load a file.
  Reload    ::              REPLExpr 'CReload    -- Reloads the most recently
                                                 -- loaded file.
  Doc       :: DocInput  -> REPLExpr 'CDoc       -- Show documentation.
  Nop       ::              REPLExpr 'CNop       -- No-op, e.g. if the user
                                                 -- just enters a comment
  Help      ::              REPLExpr 'CHelp      -- Show help
  Names     ::              REPLExpr 'CNames     -- Show bound names

deriving instance Show (REPLExpr c)

-- | An existential wrapper around any REPL expression.
data SomeREPLExpr where
  SomeREPL :: Typeable c => REPLExpr c -> SomeREPLExpr

------------------------------------------------------------
-- REPL command types
------------------------------------------------------------

data REPLCommandCategory
  = -- | REPL commands for everyday users
    User
  | -- | REPL commands for developers working on Disco
    Dev
  deriving (Eq, Show)

data REPLCommandType
  = -- | Things that don't start with a colon: eval and nop
    BuiltIn
  | -- | Things that start with a colon, e.g. :help, :names, :load...
    ColonCmd
  deriving (Eq, Show)

-- | Tags used at the type level to denote each REPL command.
data CmdTag
  = CTypeCheck
  | CEval
  | CShowDefn
  | CParse
  | CPretty
  | CPrint
  | CAnn
  | CDesugar
  | CCompile
  | CLoad
  | CReload
  | CDoc
  | CNop
  | CHelp
  | CNames
  | CTestProp
  deriving (Show, Eq, Typeable)

------------------------------------------------------------
-- REPL command info record
------------------------------------------------------------

-- | Data type to represent all the information about a single REPL
--   command.
data REPLCommand (c :: CmdTag) = REPLCommand
  { -- | Name of the command
    name      :: String,
    -- | Help text showing how to use the command, e.g. ":ann <term>"
    helpcmd   :: String,
    -- | Short free-form text explaining the command.
    --   We could also consider adding long help text as well.
    shortHelp :: String,
    -- | Is the command for users or devs?
    category  :: REPLCommandCategory,
    -- | Is it a built-in command or colon command?
    cmdtype   :: REPLCommandType,
    -- | The action to execute,
    -- given the input to the
    -- command.
    action    :: REPLExpr c -> (forall r. Members DiscoEffects r => Sem r ()),
    -- | Parser for the command argument(s).
    parser    :: Parser (REPLExpr c)
  }

-- | An existential wrapper around any REPL command info record.
data SomeREPLCommand where
  SomeCmd :: Typeable c => REPLCommand c -> SomeREPLCommand

------------------------------------------------------------
-- REPL command lists
------------------------------------------------------------

type REPLCommands = [SomeREPLCommand]

-- | Keep only commands of a certain type.
byCmdType :: REPLCommandType -> REPLCommands -> REPLCommands
byCmdType ty = P.filter (\(SomeCmd rc) -> cmdtype rc == ty)

-- | Given a list of REPL commands and something typed at the REPL,
--   pick the first command with a matching type-level tag and run its
--   associated action.
dispatch :: Members DiscoEffects r => REPLCommands -> SomeREPLExpr -> Sem r ()
dispatch [] _ = return ()
dispatch (SomeCmd c : cs) r@(SomeREPL e) = case gcast e of
  Just e' -> outputDiscoErrors $ action c e'
  Nothing -> dispatch cs r

-- | The list of all commands that can be used at the REPL.
--   Resolution of REPL commands searches this list /in order/, which
--   means ambiguous command prefixes (e.g. :t for :type) are resolved
--   to the first matching command.
discoCommands :: REPLCommands
discoCommands =
  [ SomeCmd annCmd,
    SomeCmd compileCmd,
    SomeCmd desugarCmd,
    SomeCmd docCmd,
    SomeCmd evalCmd,
    SomeCmd helpCmd,
    SomeCmd loadCmd,
    SomeCmd namesCmd,
    SomeCmd nopCmd,
    SomeCmd parseCmd,
    SomeCmd prettyCmd,
    SomeCmd printCmd,
    SomeCmd reloadCmd,
    SomeCmd showDefnCmd,
    SomeCmd typeCheckCmd,
    SomeCmd testPropCmd
  ]

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

builtinCommandParser :: REPLCommands -> Parser SomeREPLExpr
builtinCommandParser =
  foldr ((<|>) . (\(SomeCmd rc) -> SomeREPL <$> try (parser rc))) empty
    . byCmdType BuiltIn

-- | Parse one of the colon commands in the given list of commands.
commandParser :: REPLCommands -> Parser SomeREPLExpr
commandParser allCommands =
  (symbol ":" *> many C.lowerChar) >>= parseCommandArgs allCommands

-- | Given a list of available commands and a string seen after a
--   colon, return a parser for its arguments.
parseCommandArgs :: REPLCommands -> String -> Parser SomeREPLExpr
parseCommandArgs allCommands cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."

    parsers =
      map (\(SomeCmd rc) -> (name rc, SomeREPL <$> parser rc)) $
        byCmdType ColonCmd allCommands

-- | Parse a file name.
fileParser :: Parser FilePath
fileParser = many C.spaceChar *> many (satisfy (not . isSpace))

-- | A parser for something entered at the REPL prompt.
lineParser :: REPLCommands -> Parser SomeREPLExpr
lineParser allCommands =
  builtinCommandParser allCommands
    <|> commandParser allCommands

-- | Given a list of available REPL commands and the currently enabled
--   extensions, parse a string entered at the REPL prompt, returning
--   either a parse error message or a parsed REPL expression.
parseLine :: REPLCommands -> ExtSet -> String -> Either String SomeREPLExpr
parseLine allCommands exts s =
  case runParser (withExts exts (lineParser allCommands)) "" s of
    Left e  -> Left $ errorBundlePretty e
    Right l -> Right l

--------------------------------------------------------------------------------
-- The commands!
--------------------------------------------------------------------------------

------------------------------------------------------------
-- :ann

annCmd :: REPLCommand 'CAnn
annCmd =
  REPLCommand
    { name = "ann",
      helpcmd = ":ann",
      shortHelp = "Show type-annotated typechecked term",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleAnn $ x,
      parser = Ann <$> term
    }

handleAnn ::
  Members '[Error DiscoError, Input TopInfo, Output Message] r =>
  REPLExpr 'CAnn ->
  Sem r ()
handleAnn (Ann t) = do
  (at, _) <- typecheckTop $ inferTop t
  infoPretty at

------------------------------------------------------------
-- :compile

compileCmd :: REPLCommand 'CCompile
compileCmd =
  REPLCommand
    { name = "compile",
      helpcmd = ":compile",
      shortHelp = "Show a compiled term",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleCompile $ x,
      parser = Compile <$> term
    }

handleCompile ::
  Members '[Error DiscoError, Input TopInfo, Output Message] r =>
  REPLExpr 'CCompile ->
  Sem r ()
handleCompile (Compile t) = do
  (at, _) <- typecheckTop $ inferTop t
  infoPretty . compileTerm $ at

------------------------------------------------------------
-- :desugar

desugarCmd :: REPLCommand 'CDesugar
desugarCmd =
  REPLCommand
    { name = "desugar",
      helpcmd = ":desugar",
      shortHelp = "Show a desugared term",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleDesugar $ x,
      parser = Desugar <$> term
    }

handleDesugar ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  REPLExpr 'CDesugar ->
  Sem r ()
handleDesugar (Desugar t) = do
  (at, _) <- typecheckTop $ inferTop t
  info $ pretty' . eraseDTerm . runDesugar . desugarTerm $ at

------------------------------------------------------------
-- :doc

docCmd :: REPLCommand 'CDoc
docCmd =
  REPLCommand
    { name = "doc",
      helpcmd = ":doc <term>",
      shortHelp = "Show documentation",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleDoc $ x,
      parser = Doc <$> parseDoc
    }

-- XXX
data DocInput = DocTerm Term | DocPrim Prim | DocOther String
  deriving (Show)

parseDoc :: Parser DocInput
parseDoc =
      (DocTerm <$> try term)
  <|> (DocPrim <$> try (parseNakedOpPrim <?> "operator"))
  <|> (DocOther <$> (sc *> many (anySingleBut ' ')))

handleDoc ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  REPLExpr 'CDoc ->
  Sem r ()
handleDoc (Doc (DocTerm (TBool _))) = handleDocBool
handleDoc (Doc (DocTerm (TPrim p))) = handleDocPrim p
handleDoc (Doc (DocTerm (TVar x)))  = handleDocVar x
handleDoc (Doc (DocTerm _))         =
  err "Can't display documentation for an expression.  Try asking about a function, operator, or type name."
handleDoc (Doc (DocPrim p))         = handleDocPrim p
handleDoc (Doc (DocOther s))        = handleDocOther s

handleDocBool :: Members '[Output Message] r => Sem r ()
handleDocBool =
  info $
    "true and false (also written True and False) are the two possible values of type Boolean."
    $+$
    mkReference "bool"

handleDocVar ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  Name Term ->
  Sem r ()
handleDocVar x = do
  ctx  <- inputs @TopInfo (view (replModInfo . miTys))
  tydefs <- inputs @TopInfo (view (replModInfo . miTydefs))
  docs <- inputs @TopInfo (view (replModInfo . miDocs))

  debug $ text . show $ docs

  case (Ctx.lookupAll' x ctx, M.lookup (name2String x) tydefs) of
    ([], Nothing) ->
      -- Maybe the variable name entered by the user is actually a prim.
      case toPrim (name2String x) of
        (prim:_) -> handleDocPrim prim
        _        -> err $ "No documentation found for '" <> pretty' x <> "'."
    (binds, def) ->
      mapM_ (showDoc docs) (map Left binds ++ map Right (maybeToList def))

  where
    showDoc docMap (Left (qn, ty)) = info $
      hsep [pretty' x, ":", pretty' ty]
      $+$
      case Ctx.lookup' qn docMap of
        Just (DocString ss : _) -> vcat (text "" : map text ss ++ [text ""])
        _                       -> Pretty.empty
    showDoc docMap (Right tdBody) = info $
      pretty' (name2String x, tdBody)
      $+$
      case Ctx.lookupAll' x docMap of
        ((_, DocString ss : _) : _) -> vcat (text "" : map text ss ++ [text ""])
        _                           -> Pretty.empty

handleDocPrim ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  Prim ->
  Sem r ()
handleDocPrim prim = do
  handleTypeCheck (TypeCheck (TPrim prim))
  info $ vcat
    [ case prim of
        PrimUOp u -> describeAlts (f == Post) (f == Pre) syns
          where
            OpInfo (UOpF f _) syns _ = uopMap ! u
        PrimBOp b -> describeAlts True True (opSyns $ bopMap ! b)
        _         -> Pretty.empty
    , case prim of
        PrimUOp u -> describePrec (uPrec u)
        PrimBOp b -> describePrec (bPrec b) <> describeFixity (assoc b)
        _         -> Pretty.empty
    ]
  case (M.lookup prim primDoc, M.lookup prim primReference) of
    (Nothing, Nothing) -> return ()
    (Nothing, Just p)  -> info $ mkReference p
    (Just d, mp)  ->
      info $ "" $+$ text d $+$ "" $+$ maybe Pretty.empty (\p -> mkReference p $+$ "") mp
  where
    describePrec p = "precedence level" <+> text (show p)
    describeFixity In  = Pretty.empty
    describeFixity InL = ", left associative"
    describeFixity InR = ", right associative"
    describeAlts _ _ []            = Pretty.empty
    describeAlts _ _ [_]           = Pretty.empty
    describeAlts pre post (_:alts) = "Alternative syntax:" <+> intercalate "," (map showOp alts)
      where
        showOp op = hcat
          [ if pre then "~" else Pretty.empty
          , text op
          , if post then "~" else Pretty.empty]


mkReference :: String -> Sem r Doc
mkReference p =
  "https://disco-lang.readthedocs.io/en/latest/reference/" <> text p <> ".html"

handleDocOther ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  String ->
  Sem r ()
handleDocOther s =
  case (M.lookup s otherDoc, M.lookup s otherReference) of
    (Nothing, Nothing) -> info $ "No documentation found for '" <> text s <> "'."
    (Nothing, Just p)  -> info $ mkReference p
    (Just d, mp)  ->
      info $ text d $+$ "" $+$ maybe Pretty.empty (\p -> mkReference p $+$ "") mp

------------------------------------------------------------
-- eval

evalCmd :: REPLCommand 'CEval
evalCmd = REPLCommand
  { name      = "eval"
  , helpcmd   = "<code>"
  , shortHelp = "Evaluate a block of code"
  , category  = User
  , cmdtype   = BuiltIn
  , action    = \x -> handleEval x
  , parser    = Eval <$> wholeModule REPL
  }

handleEval
  :: Members (Error DiscoError ': State TopInfo ': Output Message ': Embed IO ': EvalEffects) r
  => REPLExpr 'CEval -> Sem r ()
handleEval (Eval m) = do
  mi <- inputToState @TopInfo $ loadParsedDiscoModule False FromCwdOrStdlib REPLModule m
  addToREPLModule mi
  forM_ (mi ^. miTerms) (mapError EvalErr . evalTerm True . fst)
  -- garbageCollect?

-- First argument = should the value be printed?
evalTerm :: Members (Error EvalError ': State TopInfo ': Output Message ': EvalEffects) r => Bool -> ATerm -> Sem r Value
evalTerm pr at = do
  env <- use @TopInfo topEnv
  v <- runInputConst env $ eval (compileTerm at)

  tydefs <- use @TopInfo (replModInfo . to allTydefs)
  when pr $ info $ runInputConst tydefs $ prettyValue' ty v

  modify @TopInfo $
    (replModInfo . miTys %~ Ctx.insert (QName (QualifiedName REPLModule) (string2Name "it")) (toPolyType ty)) .
    (topEnv %~ Ctx.insert (QName (QualifiedName REPLModule) (string2Name "it")) v)
  return v
  where
    ty = getType at

------------------------------------------------------------
-- :help

helpCmd :: REPLCommand 'CHelp
helpCmd =
  REPLCommand
    { name = "help",
      helpcmd = ":help",
      shortHelp = "Show help",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> handleHelp x,
      parser = return Help
    }

handleHelp :: Member (Output Message) r => REPLExpr 'CHelp -> Sem r ()
handleHelp Help =
  info $
    vcat
    [ "Commands available from the prompt:"
    , text ""
    , vcat (map (\(SomeCmd c) -> showCmd c) $ sortedList discoCommands)
    , text ""
    ]
  where
    maxlen = longestCmd discoCommands
    sortedList cmds =
      sortBy (\(SomeCmd x) (SomeCmd y) -> compare (name x) (name y)) $ filteredCommands cmds
    --  don't show dev-only commands by default
    filteredCommands cmds = P.filter (\(SomeCmd c) -> category c == User) cmds
    showCmd c = text (padRight (helpcmd c) maxlen ++ "  " ++ shortHelp c)
    longestCmd cmds = maximum $ map (\(SomeCmd c) -> length $ helpcmd c) cmds
    padRight s maxsize = take maxsize (s ++ repeat ' ')

------------------------------------------------------------
-- :load

loadCmd :: REPLCommand 'CLoad
loadCmd =
  REPLCommand
    { name = "load",
      helpcmd = ":load <filename>",
      shortHelp = "Load a file",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> handleLoadWrapper x,
      parser = Load <$> fileParser
    }

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper ::
  Members (Error DiscoError ': State TopInfo ': Output Message ': Embed IO ': EvalEffects) r =>
  REPLExpr 'CLoad ->
  Sem r ()
handleLoadWrapper (Load fp) = void (handleLoad fp)

handleLoad ::
  Members (Error DiscoError ': State TopInfo ': Output Message ': Embed IO ': EvalEffects) r =>
  FilePath ->
  Sem r Bool
handleLoad fp = do
  let (directory, modName) = splitFileName fp

  -- Reset top-level module map and context to empty, so we start
  -- fresh and pick up any changes to imported modules etc.
  modify @TopInfo $ topModMap .~ M.empty
  modify @TopInfo $ topEnv .~ Ctx.emptyCtx

  -- Load the module.
  m <- inputToState @TopInfo $ loadDiscoModule False (FromDir directory) modName
  setREPLModule m

  -- Now run any tests
  t <- inputToState $ runAllTests (m ^. miProps)

  -- Remember which was the most recently loaded file, so we can :reload
  modify @TopInfo (lastFile ?~ fp)
  info "Loaded."
  return t

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing, & move this
-- somewhere else.
runAllTests :: Members (Output Message ': Input TopInfo ': EvalEffects) r => Ctx ATerm [AProperty] -> Sem r Bool -- (Ctx ATerm [TestResult])
runAllTests aprops
  | Ctx.null aprops = return True
  | otherwise     = do
      info "Running tests..."
      and <$> mapM (uncurry runTests) (Ctx.assocs aprops)

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: Members (Output Message ': Input TopInfo ': EvalEffects) r => QName ATerm -> [AProperty] -> Sem r Bool
    runTests (QName _ n) props = do
      results <- inputTopEnv $ traverse (sequenceA . (id &&& runTest numSamples)) props
      let failures = P.filter (not . testIsOk . snd) results
          hdr = pretty' n <> ":"

      case P.null failures of
        True  -> info $ nest 2 $ hdr <+> "OK"
        False -> do
          tydefs <- inputs @TopInfo (view (replModInfo . to allTydefs))
          let prettyFailures =
                runInputConst tydefs . runReader initPA . runLFresh $
                  bulletList "-" $ map (uncurry prettyTestFailure) failures
          info $ nest 2 $ hdr $+$ prettyFailures
      return (P.null failures)

------------------------------------------------------------
-- :names

namesCmd :: REPLCommand 'CNames
namesCmd =
  REPLCommand
    { name = "names",
      helpcmd = ":names",
      shortHelp = "Show all names in current scope",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> inputToState . handleNames $ x,
      parser = return Names
    }

-- | Show names and types for each item in the top-level context.
handleNames ::
  Members '[Input TopInfo, LFresh, Output Message] r =>
  REPLExpr 'CNames ->
  Sem r ()
handleNames Names = do
  tyDef <- inputs @TopInfo (view (replModInfo . miTydefs))
  ctx <- inputs @TopInfo (view (replModInfo . miTys))
  info $
    vcat (map pretty' (M.assocs tyDef))
    $+$
    vcat (map showFn (Ctx.assocs ctx))
  where
    showFn (QName _ x, ty) = hsep [pretty' x, text ":", pretty' ty]

------------------------------------------------------------
-- nop

nopCmd :: REPLCommand 'CNop
nopCmd =
  REPLCommand
    { name = "nop",
      helpcmd = "",
      shortHelp = "No-op, e.g. if the user just enters a comment",
      category = Dev,
      cmdtype = BuiltIn,
      action = \x -> handleNop x,
      parser = Nop <$ (sc <* eof)
    }

handleNop :: REPLExpr 'CNop -> Sem r ()
handleNop Nop = pure ()

------------------------------------------------------------
-- :parse

parseCmd :: REPLCommand 'CParse
parseCmd =
  REPLCommand
    { name = "parse",
      helpcmd = ":parse <expr>",
      shortHelp = "Show the parsed AST",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> handleParse x,
      parser = Parse <$> term
    }

handleParse :: Member (Output Message) r => REPLExpr 'CParse -> Sem r ()
handleParse (Parse t) = info (text (show t))

------------------------------------------------------------
-- :pretty

prettyCmd :: REPLCommand 'CPretty
prettyCmd =
  REPLCommand
    { name = "pretty",
      helpcmd = ":pretty <expr>",
      shortHelp = "Pretty-print a term",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> handlePretty x,
      parser = Pretty <$> term
    }

handlePretty :: Members '[LFresh, Output Message] r => REPLExpr 'CPretty -> Sem r ()
handlePretty (Pretty t) = info $ pretty' t

------------------------------------------------------------
-- :print

printCmd :: REPLCommand 'CPrint
printCmd =
  REPLCommand
    { name = "print",
      helpcmd = ":print <expr>",
      shortHelp = "Print a string without the double quotes, interpreting special characters",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> handlePrint x,
      parser = Print <$> term
    }

handlePrint :: Members (Error DiscoError ': State TopInfo ': Output Message ': EvalEffects) r => REPLExpr 'CPrint -> Sem r ()
handlePrint (Print t) = do
  at <- inputToState . typecheckTop $ checkTop t (toPolyType TyString)
  v <- mapError EvalErr . evalTerm False $ at
  info $ text (vlist vchar v)

------------------------------------------------------------
-- :reload

reloadCmd :: REPLCommand 'CReload
reloadCmd =
  REPLCommand
    { name = "reload",
      helpcmd = ":reload",
      shortHelp = "Reloads the most recently loaded file",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> handleReload x,
      parser = return Reload
    }

handleReload ::
  Members (Error DiscoError ': State TopInfo ': Output Message ': Embed IO ': EvalEffects) r =>
  REPLExpr 'CReload ->
  Sem r ()
handleReload Reload = do
  file <- use lastFile
  case file of
    Nothing -> info "No file to reload."
    Just f  -> void (handleLoad f)

------------------------------------------------------------
-- :defn

showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd =
  REPLCommand
    { name = "defn",
      helpcmd = ":defn <var>",
      shortHelp = "Show a variable's definition",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleShowDefn $ x,
      parser = ShowDefn <$> (sc *> ident)
    }

handleShowDefn ::
  Members '[Input TopInfo, LFresh, Output Message] r =>
  REPLExpr 'CShowDefn ->
  Sem r ()
handleShowDefn (ShowDefn x) = do
  let name2s = name2String x
  defns   <- inputs @TopInfo (view (replModInfo . miTermdefs))
  tyDefns <- inputs @TopInfo (view (replModInfo . miTydefs))

  let xdefs = Ctx.lookupAll' (coerce x) defns
      mtydef = M.lookup name2s tyDefns

  info $ do
    let ds = map (pretty' . snd) xdefs ++ maybe [] (pure . pretty' . (name2s,)) mtydef
    case ds of
      [] -> text "No definition for" <+> pretty' x
      _  -> vcat ds

------------------------------------------------------------
-- :test

testPropCmd :: REPLCommand 'CTestProp
testPropCmd =
  REPLCommand
    { name = "test",
      helpcmd = ":test <property>",
      shortHelp = "Test a property using random examples",
      category = User,
      cmdtype = ColonCmd,
      action = \x -> handleTest x,
      parser = TestProp <$> term
    }

handleTest ::
  Members (Error DiscoError ': State TopInfo ': Output Message ': EvalEffects) r =>
  REPLExpr 'CTestProp ->
  Sem r ()
handleTest (TestProp t) = do
  at <- inputToState . typecheckTop $ checkProperty t
  tydefs <- use @TopInfo (replModInfo . to allTydefs)
  inputToState . inputTopEnv $ do
    r <- runTest 100 at -- XXX make configurable
    info $ runInputConst tydefs . runReader initPA $ nest 2 $ "-" <+> prettyTestResult at r

------------------------------------------------------------
-- :type

typeCheckCmd :: REPLCommand 'CTypeCheck
typeCheckCmd =
  REPLCommand
    { name = "type",
      helpcmd = ":type <term>",
      shortHelp = "Typecheck a term",
      category = Dev,
      cmdtype = ColonCmd,
      action = \x -> inputToState @TopInfo . handleTypeCheck $ x,
      parser = parseTypeCheck
    }

handleTypeCheck ::
  Members '[Error DiscoError, Input TopInfo, LFresh, Output Message] r =>
  REPLExpr 'CTypeCheck ->
  Sem r ()
handleTypeCheck (TypeCheck t) = do
  (_, sig) <- typecheckTop $ inferTop t
  info $ pretty' t <+> text ":" <+> pretty' sig

parseTypeCheck :: Parser (REPLExpr 'CTypeCheck)
parseTypeCheck =
  TypeCheck
    <$> ( (try term <?> "expression")
            <|> (parseNakedOp <?> "operator")
        )

-- In a :type or :doc command, allow naked operators, as in :type + ,
-- even though + by itself is not a syntactically valid term.
-- However, this seems like it may be a common thing for a student to
-- ask and there is no reason we can't have this as a special case.
parseNakedOp :: Parser Term
parseNakedOp = TPrim <$> parseNakedOpPrim

parseNakedOpPrim :: Parser Prim
parseNakedOpPrim = sc *> choice (map mkOpParser (concat opTable))
  where
    mkOpParser :: OpInfo -> Parser Prim
    mkOpParser (OpInfo (UOpF _ op) syns _) = choice (map ((PrimUOp op <$) . reservedOp) syns)
    mkOpParser (OpInfo (BOpF _ op) syns _) = choice (map ((PrimBOp op <$) . reservedOp) syns)

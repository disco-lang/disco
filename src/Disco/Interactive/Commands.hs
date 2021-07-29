{-# LANGUAGE KindSignatures     #-}
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
--
-----------------------------------------------------------------------------

module Disco.Interactive.Commands
  (
    dispatch,
    discoCommands,
    handleLoad,
    loadFile,
    parseLine
  ) where

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (view, (%~), (?~))
import           Control.Monad.Except
import           Data.Char                        (isSpace)
import           Data.Coerce
import           Data.List                        (find, isPrefixOf, sortBy)
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Data.Typeable
import           System.FilePath                  (splitFileName)

import           Text.Megaparsec                  hiding (State, runParser)
import qualified Text.Megaparsec.Char             as C
import           Unbound.Generics.LocallyNameless (Name, name2String,
                                                   string2Name)

import           Disco.Effects.Error              hiding (try)
import           Disco.Effects.Input
import           Disco.Effects.LFresh
import           Disco.Effects.Output
import           Disco.Effects.Store
import           Polysemy
import           Polysemy.State

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Desugar
import           Disco.Error
import           Disco.Eval
import           Disco.Extensions
import           Disco.Interpret.Core
import           Disco.Module
import           Disco.Parser                     (Parser, ident, reservedOp,
                                                   runParser, sc, symbol, term,
                                                   wholeModule, withExts)
import           Disco.Pretty                     hiding (empty)
import           Disco.Property
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims               (Prim (PrimBOp, PrimUOp))
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Types                      (toPolyType)
import           Disco.Value

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

-- | Data type to represent things typed at the Disco REPL.  Each
--   constructor has a singleton type to facilitate dispatch.
data REPLExpr :: CmdTag -> * where
  TypeCheck :: Term -> REPLExpr 'CTypeCheck        -- Typecheck a term
  Eval      :: Module -> REPLExpr 'CEval           -- Evaluate a block
  TestProp  :: Term -> REPLExpr 'CTestProp         -- Run a property test
  ShowDefn  :: Name Term -> REPLExpr 'CShowDefn    -- Show a variable's definition
  Parse     :: Term -> REPLExpr 'CParse            -- Show the parsed AST
  Pretty    :: Term -> REPLExpr 'CPretty           -- Pretty-print a term
  Ann       :: Term -> REPLExpr 'CAnn              -- Show type-annotated typechecked term
  Desugar   :: Term -> REPLExpr 'CDesugar          -- Show a desugared term
  Compile   :: Term -> REPLExpr 'CCompile          -- Show a compiled term
  RNF       :: Term -> REPLExpr 'CRNF
  Mem       :: REPLExpr 'CMem
  Load      :: FilePath -> REPLExpr 'CLoad         -- Load a file.
  Reload    :: REPLExpr 'CReload                   -- Reloads the most recently loaded file.
  Doc       :: Name Term -> REPLExpr 'CDoc         -- Show documentation.
  Nop       :: REPLExpr 'CNop                      -- No-op, e.g. if the user
                                                   -- just enters a comment
  Help      :: REPLExpr 'CHelp
  Names     :: REPLExpr 'CNames

deriving instance Show (REPLExpr c)

-- | An existential wrapper around any REPL expression.
data SomeREPLExpr where
  SomeREPL :: Typeable c => REPLExpr c -> SomeREPLExpr

------------------------------------------------------------
-- REPL command types
------------------------------------------------------------

data REPLCommandCategory =
    User    -- ^ REPL commands for everyday users
  | Dev     -- ^ REPL commands for developers working on Disco
  deriving (Eq, Show)

data REPLCommandType =
    BuiltIn   -- ^ Things that don't start with a colon: eval and nop
  | ColonCmd  -- ^ Things that start with a colon, e.g. :help, :names, :load...
  deriving (Eq, Show)

-- | Tags used at the type level to denote each REPL command.
data CmdTag = CTypeCheck | CEval | CShowDefn
  | CParse | CPretty | CAnn | CDesugar | CCompile | CRNF | CMem | CLoad
  | CReload | CDoc | CNop | CHelp | CNames | CTestProp
  deriving (Show, Eq, Typeable)

------------------------------------------------------------
-- REPL command info record
------------------------------------------------------------

-- | Data type to represent all the information about a single REPL
--   command.
data REPLCommand (c :: CmdTag) = REPLCommand
  { name      :: String    -- ^ Name of the command
  , helpcmd   :: String    -- ^ Help text showing how to use the command, e.g. ":ann <term>"
  , shortHelp :: String    -- ^ Short free-form text explaining the command.
                           --   We could also consider adding long help text as well.
  , category  :: REPLCommandCategory  -- ^ Is the command for users or devs?
  , cmdtype   :: REPLCommandType      -- ^ Is it a built-in command or colon command?
  , action    :: REPLExpr c -> (forall r. Members DiscoEffects r => Sem r ())
                                      -- ^ The action to execute,
                                      -- given the input to the
                                      -- command.
  , parser    :: Parser (REPLExpr c) -- ^ Parser for the command argument(s).
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
byCmdType ty = filter (\(SomeCmd rc) -> cmdtype rc == ty)

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
  [ SomeCmd annCmd
  , SomeCmd compileCmd
  , SomeCmd desugarCmd
  , SomeCmd docCmd
  , SomeCmd evalCmd
  , SomeCmd helpCmd
  , SomeCmd loadCmd
  , SomeCmd memCmd
  , SomeCmd namesCmd
  , SomeCmd nopCmd
  , SomeCmd parseCmd
  , SomeCmd prettyCmd
  , SomeCmd reloadCmd
  , SomeCmd rnfCmd
  , SomeCmd showDefnCmd
  , SomeCmd typeCheckCmd
  , SomeCmd testPropCmd
  ]

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

builtinCommandParser :: REPLCommands -> Parser SomeREPLExpr
builtinCommandParser
  = foldr ((<|>) . (\(SomeCmd rc) -> SomeREPL <$> try (parser rc))) empty
  . byCmdType BuiltIn

-- | Parse one of the colon commands in the given list of commands.
commandParser :: REPLCommands -> Parser SomeREPLExpr
commandParser allCommands
  = (symbol ":" *> many C.lowerChar) >>= parseCommandArgs allCommands

-- | Given a list of available commands and a string seen after a
--   colon, return a parser for its arguments.
parseCommandArgs ::  REPLCommands -> String -> Parser SomeREPLExpr
parseCommandArgs allCommands cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."

    parsers
      = map (\(SomeCmd rc) -> (name rc, SomeREPL <$> parser rc))
      $ byCmdType ColonCmd allCommands

-- | Parse a file name.
fileParser :: Parser FilePath
fileParser = many C.spaceChar *> many (satisfy (not . isSpace))

-- | A parser for something entered at the REPL prompt.
lineParser :: REPLCommands -> Parser SomeREPLExpr
lineParser allCommands
  =   builtinCommandParser allCommands
  <|> commandParser allCommands

-- | Given a list of available REPL commands and the currently enabled
--   extensions, parse a string entered at the REPL prompt, returning
--   either a parse error message or a parsed REPL expression.
parseLine :: REPLCommands -> ExtSet -> String -> Either String SomeREPLExpr
parseLine allCommands exts s =
  case runParser (withExts exts (lineParser allCommands)) "" s of
    Left  e -> Left $ errorBundlePretty e
    Right l -> Right l

--------------------------------------------------------------------------------
-- The commands!
--------------------------------------------------------------------------------

------------------------------------------------------------
-- :ann

annCmd :: REPLCommand 'CAnn
annCmd = REPLCommand
  { name = "ann"
  , helpcmd = ":ann"
  , shortHelp = "Show type-annotated typechecked term"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleAnn
  , parser = Ann <$> term
  }

handleAnn
  :: Members '[Error DiscoError, Input TopInfo, Output String] r
  => REPLExpr 'CAnn -> Sem r ()
handleAnn (Ann t) = do
  (at, _) <- typecheckDisco $ inferTop t
  outputLn (show at)

------------------------------------------------------------
-- :compile

compileCmd :: REPLCommand 'CCompile
compileCmd = REPLCommand
  { name = "compile"
  , helpcmd = ":compile"
  , shortHelp = "Show a compiled term"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleCompile
  , parser = Compile <$> term
  }

handleCompile
  :: Members '[Error DiscoError, Input TopInfo, Output String] r
  => REPLExpr 'CCompile -> Sem r ()
handleCompile (Compile t) = do
  (at, _) <- typecheckDisco $ inferTop t
  outputLn . show . compileTerm $ at

------------------------------------------------------------
-- :desugar

desugarCmd :: REPLCommand 'CDesugar
desugarCmd = REPLCommand
  { name = "desugar"
  , helpcmd = ":desugar"
  , shortHelp = "Show a desugared term"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleDesugar
  , parser = Desugar <$> term
  }

handleDesugar
  :: Members '[Error DiscoError, Input TopInfo, LFresh, Output String] r
  => REPLExpr 'CDesugar -> Sem r ()
handleDesugar (Desugar t) = do
  (at, _) <- typecheckDisco $ inferTop t
  s <- renderDoc . prettyTerm . eraseDTerm . runDesugar . desugarTerm $ at
  outputLn s

------------------------------------------------------------
-- :doc

docCmd :: REPLCommand 'CDoc
docCmd = REPLCommand
  { name = "doc"
  , helpcmd = ":doc <term>"
  , shortHelp = "Show documentation"
  , category = User
  , cmdtype = ColonCmd
  , action = handleDoc
  , parser = Doc <$> (sc *> ident)
  }

handleDoc
  :: Members '[Input TopInfo, LFresh, Output String] r
  => REPLExpr 'CDoc -> Sem r ()
handleDoc (Doc x) = do
  ctx  <- inputs @TopInfo (view topCtx)
  docs <- inputs @TopInfo (view topDocs)
  case M.lookup x ctx of
    Nothing -> outputLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      outputLn p
      case M.lookup x docs of
        Just (DocString ss : _) -> outputLn $ "\n" ++ unlines ss
        _                       -> return ()

------------------------------------------------------------
-- eval

evalCmd :: REPLCommand 'CEval
evalCmd = REPLCommand
  { name      = "eval"
  , helpcmd   = "<code>"
  , shortHelp = "Evaluate a block of code"
  , category  = User
  , cmdtype   = BuiltIn
  , action    = handleEval
  , parser    = Eval <$> wholeModule REPL
  }

handleEval
  :: Members (Error DiscoError ': State TopInfo ': Output String ': Embed IO ': EvalEffects) r
  => REPLExpr 'CEval -> Sem r ()
handleEval (Eval m) = inputToState $ do
  mi@(ModuleInfo _ _ _ _ _ tms exts) <- loadParsedDiscoModule FromCwdOrStdlib "" m
  modify @TopInfo (extSet %~ S.union exts)
  addModule REPL mi
  forM_ tms (evalTerm . fst)
  -- garbageCollect

evalTerm :: Members (State TopInfo ': Output String ': EvalEffects) r => ATerm -> Sem r Value
evalTerm at = do
  v <- inputToState . withTopEnv $ do
    cv <- mkValue c
    prettyValue ty cv
    return cv
  modify @TopInfo $
    (topCtx %~ M.insert (string2Name "it") (toPolyType ty)) .
    (topEnv %~ M.insert (string2Name "it") v)
  return v
  where
    ty = getType at
    c  = compileTerm at

------------------------------------------------------------
-- :help

helpCmd :: REPLCommand 'CHelp
helpCmd =
  REPLCommand
  { name = "help"
  , helpcmd = ":help"
  , shortHelp = "Show help"
  , category = User
  , cmdtype = ColonCmd
  , action = handleHelp
  , parser = return Help
  }

handleHelp :: Member (Output String) r => REPLExpr 'CHelp -> Sem r ()
handleHelp Help = do
  outputLn "Commands available from the prompt:\n"
  let maxlen = longestCmd discoCommands
  mapM_ (\(SomeCmd c) -> outputLn $ showCmd c maxlen) $  sortedList discoCommands
  outputLn ""
  where
    sortedList cmds =
      sortBy (\(SomeCmd x) (SomeCmd y) -> compare (name x) (name y)) $ filteredCommands cmds
    --  don't show dev-only commands by default
    filteredCommands cmds = filter (\(SomeCmd c) -> category c == User) cmds
    showCmd c maxlen = padRight (helpcmd c) maxlen ++ "  " ++ shortHelp c
    longestCmd cmds = maximum $ map (\(SomeCmd c) -> length $ helpcmd c) cmds
    padRight s maxsize = take maxsize (s ++ repeat ' ')

------------------------------------------------------------
-- :load

loadCmd :: REPLCommand 'CLoad
loadCmd = REPLCommand
  { name = "load"
  , helpcmd = ":load <filename>"
  , shortHelp = "Load a file"
  , category = User
  , cmdtype = ColonCmd
  , action = handleLoadWrapper
  , parser = Load <$> fileParser
  }

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper
  :: Members (Error DiscoError ': State TopInfo ': Output String ': Embed IO ': EvalEffects) r
  => REPLExpr 'CLoad -> Sem r ()
handleLoadWrapper (Load fp) =  void (handleLoad fp)

handleLoad
  :: Members (Error DiscoError ': State TopInfo ': Output String ': Embed IO ': EvalEffects) r
  => FilePath -> Sem r Bool
handleLoad fp = do
  let (directory, modName) = splitFileName fp
  m@(ModuleInfo _ props _ _ _ _ _) <- inputToState $ loadDiscoModule (FromDir directory) modName
  setLoadedModule m
  t <- inputToState . withTopEnv $ runAllTests props
  modify @TopInfo (lastFile ?~ fp)
  outputLn "Loaded."
  garbageCollect
  return t

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing, & move this
-- somewhere else.
runAllTests :: Members (Output String ': Input TopInfo ': EvalEffects) r => Ctx ATerm [AProperty] -> Sem r Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      outputLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: Members (Output String ': Input TopInfo ': EvalEffects) r => Name ATerm -> [AProperty] -> Sem r Bool
    runTests n props = do
      output ("  " ++ name2String n ++ ":")
      results <- traverse (sequenceA . (id &&& runTest numSamples)) props
      let failures = filter (not . testIsOk . snd) results
      case null failures of
        True  -> outputLn " OK"
        False -> do
          outputLn ""
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

------------------------------------------------------------
-- :names

namesCmd :: REPLCommand 'CNames
namesCmd = REPLCommand
  { name = "names"
  , helpcmd = ":names"
  , shortHelp = "Show all names in current scope"
  , category = User
  , cmdtype = ColonCmd
  , action = handleNames
  , parser = return Names
  }

-- | Show names and types for each item in the top-level context.
handleNames
  :: Members '[Input TopInfo, LFresh, Output String] r
  => REPLExpr 'CNames -> Sem r ()
handleNames Names = do
  ctx   <- inputs @TopInfo (view topCtx)
  tyDef <- inputs @TopInfo (view topTyDefs)
  mapM_ showTyDef $ M.toList tyDef
  mapM_ showFn $ M.toList ctx
  where
    showTyDef (nm, body) = renderDoc (prettyTyDef nm body) >>= outputLn
    showFn (x, ty) = do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      outputLn p

------------------------------------------------------------
-- nop

nopCmd :: REPLCommand 'CNop
nopCmd = REPLCommand
  { name = "nop"
  , helpcmd = ""
  , shortHelp = "No-op, e.g. if the user just enters a comment"
  , category = Dev
  , cmdtype = BuiltIn
  , action = handleNop
  , parser = Nop <$ (sc <* eof)
  }

handleNop :: REPLExpr 'CNop -> Sem r ()
handleNop Nop = pure ()

------------------------------------------------------------
-- :parse

parseCmd :: REPLCommand 'CParse
parseCmd = REPLCommand
  { name = "parse"
  , helpcmd = ":parse <expr>"
  , shortHelp = "Show the parsed AST"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleParse
  , parser = Parse <$> term
  }

handleParse :: Member (Output String) r => REPLExpr 'CParse -> Sem r ()
handleParse (Parse t) = printoutLn t

------------------------------------------------------------
-- :pretty

prettyCmd :: REPLCommand 'CPretty
prettyCmd = REPLCommand
  { name = "pretty"
  , helpcmd = ":pretty <expr>"
  , shortHelp = "Pretty-print a term"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handlePretty
  , parser = Pretty <$> term
  }

handlePretty :: Members '[LFresh, Output String] r => REPLExpr 'CPretty -> Sem r ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= outputLn

------------------------------------------------------------
-- :reload

reloadCmd :: REPLCommand 'CReload
reloadCmd = REPLCommand
  { name = "reload"
  , helpcmd = ":reload"
  , shortHelp = "Reloads the most recently loaded file"
  , category = User
  , cmdtype = ColonCmd
  , action = handleReload
  , parser = return Reload
  }

handleReload
  :: Members (Error DiscoError ': State TopInfo ': Output String ': Embed IO ': EvalEffects) r
  => REPLExpr 'CReload -> Sem r ()
handleReload Reload = do
  file <- gets (view lastFile)
  case file of
    Nothing -> outputLn "No file to reload."
    Just f  -> void (handleLoad f)

------------------------------------------------------------
-- :defn

showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd = REPLCommand
  { name = "defn"
  , helpcmd = ":defn <var>"
  , shortHelp = "Show a variable's definition"
  , category = User
  , cmdtype = ColonCmd
  , action = handleShowDefn
  , parser = ShowDefn <$> (sc *> ident)
  }

handleShowDefn
  :: Members '[Input TopInfo, LFresh, Output String] r
  => REPLExpr 'CShowDefn -> Sem r ()
handleShowDefn (ShowDefn x) = do
  let name2s = name2String x
  defns   <- inputs @TopInfo (view topDefs)
  tyDefns <- inputs @TopInfo (view topTyDefs)
  s <- case M.lookup (coerce x) defns of
         Just d  -> renderDoc $ prettyDefn d
         Nothing -> case M.lookup name2s tyDefns of
           Just t  -> renderDoc $ prettyTyDef name2s t
           Nothing -> return $ "No definition for " ++ show x
  outputLn s

------------------------------------------------------------
-- :test

testPropCmd :: REPLCommand 'CTestProp
testPropCmd = REPLCommand
  { name = "test"
  , helpcmd = ":test <property>"
  , shortHelp = "Test a property using random examples"
  , category = User
  , cmdtype = ColonCmd
  , action = handleTest
  , parser = TestProp <$> term
  }

handleTest
  :: Members (Error DiscoError ': State TopInfo ': Output String ': EvalEffects) r
  => REPLExpr 'CTestProp -> Sem r ()
handleTest (TestProp t) = do
  at <- inputToState . typecheckDisco $ checkProperty t
  inputToState . withTopEnv $ do
    r <- runTest 100 at   -- XXX make configurable
    prettyTestResult at r
  garbageCollect

------------------------------------------------------------
-- :type

typeCheckCmd :: REPLCommand 'CTypeCheck
typeCheckCmd = REPLCommand
  { name = "type"
  , helpcmd = ":type <term>"
  , shortHelp = "Typecheck a term"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleTypeCheck
  , parser = parseTypeCheck
  }

handleTypeCheck
  :: Members '[Error DiscoError, Input TopInfo, LFresh, Output String] r
  => REPLExpr 'CTypeCheck -> Sem r ()
handleTypeCheck (TypeCheck t) = do
  (_, sig) <- typecheckDisco $ inferTop t
  s <- renderDoc $ prettyTerm t <+> text ":" <+> prettyPolyTy sig
  outputLn s

parseTypeCheck :: Parser (REPLExpr 'CTypeCheck)
parseTypeCheck = TypeCheck <$>
  (   (try term <?> "expression")
  <|> (parseNakedOp <?> "operator")
  )

-- In a :type command, allow naked operators, as in :type + , even
-- though + by itself is not a syntactically valid term.  However,
-- this seems like it may be a common thing for a student to ask and
-- there is no reason we can't have this as a special case.
parseNakedOp :: Parser Term
parseNakedOp = sc *> choice (map mkOpParser (concat opTable))
  where
    mkOpParser :: OpInfo -> Parser Term
    mkOpParser (OpInfo (UOpF _ op) syns _) = choice (map ((TPrim (PrimUOp op) <$) . reservedOp) syns)
    mkOpParser (OpInfo (BOpF _ op) syns _) = choice (map ((TPrim (PrimBOp op) <$) . reservedOp) syns)

------------------------------------------------------------
-- :rnf

rnfCmd :: REPLCommand 'CRNF
rnfCmd = REPLCommand
  { name = "rnf"
  , helpcmd = "rnf <expr>"
  , shortHelp = "Reduce an expression to RNF"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleRNF
  , parser = RNF <$> term
  }

handleRNF
  :: Members (Error DiscoError ': State TopInfo ': Output String ': EvalEffects) r
  => REPLExpr 'CRNF -> Sem r ()
handleRNF (RNF t) = do
  (at, _) <- inputToState . typecheckDisco $ inferTop t
  let c  = compileTerm at
  inputToState . withTopEnv $ do
    cv <- mkValue c >>= rnfV
    printoutLn cv

------------------------------------------------------------
-- :mem

memCmd :: REPLCommand 'CMem
memCmd = REPLCommand
  { name = "mem"
  , helpcmd = "mem"
  , shortHelp = "Show the contents of memory"
  , category = Dev
  , cmdtype = ColonCmd
  , action = handleMem
  , parser = return Mem
  }

handleMem :: Members '[Store Cell, Output String] r => REPLExpr 'CMem -> Sem r ()
handleMem Mem = showMemory

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Commands
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-----------------------------------------------------------------------------

module Disco.Interactive.Commands
  (
    dispatch,
    discoCommands,
    handleLoad,
    loadFile
  ) where

import           Control.Arrow                    ((&&&))
import           Control.Exception                (IOException, handle)
import           Control.Lens                     (view, (%~), (.~))
import           Control.Monad.Except
import           Data.Coerce
import           Data.List                        (sortBy)
import qualified Data.Map                         as M
import           Data.Typeable
import           System.FilePath                  (splitFileName)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Extensions
import           Disco.Interactive.Parser
import           Disco.Interactive.Types
import           Disco.Interpret.Core
import           Disco.Module
import           Disco.Parser                     (Parser, ident, parseExtName,
                                                   parseImport, reserved,
                                                   reservedOp, sc, term)
import           Disco.Pretty
import           Disco.Property
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims               (Prim (PrimBOp, PrimUOp))
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Value

import           Text.Megaparsec                  hiding (State, runParser)
import           Unbound.Generics.LocallyNameless (Name, bind, name2String,
                                                   string2Name)

import           Disco.Effects.Counter
import           Disco.Effects.LFresh
import           Disco.Effects.Output
import           Polysemy
import           Polysemy.Error                   (Error, mapError)
import           Polysemy.Input
import           Polysemy.Reader
import           Polysemy.State

dispatch :: Members DiscoEffects r => [SomeREPLCommand] -> SomeREPLExpr -> Sem r ()
dispatch [] _ = return ()
dispatch (SomeCmd c : cs) r@(SomeREPL e) = case gcast e of
  Just e' -> outputErrors @IErr $ action c e'
  Nothing -> dispatch cs r

-- Resolution of REPL commands searches this list _in order_, which means
-- ambiguous command prefixes (e.g. :t for :type) are resolved to the first
-- matching command.
discoCommands :: [SomeREPLCommand]
discoCommands =
  [ SomeCmd annCmd
  , SomeCmd compileCmd
  , SomeCmd desugarCmd
  , SomeCmd docCmd
  , SomeCmd evalCmd
  , SomeCmd helpCmd
  , SomeCmd importCmd
  , SomeCmd letCmd
  , SomeCmd loadCmd
  , SomeCmd namesCmd
  , SomeCmd nopCmd
  , SomeCmd parseCmd
  , SomeCmd prettyCmd
  , SomeCmd reloadCmd
  , SomeCmd showDefnCmd
  , SomeCmd typeCheckCmd
  , SomeCmd testPropCmd
  , SomeCmd usingCmd
  ]

------------------------------------------
-- Commands
------------------------------------------

annCmd :: REPLCommand 'CAnn
annCmd = REPLCommand
  { name = "ann"
  , helpcmd = ":ann"
  , shortHelp = "Show type-annotated typechecked term"
  , category = Dev
  , cmdtype = ShellCmd
  , action = handleAnn
  , parser = Ann <$> term
  }

handleAnn :: Members '[Error IErr, Input TopInfo, Output String] r => REPLExpr 'CAnn -> Sem r ()
handleAnn (Ann t) = do
  (at, _) <- typecheckDisco $ inferTop t
  outputLn (show at)

compileCmd :: REPLCommand 'CCompile
compileCmd = REPLCommand
  { name = "compile"
  , helpcmd = ":compile"
  , shortHelp = "Show a compiled term"
  , category = Dev
  , cmdtype = ShellCmd
  , action = handleCompile
  , parser = Compile <$> term
  }

handleCompile :: Members '[Error IErr, Input TopInfo, Output String] r => REPLExpr 'CCompile -> Sem r ()
handleCompile (Compile t) = do
  (at, _) <- typecheckDisco $ inferTop t
  outputLn . show . compileTerm $ at

desugarCmd :: REPLCommand 'CDesugar
desugarCmd = REPLCommand
  { name = "desugar"
  , helpcmd = ":desugar"
  , shortHelp = "Show a desugared term"
  , category = Dev
  , cmdtype = ShellCmd
  , action = handleDesugar
  , parser = Desugar <$> term
  }

handleDesugar :: Members '[Error IErr, Input TopInfo, LFresh, Output String] r => REPLExpr 'CDesugar -> Sem r ()
handleDesugar (Desugar t) = do
  (at, _) <- typecheckDisco $ inferTop t
  s <- renderDoc . prettyTerm . eraseDTerm . runDesugar . desugarTerm $ at
  outputLn s

docCmd :: REPLCommand 'CDoc
docCmd = REPLCommand
  { name = "doc"
  , helpcmd = ":doc <term>"
  , shortHelp = "Show documentation"
  , category = User
  , cmdtype = ShellCmd
  , action = handleDoc
  , parser = Doc <$> (sc *> ident)
  }

handleDoc :: Members '[Input TopInfo, LFresh, Output String] r => REPLExpr 'CDoc -> Sem r ()
handleDoc (Doc x) = do
  ctx  <- inputs @TopInfo (view topCtx)
  docs <- inputs @TopInfo (view topDocs)
  case M.lookup x ctx of
    Nothing -> outputLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      outputLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> outputLn $ "\n" ++ unlines ss
        _                       -> return ()

evalCmd :: REPLCommand 'CEval
evalCmd = REPLCommand
  { name = "eval"
  , helpcmd = "<term>"
  , shortHelp = "Evaluate a term"
  , category = User
  , cmdtype = BuiltIn
  , action = handleEval
  , parser = Eval <$> term
  }

handleEval :: Members (State TopInfo ': Output String ': EvalEffects) r => REPLExpr 'CEval -> Sem r ()
handleEval (Eval t) = do
  (at, polyTy) <- inputToState . typecheckDisco $ inferTop t
  let ty = getType at
      c  = compileTerm at
  v <- inputToState . withTopEnv $ do
    cv <- mkValue c
    prettyValue ty cv
    return cv
  modify @TopInfo $
    (topCtx %~ M.insert (string2Name "it") polyTy) .
    (topEnv %~ M.insert (string2Name "it") v)
  garbageCollect

helpCmd :: REPLCommand 'CHelp
helpCmd =
  REPLCommand
  { name = "help"
  , helpcmd = ":help"
  , shortHelp = "Show help"
  , category = User
  , cmdtype = ShellCmd
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

importCmd :: REPLCommand 'CImport
importCmd = REPLCommand
  { name = "import"
  , helpcmd = ":import <module>"
  , shortHelp = "Import a library module"
  , category = User
  , cmdtype = BuiltIn
  , action = handleImport
  , parser = Import <$> parseImport
  }

handleImport
  :: Members '[Error IErr, State TopInfo, Reader Env, Counter, State Memory, Output String, Embed IO] r
  => REPLExpr 'CImport -> Sem r ()
handleImport (Import modName) = do
  mi <- loadDiscoModule FromCwdOrStdlib modName
  addModule mi


letCmd :: REPLCommand 'CLet
letCmd = REPLCommand
  { name = "let"
  , helpcmd = "<variable> = <expression>"
  , shortHelp = "Temporarily define a variable (until the next :load)"
  , category = User
  , cmdtype = BuiltIn
  , action = handleLet
  , parser = letParser
  }

handleLet :: Members '[Error IErr, State TopInfo, State Memory, Counter, Reader Env, Output String] r => REPLExpr 'CLet -> Sem r ()
handleLet (Let x t) = do
  (at, sig) <- inputToState . typecheckDisco $ inferTop t
  let c = compileTerm at
  thnk <- inputToState . withTopEnv $ mkValue c
  modify @TopInfo $
    (topCtx  %~ M.insert x sig) .
      -- XXX ability to define more complex things at REPL prompt, with patterns etc.
    (topDefs %~ M.insert (coerce x) (Defn (coerce x) [] (getType at) [bind [] at])) .
    (topEnv  %~ M.insert (coerce x) thnk)

loadCmd :: REPLCommand 'CLoad
loadCmd = REPLCommand
  { name = "load"
  , helpcmd = ":load <filename>"
  , shortHelp = "Load a file"
  , category = User
  , cmdtype = ShellCmd
  , action = handleLoadWrapper
  , parser = Load <$> fileParser
  }

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper :: Members (State TopInfo ': Output String ': Embed IO ': EvalEffects) r => REPLExpr 'CLoad -> Sem r ()
handleLoadWrapper (Load fp) =  void (handleLoad fp)

handleLoad :: Members (State TopInfo ': Output String ': Embed IO ': EvalEffects) r => FilePath -> Sem r Bool
handleLoad fp = do
  let (directory, modName) = splitFileName fp
  m@(ModuleInfo _ props _ _ _) <- loadDiscoModule (FromDir directory) modName
  setLoadedModule m
  t <- inputToState . withTopEnv $ runAllTests props
  modify @TopInfo (lastFile .~ Just fp)
  outputLn $ "Loaded."
  garbageCollect
  return t


namesCmd :: REPLCommand 'CNames
namesCmd = REPLCommand
  { name = "names"
  , helpcmd = ":names"
  , shortHelp = "Show all names in current scope"
  , category = User
  , cmdtype = ShellCmd
  , action = handleNames
  , parser = return Names
  }

-- | Show names and types for each item in the top-level context.
handleNames :: Members '[Input TopInfo, LFresh, Output String] r => REPLExpr 'CNames -> Sem r ()
handleNames Names = do
  ctx   <- inputs @TopInfo (view topCtx)
  tyDef <- inputs @TopInfo (view topTyDefs)
  mapM_ showTyDef $ M.toList tyDef
  mapM_ showFn $ M.toList ctx
  where
    showTyDef (nm, body) = renderDoc (prettyTyDef nm body) >>= outputLn
    showFn (x, ty) = do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      outputLn $ p


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


parseCmd :: REPLCommand 'CParse
parseCmd = REPLCommand
  { name = "parse"
  , helpcmd = ":parse <expr>"
  , shortHelp = "Show the parsed AST"
  , category = Dev
  , cmdtype = ShellCmd
  , action = handleParse
  , parser = Parse <$> term
  }

handleParse :: Member (Output String) r => REPLExpr 'CParse -> Sem r ()
handleParse (Parse t) = printout t


prettyCmd :: REPLCommand 'CPretty
prettyCmd = REPLCommand
  { name = "pretty"
  , helpcmd = ":pretty <expr>"
  , shortHelp = "Pretty-print a term"
  , category = Dev
  , cmdtype = ShellCmd
  , action = handlePretty
  , parser = Pretty <$> term
  }

handlePretty :: Members '[LFresh, Output String] r => REPLExpr 'CPretty -> Sem r ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= outputLn


reloadCmd :: REPLCommand 'CReload
reloadCmd = REPLCommand
  { name = "reload"
  , helpcmd = ":reload"
  , shortHelp = "Reloads the most recently loaded file"
  , category = User
  , cmdtype = ShellCmd
  , action = handleReload
  , parser = return Reload
  }

handleReload :: Members (State TopInfo ': Output String ': Embed IO ': EvalEffects) r => REPLExpr 'CReload -> Sem r ()
handleReload Reload = do
  file <- gets (view lastFile)
  case file of
    Nothing -> outputLn "No file to reload."
    Just f  -> void (handleLoad f)

showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd = REPLCommand
  { name = "defn"
  , helpcmd = ":defn <var>"
  , shortHelp = "Show a variable's definition"
  , category = User
  , cmdtype = ShellCmd
  , action = handleShowDefn
  , parser = ShowDefn <$> (sc *> ident)
  }

handleShowDefn :: Members '[Input TopInfo, LFresh, Output String] r => REPLExpr 'CShowDefn -> Sem r ()
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

testPropCmd :: REPLCommand 'CTestProp
testPropCmd = REPLCommand
  { name = "test"
  , helpcmd = ":test <property>"
  , shortHelp = "Test a property using random examples"
  , category = User
  , cmdtype = ShellCmd
  , action = handleTest
  , parser = TestProp <$> term
  }

handleTest :: Members (State TopInfo ': Output String ': EvalEffects) r => REPLExpr 'CTestProp -> Sem r ()
handleTest (TestProp t) = do
  at <- inputToState . typecheckDisco $ checkProperty t
  inputToState . withTopEnv $ do
    r <- runTest 100 at   -- XXX make configurable
    prettyTestResult at r
  garbageCollect


typeCheckCmd :: REPLCommand 'CTypeCheck
typeCheckCmd =
    REPLCommand {
        name = "type",
        helpcmd = ":type <term>",
        shortHelp = "Typecheck a term",
        category = Dev,
        cmdtype = ShellCmd,
        action = handleTypeCheck,
        parser = parseTypeCheck
        }

handleTypeCheck :: Members '[Error IErr, Input TopInfo, LFresh, Output String] r => REPLExpr 'CTypeCheck -> Sem r ()
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

usingCmd :: REPLCommand 'CUsing
usingCmd = REPLCommand
  { name = "using"
  , helpcmd = "using <extension>"
  , shortHelp = "Enable an extension"
  , category = User
  , cmdtype = BuiltIn
  , action = handleUsing
  , parser = Using <$> (reserved "using" *> parseExtName)
  }

handleUsing :: Member (State TopInfo) r => REPLExpr 'CUsing -> Sem r ()
handleUsing (Using e) = modify @TopInfo $ (extSet %~ addExtension e)

-- ------------------------------------------
-- --- Util functions
-- ------------------------------------------

fileNotFound :: Member (Output String) r => FilePath -> IOException -> Sem r ()
fileNotFound file _ = outputLn $ "File not found: " ++ file

loadFile :: Members '[Output String, Embed IO] r => FilePath -> Sem r (Maybe String)
loadFile file = do
  res <- liftIO $ handle (return . Left) (Right <$> readFile file)
  case res of
    Left e  -> fileNotFound file e >> return Nothing
    Right s -> return (Just s)

addModule :: Members '[State TopInfo, Reader Env, Counter, State Memory, Error IErr] r => ModuleInfo -> Sem r ()
addModule mi = do
  curMI <- gets @TopInfo (view topModInfo)
  mi' <- mapError TypeCheckErr $ combineModuleInfo [curMI, mi]
  setLoadedModule mi'

-- | Add information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule :: Members '[State TopInfo, Reader Env, Counter, State Memory] r => ModuleInfo -> Sem r ()
setLoadedModule mi = do
  modify @TopInfo $ topModInfo .~ mi
  populateCurrentModuleInfo

populateCurrentModuleInfo :: Members '[State TopInfo, Reader Env, Counter, State Memory] r => Sem r ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds <- gets @TopInfo (view topModInfo)
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  modify @TopInfo $
    (topDocs   .~ docs) .
    (topCtx    .~ tys)  .
    (topTyDefs .~ tyds) .
    (topDefs   .~ tmds)
  loadDefs cdefns
  return ()

-- XXX redo with message framework, with proper support for indentation etc.
-- XXX move it to Pretty or Property or something
prettyTestFailure :: Members (Output String ': Input TopInfo ': EvalEffects) r => AProperty -> TestResult -> Sem r ()
prettyTestFailure _    (TestResult True _ _)    = return ()
prettyTestFailure prop (TestResult False r env) = do
  prettyFailureReason prop r
  prettyTestEnv "    Counterexample:" env

prettyTestResult :: Members (Output String ': Input TopInfo ': EvalEffects) r => AProperty -> TestResult -> Sem r ()
prettyTestResult prop r | not (testIsOk r) = prettyTestFailure prop r
prettyTestResult prop (TestResult _ r _)   = do
    dp <- renderDoc $ prettyProperty (eraseProperty prop)
    output       "  - Test passed: " >> outputLn dp
    prettySuccessReason r

prettySuccessReason :: Members (Output String ': Input TopInfo ': EvalEffects) r => TestReason -> Sem r ()
prettySuccessReason (TestFound (TestResult _ _ vs)) = do
  prettyTestEnv "    Found example:" vs
prettySuccessReason (TestNotFound Exhaustive) = do
  outputLn     "    No counterexamples exist."
prettySuccessReason (TestNotFound (Randomized n m)) = do
  output       "    Checked "
  output (show (n + m))
  outputLn " possibilities without finding a counterexample."
prettySuccessReason _ = return ()

prettyFailureReason :: Members (Output String ': Input TopInfo ': EvalEffects) r => AProperty -> TestReason -> Sem r ()
prettyFailureReason prop TestBool = do
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  output     "  - Test is false: " >> outputLn dp
prettyFailureReason prop (TestEqual ty v1 v2) = do
  output     "  - Test result mismatch for: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  outputLn dp
  output     "    - Left side:  " >> prettyValue ty v2
  output     "    - Right side: " >> prettyValue ty v1
prettyFailureReason prop (TestRuntimeError e) = do
  output     "  - Test failed: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  outputLn dp
  output     "    " >> printout e
prettyFailureReason prop (TestFound (TestResult _ r _)) = do
  prettyFailureReason prop r
prettyFailureReason prop (TestNotFound Exhaustive) = do
  output     "  - No example exists: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  outputLn dp
  outputLn   "    All possible values were checked."
prettyFailureReason prop (TestNotFound (Randomized n m)) = do
  output     "  - No example was found: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  outputLn dp
  output     "    Checked " >> output (show (n + m)) >> outputLn " possibilities."

prettyTestEnv :: Members (Output String ': Input TopInfo ': EvalEffects) r => String -> TestEnv -> Sem r ()
prettyTestEnv _ (TestEnv []) = return ()
prettyTestEnv s (TestEnv vs) = do
  outputLn s
  mapM_ prettyBind vs
  where
    maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
    prettyBind (x, ty, v) = do
      output "      "
      output x
      output (replicate (maxNameLen - length x) ' ')
      output " = "
      prettyValue ty v

runTest :: Members EvalEffects r => Int -> AProperty -> Sem r TestResult
runTest n p = testProperty (Randomized n' n') =<< mkValue (compileProperty p)
  where
    n' = fromIntegral (n `div` 2)

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: Members (Output String ': Input TopInfo ': EvalEffects) r => Ctx ATerm [AProperty] -> Sem r Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      outputLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?

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

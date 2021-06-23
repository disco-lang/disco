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

import           Capability.State
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
import           Disco.Capability
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
import           Disco.Typecheck.Monad
import           Disco.Types
import           Disco.Util
import           Disco.Value
import           Text.Megaparsec                  hiding (runParser)
import           Unbound.Generics.LocallyNameless

dispatch :: [SomeREPLCommand] -> SomeREPLExpr -> Disco ()
dispatch [] _ = return ()
dispatch (SomeCmd c : cs) r@(SomeREPL e) = case gcast e of
  Just e' -> action c e'
  Nothing -> dispatch cs r

-- Resolution of REPL commands searches this list _in order_, which means
-- ambiguous command prefixes (e.g. :t for :type) are resolved to the first
-- matching command.
discoCommands :: [SomeREPLCommand]
discoCommands =
  [
    SomeCmd annCmd,
    SomeCmd compileCmd,
    SomeCmd desugarCmd,
    SomeCmd docCmd,
    SomeCmd evalCmd,
    SomeCmd helpCmd,
    SomeCmd importCmd,
    SomeCmd letCmd,
    SomeCmd loadCmd,
    SomeCmd namesCmd,
    SomeCmd nopCmd,
    SomeCmd parseCmd,
    SomeCmd prettyCmd,
    SomeCmd reloadCmd,
    SomeCmd showDefnCmd,
    SomeCmd typeCheckCmd,
    SomeCmd testPropCmd,
    SomeCmd usingCmd
  ]

------------------------------------------
-- Commands
------------------------------------------

annCmd :: REPLCommand 'CAnn
annCmd =
    REPLCommand {
      name = "ann",
      helpcmd = ":ann",
      shortHelp = "Show type-annotated typechecked term",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleAnn,
      parser = Ann <$> term
    }

handleAnn :: Has '[St "top", MonadIO] m => REPLExpr 'CAnn -> m ()
handleAnn (Ann t) = do
    ctx   <- gets @"top" (view topCtx)
    tymap <- gets @"top" (view topTyDefs)
    s <- case evalTCM $ extends @"tyctx" ctx $ withTyDefns tymap $ inferTop t of
        Left  e       -> return . show $ e
        Right (at, _) -> return . show $ at
    iputStrLn s

compileCmd :: REPLCommand 'CCompile
compileCmd =
    REPLCommand {
      name = "compile",
      helpcmd = ":compile",
      shortHelp = "Show a compiled term",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleCompile,
      parser = Compile <$> term
    }

handleCompile :: Has '[St "top", MonadIO] m => REPLExpr 'CCompile -> m ()
handleCompile (Compile t) = do
  ctx <- gets @"top" (view topCtx)
  s <- case evalTCM (extends @"tyctx" ctx $ inferTop t) of
        Left e       -> return . show $ e
        Right (at,_) -> return . show . compileTerm $ at
  iputStrLn s


desugarCmd :: REPLCommand 'CDesugar
desugarCmd =
    REPLCommand {
      name = "desugar",
      helpcmd = ":desugar",
      shortHelp = "Show a desugared term",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleDesugar,
      parser = Desugar <$> term
    }

handleDesugar :: Has '[St "top", MonadIO, LFresh] m => REPLExpr 'CDesugar -> m ()
handleDesugar (Desugar t) = do
  ctx <- gets @"top" (view topCtx)
  s <- case evalTCM (extends @"tyctx" ctx $ inferTop t) of
        Left e       -> return.show $ e
        Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at
  iputStrLn s

docCmd :: REPLCommand 'CDoc
docCmd =
    REPLCommand {
      name = "doc",
      helpcmd = ":doc <term>",
      shortHelp = "Show documentation",
      category = User,
      cmdtype = ShellCmd,
      action = handleDoc,
      parser = Doc <$> (sc *> ident)
    }

handleDoc :: Has '[St "top", MonadIO, LFresh] m => REPLExpr 'CDoc -> m ()
handleDoc (Doc x) = do
  ctx  <- gets @"top" (view topCtx)
  docs <- gets @"top" (view topDocs)
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _                       -> return ()

evalCmd :: REPLCommand 'CEval
evalCmd =
    REPLCommand {
      name = "eval",
      helpcmd = "<term>",
      shortHelp = "Evaluate a term",
      category = User,
      cmdtype = BuiltIn,
      action = handleEval,
      parser = Eval <$> term
    }

handleEval :: (Has '[St "top"] m, MonadDisco m) => REPLExpr 'CEval -> m ()
handleEval (Eval t) = do
  ctx   <- gets @"top" (view topCtx)
  tymap <- gets @"top" (view topTyDefs)
  case evalTCM (extends @"tyctx" ctx $ withTyDefns tymap $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = compileTerm at
      in do
        v <- withTopEnv $ do
          cv <- mkValue c
          prettyValue ty cv
          return cv
        modify @"top" $
          (topCtx %~ M.insert (string2Name "it") (toPolyType ty)) .
          (topEnv %~ M.insert (string2Name "it") v)
        garbageCollect

helpCmd :: REPLCommand 'CHelp
helpCmd =
    REPLCommand {
      name = "help",
      helpcmd = ":help",
      shortHelp = "Show help",
      category = User,
      cmdtype = ShellCmd,
      action = handleHelp,
      parser = return Help
    }

handleHelp :: MonadIO m => REPLExpr 'CHelp -> m ()
handleHelp Help = do
  iputStrLn "Commands available from the prompt:\n"
  let maxlen = longestCmd discoCommands
  mapM_ (\(SomeCmd c) -> iputStrLn $ showCmd c maxlen) $  sortedList discoCommands
  iputStrLn ""
  where
    sortedList cmds =
      sortBy (\(SomeCmd x) (SomeCmd y) -> compare (name x) (name y)) $ filteredCommands cmds
    --  don't show dev-only commands by default
    filteredCommands cmds = filter (\(SomeCmd c) -> category c == User) cmds
    showCmd c maxlen = padRight (helpcmd c) maxlen ++ "  " ++ shortHelp c
    longestCmd cmds = maximum $ map (\(SomeCmd c) -> length $ helpcmd c) cmds
    padRight s maxsize = take maxsize (s ++ repeat ' ')

importCmd :: REPLCommand 'CImport
importCmd =
    REPLCommand {
      name = "import",
      helpcmd = ":import <module>",
      shortHelp = "Import a library module",
      category = User,
      cmdtype = BuiltIn,
      action = handleImport,
      parser = Import <$> parseImport
    }

handleImport
  :: Has '[St "top", Rd "env", Sc "nextloc", St "mem", Th "err", Ct "err", MonadIO] m
  => REPLExpr 'CImport -> m ()
handleImport (Import modName) = catchAndPrintErrors () $ do
  mi <- loadDiscoModule FromCwdOrStdlib modName
  addModule mi


letCmd :: REPLCommand 'CLet
letCmd =
    REPLCommand {
      name = "let",
      helpcmd = "<variable> = <expression>",
      shortHelp = "Temporarily define a variable (until the next :load)",
      category = User,
      cmdtype = BuiltIn,
      action = handleLet,
      parser = letParser
    }

handleLet :: Has '[St "top", St "mem", Sc "nextloc", Rd "env", MonadIO] m => REPLExpr 'CLet -> m ()
handleLet (Let x t) = do
  ctx   <- gets @"top" (view topCtx)
  tymap <- gets @"top" (view topTyDefs)
  let mat = evalTCM (extends @"tyctx" ctx $ withTyDefns tymap $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, sig) -> do
      let c = compileTerm at
      thnk <- withTopEnv (mkValue c)
      modify @"top" $
        (topCtx  %~ M.insert x sig) .
          -- XXX ability to define more complex things at REPL prompt, with patterns etc.
        (topDefs %~ M.insert (coerce x) (Defn (coerce x) [] (getType at) [bind [] at])) .
        (topEnv  %~ M.insert (coerce x) thnk)

loadCmd :: REPLCommand 'CLoad
loadCmd =
    REPLCommand {
      name = "load",
      helpcmd = ":load <filename>",
      shortHelp = "Load a file",
      category = User,
      cmdtype = ShellCmd,
      action = handleLoadWrapper,
      parser = Load <$> fileParser
    }

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper :: (Has '[St "top", St "lastfile"] m, MonadDisco m) => REPLExpr 'CLoad -> m ()
handleLoadWrapper (Load fp) =  void (handleLoad fp)

handleLoad :: (Has '[St "top", St "lastfile"] m, MonadDisco m) => FilePath -> m Bool
handleLoad fp = catchAndPrintErrors False $ do
  let (directory, modName) = splitFileName fp
  m@(ModuleInfo _ props _ _ _) <- loadDiscoModule (FromDir directory) modName
  setLoadedModule m
  t <- withTopEnv $ runAllTests props
  put @"lastfile" $ Just fp
  io . putStrLn $ "Loaded."
  garbageCollect
  return t


namesCmd :: REPLCommand 'CNames
namesCmd =
    REPLCommand {
      name = "names",
      helpcmd = ":names",
      shortHelp = "Show all names in current scope",
      category = User,
      cmdtype = ShellCmd,
      action = handleNames,
      parser = return Names
    }

-- | Show names and types for each item in the top-level context.
handleNames :: Has '[St "top", MonadIO, LFresh] m => REPLExpr 'CNames -> m ()
handleNames Names = do
  ctx   <- gets @"top" (view topCtx)
  tyDef <- gets @"top" (view topTyDefs)
  mapM_ showTyDef $ M.toList tyDef
  mapM_ showFn $ M.toList ctx
  where
      showTyDef (name, body) = do
        p <- renderDoc (prettyTyDef name body)
        io . putStrLn $ p
      showFn (x, ty) = do
        p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
        io . putStrLn $ p


nopCmd :: REPLCommand 'CNop
nopCmd =
    REPLCommand {
      name = "nop",
      helpcmd = "",
      shortHelp = "No-op, e.g. if the user just enters a comment",
      category = Dev,
      cmdtype = BuiltIn,
      action = handleNop,
      parser = Nop <$ (sc <* eof)
    }

handleNop :: Applicative m => REPLExpr 'CNop -> m ()
handleNop Nop = pure ()


parseCmd :: REPLCommand 'CParse
parseCmd =
    REPLCommand {
      name = "parse",
      helpcmd = ":parse <expr>",
      shortHelp = "Show the parsed AST",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleParse,
      parser = Parse <$> term
    }

handleParse :: MonadIO m => REPLExpr 'CParse -> m ()
handleParse (Parse t) = iprint t


prettyCmd :: REPLCommand 'CPretty
prettyCmd =
    REPLCommand {
      name = "pretty",
      helpcmd = ":pretty <expr>",
      shortHelp = "Pretty-print a term",
      category = Dev,
      cmdtype = ShellCmd,
      action = handlePretty,
      parser = Pretty <$> term
    }

handlePretty :: Has '[MonadIO, LFresh] m => REPLExpr 'CPretty -> m ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= iputStrLn


reloadCmd :: REPLCommand 'CReload
reloadCmd =
    REPLCommand {
      name = "reload",
      helpcmd = ":reload",
      shortHelp = "Reloads the most recently loaded file",
      category = User,
      cmdtype = ShellCmd,
      action = handleReload,
      parser = return Reload
    }

handleReload :: (Has '[St "top", St "lastfile"] m, MonadDisco m) => REPLExpr 'CReload -> m ()
handleReload Reload = do
      file <- get @"lastfile"
      case file of
        Nothing -> iputStrLn "No file to reload."
        Just f  -> void (handleLoad f)


showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd =
    REPLCommand {
      name = "defn",
      helpcmd = ":defn <var>",
      shortHelp = "Show a variable's definition",
      category = User,
      cmdtype = ShellCmd,
      action = handleShowDefn,
      parser = ShowDefn <$> (sc *> ident)
    }

handleShowDefn :: Has '[St "top", MonadIO, LFresh] m => REPLExpr 'CShowDefn -> m ()
handleShowDefn (ShowDefn x) = do
  defns   <- gets @"top" (view topDefs)
  tyDefns <- gets @"top" (view topTyDefs)
  s <- case M.lookup (coerce x) defns of
          Just d  -> renderDoc $ prettyDefn d
          Nothing -> case M.lookup name2s tyDefns of
            Just t  -> renderDoc $ prettyTyDef name2s t
            Nothing -> return $ "No definition for " ++ show x
  iputStrLn s
  where
    name2s = name2String x


testPropCmd :: REPLCommand 'CTestProp
testPropCmd =
    REPLCommand {
      name = "test",
      helpcmd = ":test <property>",
      shortHelp = "Test a property using random examples",
      category = User,
      cmdtype = ShellCmd,
      action = handleTest,
      parser = TestProp <$> term
    }

handleTest :: (Has '[St "top"] m, MonadDisco m) => REPLExpr 'CTestProp -> m ()
handleTest (TestProp t) = do
  ctx   <- gets @"top" (view topCtx)
  tymap <- gets @"top" (view topTyDefs)
  case evalTCM (extends @"tyctx" ctx $ withTyDefns tymap $ checkProperty t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right at -> do
      withTopEnv $ do
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

handleTypeCheck :: Has '[St "top", MonadIO, LFresh] m => REPLExpr 'CTypeCheck -> m ()
handleTypeCheck (TypeCheck t) = do
  ctx   <- gets @"top" (view topCtx)
  tymap <- gets @"top" (view topTyDefs)
  s <- case evalTCM $ extends @"tyctx" ctx $ withTyDefns tymap $ inferTop t of
        Left e        -> return.show $ e    -- XXX pretty-print
        Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettyPolyTy sig
  iputStrLn s

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
usingCmd =
    REPLCommand {
        name = "using",
        helpcmd = "using <extension>",
        shortHelp = "Enable an extension",
        category = User,
        cmdtype = BuiltIn,
        action = handleUsing,
        parser = Using <$> (reserved "using" *> parseExtName)
        }

handleUsing :: Has '[St "exts"] m => REPLExpr 'CUsing -> m ()
handleUsing (Using e) = modify @"exts" $ addExtension e

------------------------------------------
--- Util functions
------------------------------------------

fileNotFound :: MonadIO m => FilePath -> IOException -> m ()
fileNotFound file _ = iputStrLn $ "File not found: " ++ file

loadFile :: MonadIO m => FilePath -> m (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

addModule :: Has '[St "top", Rd "env", Sc "nextloc", St "mem", Th "err"] m => ModuleInfo -> m ()
addModule mi = do
  curMI <- gets @"top" (view topModInfo)
  mi' <- adaptError TypeCheckErr $ combineModuleInfo [curMI, mi]
  setLoadedModule mi'

-- | Add information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule :: Has '[St "top", Rd "env", Sc "nextloc", St "mem"] m => ModuleInfo -> m ()
setLoadedModule mi = do
  modify @"top" $ topModInfo .~ mi
  populateCurrentModuleInfo

populateCurrentModuleInfo :: Has '[St "top", Rd "env", Sc "nextloc", St "mem"] m => m ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds <- gets @"top" (view topModInfo)
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  modify @"top" $
    (topDocs   .~ docs) .
    (topCtx    .~ tys)  .
    (topTyDefs .~ tyds) .
    (topDefs   .~ tmds)
  loadDefs cdefns
  return ()

-- XXX redo with message framework, with proper support for indentation etc.
-- XXX move it to Pretty or Property or something
prettyTestFailure :: (Has '[St "top"] m, MonadDisco m) => AProperty -> TestResult -> m ()
prettyTestFailure _    (TestResult True _ _)    = return ()
prettyTestFailure prop (TestResult False r env) = do
  prettyFailureReason prop r
  prettyTestEnv "    Counterexample:" env

prettyTestResult :: (Has '[St "top"] m, MonadDisco m) => AProperty -> TestResult -> m ()
prettyTestResult prop r | not (testIsOk r) = prettyTestFailure prop r
prettyTestResult prop (TestResult _ r _)   = do
    dp <- renderDoc $ prettyProperty (eraseProperty prop)
    iputStr       "  - Test passed: " >> iputStrLn dp
    prettySuccessReason r

prettySuccessReason :: (Has '[St "top"] m, MonadDisco m) => TestReason -> m ()
prettySuccessReason (TestFound (TestResult _ _ vs)) = do
  prettyTestEnv "    Found example:" vs
prettySuccessReason (TestNotFound Exhaustive) = do
  iputStrLn     "    No counterexamples exist."
prettySuccessReason (TestNotFound (Randomized n m)) = do
  iputStr       "    Checked "
  iputStr (show (n + m))
  iputStrLn " possibilities without finding a counterexample."
prettySuccessReason _ = return ()

prettyFailureReason :: (Has '[St "top"] m, MonadDisco m) => AProperty -> TestReason -> m ()
prettyFailureReason prop TestBool = do
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStr     "  - Test is false: " >> iputStrLn dp
prettyFailureReason prop (TestEqual ty v1 v2) = do
  iputStr     "  - Test result mismatch for: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    - Left side:  " >> prettyValue ty v2
  iputStr     "    - Right side: " >> prettyValue ty v1
prettyFailureReason prop (TestRuntimeError e) = do
  iputStr     "  - Test failed: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    " >> iprint e
prettyFailureReason prop (TestFound (TestResult _ r _)) = do
  prettyFailureReason prop r
prettyFailureReason prop (TestNotFound Exhaustive) = do
  iputStr     "  - No example exists: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStrLn   "    All possible values were checked."
prettyFailureReason prop (TestNotFound (Randomized n m)) = do
  iputStr     "  - No example was found: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    Checked " >> iputStr (show (n + m)) >> iputStrLn " possibilities."

prettyTestEnv :: (Has '[St "top"] m, MonadDisco m) => String -> TestEnv -> m ()
prettyTestEnv _ (TestEnv []) = return ()
prettyTestEnv s (TestEnv vs) = do
  iputStrLn s
  mapM_ prettyBind vs
  where
    maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
    prettyBind (x, ty, v) = do
      iputStr "      "
      iputStr x
      iputStr (replicate (maxNameLen - length x) ' ')
      iputStr " = "
      prettyValue ty v

runTest :: MonadDisco m => Int -> AProperty -> m TestResult
runTest n p = testProperty (Randomized n' n') =<< mkValue (compileProperty p)
  where
    n' = fromIntegral (n `div` 2)

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: (Has '[St "top"] m, MonadDisco m) => Ctx ATerm [AProperty] -> m Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      io $ putStrLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: (Has '[St "top"] m, MonadDisco m) => Name ATerm -> [AProperty] -> m Bool
    runTests n props = do
      iputStr ("  " ++ name2String n ++ ":")
      results <- traverse (sequenceA . (id &&& runTest numSamples)) props
      let failures = filter (not . testIsOk . snd) results
      case null failures of
        True  -> iputStrLn " OK"
        False -> do
          iputStrLn ""
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

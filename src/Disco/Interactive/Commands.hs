{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
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

import           Disco.Parser                            (ident, sc, term)


import           System.Console.Haskeline                as H
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.Except
import           Data.Coerce
import           Data.List                               (sortBy)
import qualified Data.Map                                as M
import           Data.Typeable
import           System.FilePath                         (splitFileName)

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
import           Disco.Parser                            (parseExtName,
                                                          parseImport, reserved)
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Typecheck.Monad
import           Disco.Types
import           Text.Megaparsec                         hiding (runParser)
import           Unbound.Generics.LocallyNameless

dispatch :: [SomeREPLCommand] -> SomeREPLExpr -> Disco IErr ()
dispatch [] _ = return ()
dispatch (SomeCmd c : cs) r@(SomeREPL e) = case gcast e of
  Just e' -> action c e'
  Nothing -> dispatch cs r

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
    SomeCmd usingCmd
  ]

------------------------------------------
-- Commands
------------------------------------------

annCmd :: REPLCommand 'CAnn
annCmd =
    REPLCommand {
      name = "ann",
      shortHelp = "Show type-annotated typechecked term",
      longHelp = "",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleAnn,
      parser = Ann <$> term
    }


compileCmd :: REPLCommand 'CCompile
compileCmd =
    REPLCommand {
      name = "compile",
      shortHelp = "Show a compiled term",
      longHelp = "",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleCompile,
      parser = Compile <$> term
    }

desugarCmd :: REPLCommand 'CDesugar
desugarCmd =
    REPLCommand {
      name = "desugar",
      shortHelp = "Show a desugared term",
      longHelp = "",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleDesugar,
      parser = Desugar <$> term
    }

docCmd :: REPLCommand 'CDoc
docCmd =
    REPLCommand {
      name = "doc",
      shortHelp = "Show documentation",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleDoc,
      parser = Doc <$> (sc *> ident)
    }

evalCmd :: REPLCommand 'CEval
evalCmd =
    REPLCommand {
      name = "eval",
      shortHelp = "Evaluate a term",
      longHelp = "",
      category = User,
      cmdtype = BuiltIn,
      action = handleEval,
      parser = Eval <$> term
    }

helpCmd :: REPLCommand 'CHelp
helpCmd =
    REPLCommand {
      name = "help",
      shortHelp = "Show help",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleHelp,
      parser = return Help
    }

importCmd :: REPLCommand 'CImport
importCmd =
    REPLCommand {
      name = "import",
      shortHelp = "Import a library module",
      longHelp = "",
      category = User,
      cmdtype = BuiltIn,
      action = handleImport,
      parser = Import <$> parseImport
    }

letCmd :: REPLCommand 'CLet
letCmd =
    REPLCommand {
      name = "let",
      shortHelp = "Toplevel let-expression: for the REPL",
      longHelp = "",
      category = User,
      cmdtype = BuiltIn,
      action = handleLet,
      parser = letParser
    }

loadCmd :: REPLCommand 'CLoad
loadCmd =
    REPLCommand {
      name = "load",
      shortHelp = "Load a file",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleLoadWrapper,
      parser = Load <$> fileParser
    }

namesCmd :: REPLCommand 'CNames
namesCmd =
    REPLCommand {
      name = "names",
      shortHelp = "Show all names in current scope",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleNames,
      parser = return Names
    }

nopCmd :: REPLCommand 'CNop
nopCmd =
    REPLCommand {
      name = "nop",
      shortHelp = "No-op, e.g. if the user just enters a comment",
      longHelp = "",
      category = Dev,
      cmdtype = BuiltIn,
      action = handleNop,
      parser = Nop <$ (sc <* eof)
    }

parseCmd :: REPLCommand 'CParse
parseCmd =
    REPLCommand {
      name = "parse",
      shortHelp = "Show the parsed AST",
      longHelp = "",
      category = Dev,
      cmdtype = ShellCmd,
      action = handleParse,
      parser = Parse <$> term
    }

prettyCmd :: REPLCommand 'CPretty
prettyCmd =
    REPLCommand {
      name = "pretty",
      shortHelp = "Pretty-print a term",
      longHelp = "",
      category = Dev,
      cmdtype = ShellCmd,
      action = handlePretty,
      parser = Pretty <$> term
    }

reloadCmd :: REPLCommand 'CReload
reloadCmd =
    REPLCommand {
      name = "reload",
      shortHelp = "Reloads the most recently loaded file",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleReload,
      parser = return Reload
    }

showDefnCmd :: REPLCommand 'CShowDefn
showDefnCmd =
    REPLCommand {
      name = "defn",
      shortHelp = "Show a variable's definition",
      longHelp = "",
      category = User,
      cmdtype = ShellCmd,
      action = handleShowDefn,
      parser = ShowDefn <$> (sc *> ident)
    }

typeCheckCmd :: REPLCommand 'CTypeCheck
typeCheckCmd =
    REPLCommand {
        name = "type",
        shortHelp = "Typecheck a term",
        longHelp = "",
        category = Dev,
        cmdtype = ShellCmd,
        action = handleTypeCheck,
        parser = TypeCheck <$> parseTypeTarget
        }

usingCmd :: REPLCommand 'CUsing
usingCmd =
    REPLCommand {
        name = "using",
        shortHelp = "Enable an extension",
        longHelp = "",
        category = Dev,
        cmdtype = BuiltIn,
        action = handleUsing,
        parser = Using <$> (reserved "using" *> parseExtName)
        }


-- ------------------------------------------
-- --- Command implementations
-- ------------------------------------------

handleAnn :: REPLExpr 'CAnn -> Disco IErr ()
handleAnn (Ann t) = do
    ctx   <- use topCtx
    tymap <- use topTyDefns
    s <- case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
        Left  e       -> return . show $ e
        Right (at, _) -> return . show $ at
    iputStrLn s

handleCompile :: REPLExpr 'CCompile -> Disco IErr ()
handleCompile (Compile t) = do
  ctx <- use topCtx
  s <- case evalTCM (extends ctx $ inferTop t) of
        Left e       -> return . show $ e
        Right (at,_) -> return . show . compileTerm $ at
  iputStrLn s

handleDesugar :: REPLExpr 'CDesugar -> Disco IErr ()
handleDesugar (Desugar t) = do
  ctx <- use topCtx
  s <- case evalTCM (extends ctx $ inferTop t) of
        Left e       -> return.show $ e
        Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at
  iputStrLn s

handleDoc :: REPLExpr 'CDoc -> Disco IErr ()
handleDoc (Doc x) = do
  ctx  <- use topCtx
  docs <- use topDocs
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _                       -> return ()


handleEval :: REPLExpr 'CEval -> Disco IErr ()
handleEval (Eval t) = do
  ctx   <- use topCtx
  tymap <- use topTyDefns
  case evalTCM (extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = compileTerm at
      in do
        v <- withTopEnv $ do
          cv <- mkValue c
          prettyValue ty cv
          return cv
        topCtx %= M.insert (string2Name "it") (toPolyType ty)
        topEnv %= M.insert (string2Name "it") v
        garbageCollect

handleHelp :: REPLExpr 'CHelp -> Disco IErr ()
handleHelp Help = do
  iputStrLn "Commands available from the prompt:\n"
  mapM_ (\(SomeCmd c) -> iputStrLn $ ":" ++ name c ++ "\t\t" ++ shortHelp c) $  sortedList discoCommands
  iputStrLn ""
  where
    sortedList cmds = sortBy (\(SomeCmd x) (SomeCmd y) -> compare (name x) (name y)) $ filteredCommands cmds
    -- remove "builtins" (let, eval, etc), don't show dev-only commands by default
    filteredCommands cmds = filter (\(SomeCmd c) -> category c == User) $ withoutBuiltins cmds

handleImport :: REPLExpr 'CImport -> Disco IErr ()
handleImport (Import modName) = catchAndPrintErrors () $ do
  mi <- loadDiscoModule FromCwdOrStdlib modName
  addModule mi

handleLet :: REPLExpr 'CLet -> Disco IErr ()
handleLet (Let x t) = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  let mat = evalTCM (extends ctx $ withTyDefns tymap $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, sig) -> do
      let c = compileTerm at
      thnk <- withTopEnv (mkValue c)
      topCtx   %= M.insert x sig
        -- XXX ability to define more complex things at REPL prompt, with patterns etc.
      topDefns %= M.insert (coerce x) (Defn (coerce x) [] (getType at) [bind [] at])
      topEnv   %= M.insert (coerce x) thnk

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
--   Disco.Interactive.CmdLine uses a version of this function that returns a Bool.
handleLoadWrapper :: REPLExpr 'CLoad -> Disco IErr ()
handleLoadWrapper (Load fp) =  handleLoad fp >> return ()

handleLoad :: FilePath -> Disco IErr Bool
handleLoad fp = catchAndPrintErrors False $ do
  let (directory, modName) = splitFileName fp
  m@(ModuleInfo _ props _ _ _) <- loadDiscoModule (FromDir directory) modName
  setLoadedModule m
  t <- withTopEnv $ runAllTests props
  io . putStrLn $ "Loaded."
  garbageCollect
  return t

-- | show names and types for each item in 'topCtx'
handleNames :: REPLExpr 'CNames -> Disco IErr ()
handleNames Names = do
  ctx  <- use topCtx
  mapM_ showFn $ M.toList ctx
  where
      showFn (x, ty) = do
        p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
        io . putStrLn $ p

handleNop :: REPLExpr 'CNop -> Disco IErr ()
handleNop Nop = return ()

handleParse :: REPLExpr 'CParse -> Disco IErr ()
handleParse (Parse t) = iprint $ t

handlePretty :: REPLExpr 'CPretty -> Disco IErr ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= iputStrLn

handleReload :: REPLExpr 'CReload -> Disco IErr ()
handleReload Reload = do
      file <- use lastFile
      case file of
        Nothing -> iputStrLn "No file to reload."
        Just f  -> handleLoad f >> return()

handleShowDefn :: REPLExpr 'CShowDefn -> Disco IErr ()
handleShowDefn (ShowDefn x) = do
  defns   <- use topDefns
  tyDefns <- use topTyDefns
  s <- case M.lookup (coerce x) defns of
          Just d  -> renderDoc $ prettyDefn d
          Nothing -> case M.lookup name2s tyDefns of
            Just t  -> renderDoc $ prettyTyDef name2s t
            Nothing -> return $ "No definition for " ++ show x
  iputStrLn s
  where
    name2s = name2String x


handleTypeCheck :: REPLExpr 'CTypeCheck -> Disco IErr ()
handleTypeCheck (TypeCheck t) = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  s <- case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
        Left e        -> return.show $ e    -- XXX pretty-print
        Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettyPolyTy sig
  iputStrLn s

handleUsing :: REPLExpr 'CUsing -> Disco IErr ()
handleUsing (Using e) = enabledExts %= addExtension e

------------------------------------------
--- Util functions
------------------------------------------

addModule :: ModuleInfo -> Disco IErr ()
addModule mi = do
  curMI <- use topModInfo
  mi' <- adaptError TypeCheckErr $ combineModuleInfo [curMI, mi]
  topModInfo .= mi'
  populateCurrentModuleInfo

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file

loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

populateCurrentModuleInfo :: Disco IErr ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds <- use topModInfo
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  topDocs    .= docs
  topCtx     .= tys
  topTyDefns .= tyds
  topDefns   .= tmds
  loadDefs cdefns
  return ()

-- XXX comment, move somewhere else
prettyCounterexample :: Ctx ATerm Type -> Env -> Disco IErr ()
prettyCounterexample ctx env
  | M.null env = return ()
  | otherwise  = do
      iputStrLn "    Counterexample:"
      let maxNameLen = maximum . map (length . name2String) $ M.keys env
      mapM_ (prettyBind maxNameLen) $ M.assocs env
  where
    prettyBind maxNameLen (x,v) = do
      iputStr "      "
      iputStr =<< (renderDoc . prettyName $ x)
      iputStr (replicate (maxNameLen - length (name2String x)) ' ')
      iputStr " = "
      prettyValue (ctx !? coerce x) v
    m !? k = case M.lookup k m of
      Just val -> val
      Nothing  -> error $ "Failed M.! with key " ++ show k ++ " in map " ++ show m

-- XXX redo with message framework, with proper support for indentation etc.
-- XXX also move it to Property or Pretty or somewhere like that
prettyTestFailure :: AProperty -> TestResult -> Disco IErr ()
prettyTestFailure _ (TestOK {}) = return ()
prettyTestFailure prop (TestFalse env) = do
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStr "  - Test is false: " >> iputStrLn dp
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestEqualityFailure ty v1 v2 env) = do
  iputStr     "  - Test result mismatch for: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    - Expected: " >> prettyValue ty v2
  iputStr     "    - But got:  " >> prettyValue ty v1
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestRuntimeFailure e) = do
  iputStr     "  - Test failed: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    " >> iprint e

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: Ctx ATerm [AProperty] -> Disco IErr Bool  -- (Ctx ATerm [TestResult])
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

    runTests :: Name ATerm -> [AProperty] -> Disco IErr Bool
    runTests n props = do
      iputStr ("  " ++ name2String n ++ ":")
      results <- sequenceA . fmap sequenceA $ map (id &&& runTest numSamples) props
      let failures = filter (not . testIsOK . snd) results
      case null failures of
        True  -> iputStrLn " OK"
        False -> do
          iputStrLn ""
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

-- | Add information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule :: ModuleInfo -> Disco IErr ()
setLoadedModule mi = do
  topModInfo .= mi
  populateCurrentModuleInfo

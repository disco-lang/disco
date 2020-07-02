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
    allCommands,
    discoBuiltins,
    discoCommands,
    getAction,
    getCommand,
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
import           Data.List                               (find, sortBy)
import qualified Data.Map                                as M
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

-- | Allow getting an action from a REPLCommand outside of this module
getAction :: REPLCommand -> REPLExpr -> Disco IErr ()
getAction = action

-- | Search for a command to handle a given REPLExpr
getCommand :: REPLExpr -> [REPLCommand] -> Maybe REPLCommand
getCommand r commandList = findIt $ toTag r
          where
            findIt tagname = find (\c -> name c == tagname) commandList

allCommands :: [REPLCommand]
allCommands = discoBuiltins ++ discoCommands

discoBuiltins :: [REPLCommand]
discoBuiltins =
  [
    importCmd,
    letCmd,
    nopCmd,
    usingCmd,
    evalCmd
  ]

discoCommands :: [REPLCommand]
discoCommands =
  [
    annCmd,
    compileCmd,
    desugarCmd,
    docCmd,
    helpCmd,
    loadCmd,
    namesCmd,
    parseCmd,
    prettyCmd,
    reloadCmd,
    showDefnCmd,
    typeCheckCmd
  ]


-- | Turns a REPLExpr into a single string as a way
--   to exec REPLCommand actions dynamically
toTag :: REPLExpr -> String
toTag (TypeCheck _) = name typeCheckCmd
toTag (Let _ _    ) = name letCmd
toTag (Eval     _ ) = name evalCmd
toTag (ShowDefn _ ) = name showDefnCmd
toTag (Parse    _ ) = name parseCmd
toTag (Pretty   _ ) = name prettyCmd
toTag (Ann      _ ) = name annCmd
toTag (Desugar  _ ) = name desugarCmd
toTag (Compile  _ ) = name compileCmd
toTag (Import   _ ) = name importCmd
toTag (Load     _ ) = name loadCmd
toTag Reload        = name reloadCmd
toTag (Doc _)       = name docCmd
toTag Nop           = name nopCmd
toTag Help          = name helpCmd
toTag Names         = name namesCmd
toTag (Using _)     = name usingCmd


------------------------------------------
-- Commands
------------------------------------------

annCmd :: REPLCommand
annCmd =
    REPLCommand {
      name = "ann",
      shortHelp = "Show type-annotated typechecked term",
      longHelp = "",
      category = Dev,
      cmdtype = ColonCmd,
      action = handleAnnWrapper,
      parser = Ann <$> term
    }


compileCmd :: REPLCommand
compileCmd =
    REPLCommand {
      name = "compile",
      shortHelp = "Show a compiled term",
      longHelp = "",
      category = Dev,
      cmdtype = ColonCmd,
      action = handleCompileWrapper,
      parser = Compile   <$> term
    }

desugarCmd :: REPLCommand
desugarCmd =
    REPLCommand {
      name = "desugar",
      shortHelp = "Show a desugared term",
      longHelp = "",
      category = Dev,
      cmdtype = ColonCmd,
      action = handleDesugar,
      parser = Desugar   <$> term
    }

docCmd :: REPLCommand
docCmd =
    REPLCommand {
      name = "doc",
      shortHelp = "Show documentation",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleDoc,
      parser = Doc <$> (sc *> ident)
    }

evalCmd :: REPLCommand
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


helpCmd :: REPLCommand
helpCmd =
    REPLCommand {
      name = "help",
      shortHelp = "Show help",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleHelp,
      parser = return Help
    }

importCmd :: REPLCommand
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

letCmd :: REPLCommand
letCmd =
    REPLCommand {
      name = "let",
      shortHelp = "Toplevel let-expression: for the REPL",
      longHelp = "",
      category = User,
      cmdtype = BuiltIn,
      action = handleLet,
      parser = Load <$> fileParser
    }

loadCmd :: REPLCommand
loadCmd =
    REPLCommand {
      name = "load",
      shortHelp = "Load a file",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleLoadWrapper,
      parser = Load <$> fileParser
    }

namesCmd :: REPLCommand
namesCmd =
    REPLCommand {
      name = "names",
      shortHelp = "Show all names in current scope",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleNames,
      parser = return Names
    }

nopCmd :: REPLCommand
nopCmd =
    REPLCommand {
      name = "nop",
      shortHelp = "No-op, e.g. if the user just enters a comment",
      longHelp = "",
      category = Dev,
      cmdtype = BuiltIn,
      action = handleNop,
      parser = Nop <$ (sc <* eof)
    } -- TODO

parseCmd :: REPLCommand
parseCmd =
    REPLCommand {
      name = "parse",
      shortHelp = "Show the parsed AST",
      longHelp = "",
      category = Dev,
      cmdtype = ColonCmd,
      action = handleParse,
      parser = Parse <$> term
    }

prettyCmd :: REPLCommand
prettyCmd =
    REPLCommand {
      name = "pretty",
      shortHelp = "Pretty-print a term",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handlePretty,
      parser = Pretty <$> term
    }

reloadCmd :: REPLCommand
reloadCmd =
    REPLCommand {
      name = "reload",
      shortHelp = "Reloads the most recently loaded file",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleReload,
      parser = return Reload
    }

showDefnCmd :: REPLCommand
showDefnCmd =
    REPLCommand {
      name = "defn",
      shortHelp = "Show a variable's definition",
      longHelp = "",
      category = User,
      cmdtype = ColonCmd,
      action = handleShowDefn,
      parser = ShowDefn  <$> (sc *> ident)
    }

typeCheckCmd :: REPLCommand
typeCheckCmd =
    REPLCommand {
        name = "type",
        shortHelp = "Typecheck a term",
        longHelp = "",
        category = User,
        cmdtype = ColonCmd,
        action = handleTypeCheck,
        parser = TypeCheck <$> parseTypeTarget
        }

usingCmd :: REPLCommand
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


------------------------------------------
--- Command implementations
------------------------------------------

handleAnnWrapper :: REPLExpr -> Disco IErr ()
handleAnnWrapper (Ann t) = handleAnn t >>= iputStrLn
handleAnnWrapper _       = return ()

handleAnn :: Term -> Disco IErr String
handleAnn t = do
    ctx   <- use topCtx
    tymap <- use topTyDefns
    case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
        Left  e       -> return . show $ e
        Right (at, _) -> return . show $ at

handleCompileWrapper :: REPLExpr -> Disco IErr ()
handleCompileWrapper (Compile t) = handleCompile t >>= iputStrLn
handleCompileWrapper _           = return ()

handleCompile :: Term -> Disco IErr String
handleCompile t = do
  ctx <- use topCtx
  case evalTCM (extends ctx $ inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> return.show.compileTerm $ at

handleDesugar :: REPLExpr -> Disco IErr ()
handleDesugar (Desugar t0) = handleDesugar_ t0 >>= iputStrLn
  where
    handleDesugar_ t = do
      ctx <- use topCtx
      case evalTCM (extends ctx $ inferTop t) of
        Left e       -> return.show $ e
        Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at
handleDesugar _ = return ()

handleDoc :: REPLExpr -> Disco IErr ()
handleDoc (Doc x0) =
  handleDoc_ x0
  where
    handleDoc_ :: Name Term -> Disco IErr ()
    handleDoc_ x = do
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
handleDoc _ = return ()


handleHelp :: REPLExpr -> Disco IErr ()
handleHelp _ = do
    iputStrLn "Commands available from the prompt:"
    mapM_ (\c -> iputStrLn $ ":" ++ name c ++ "\t\t" ++ shortHelp c) $ sortedList allCommands
    iputStrLn ""
    where
      sortedList cmds = sortBy (\x y -> compare (name x) (name y)) $ commandList cmds
      commandList cmds = filter (\c -> category c == User && cmdtype c == ColonCmd) cmds

handleImport :: REPLExpr -> Disco IErr ()
handleImport (Import i) = handleImport_ i
                 where
                  handleImport_ modName = catchAndPrintErrors () $ do
                    mi <- loadDiscoModule "" modName
                    addModule mi
handleImport _ = return ()

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
handleLoadWrapper :: REPLExpr -> Disco IErr ()
handleLoadWrapper (Load file) =
  handleLoad file >> lastFile .= Just file >>return ()
handleLoadWrapper _ = return ()

handleLoad :: FilePath -> Disco IErr Bool
handleLoad fp = catchAndPrintErrors False $ do
    let (directory, modName) = splitFileName fp
    m@(ModuleInfo _ props _ _ _) <- loadDiscoModule directory modName
    setLoadedModule m
    t <- withTopEnv $ runAllTests props
    io . putStrLn $ "Loaded."
    garbageCollect
    return t

handleNop :: REPLExpr -> Disco IErr ()
handleNop _ = return ()

handleParse :: REPLExpr -> Disco IErr ()
handleParse (Parse t) = iprint $ t
handleParse _         = return ()

handlePretty :: REPLExpr -> Disco IErr ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= iputStrLn
handlePretty _          = return ()

handleReload :: REPLExpr -> Disco IErr ()
handleReload _ = do
                  file <- use lastFile
                  case file of
                    Nothing -> iputStrLn "No file to reload."
                    Just f  -> handleLoad f >> return()

handleShowDefn :: REPLExpr -> Disco IErr ()
handleShowDefn (ShowDefn x0) =
  handleShowDefn_ x0 >>= iputStrLn
  where
  handleShowDefn_ x = do
    defns   <- use topDefns
    tyDefns <- use topTyDefns
    case M.lookup (coerce x) defns of
      Just d  -> renderDoc $ prettyDefn d
      Nothing -> case M.lookup tyname tyDefns of
        Just t  -> renderDoc $ prettyTyDef tyname t
        Nothing -> return $ "No definition for " ++ show x
    where
      tyname = name2String x
handleShowDefn _ = return ()


handleTypeCheck :: REPLExpr -> Disco IErr ()
handleTypeCheck (TypeCheck t0) =
    handleTypeCheck_ t0 >>= iputStrLn
    where
    handleTypeCheck_ t = do
      ctx <- use topCtx
      tymap <- use topTyDefns
      case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
        Left e        -> return.show $ e    -- XXX pretty-print
        Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettyPolyTy sig
handleTypeCheck _ = return ()

handleUsing :: REPLExpr -> Disco IErr ()
handleUsing (Using e) = enabledExts %= addExtension e
handleUsing _         = return ()

-- | show names and types for each item in 'topCtx'
handleNames :: REPLExpr -> Disco IErr ()
handleNames _ = do
  ctx  <- use topCtx
  mapM_ showFn $ M.toList ctx
  where
      showFn (x, ty) = do
        p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
        io . putStrLn $ p

------------------------------------------
--- Util functions
------------------------------------------

addModule :: ModuleInfo -> Disco IErr ()
addModule mi = do
  curMI <- use topModInfo
  mi' <- adaptError TypeCheckErr $ combineModuleInfo [curMI, mi]
  topModInfo .= mi'
  populateCurrentModuleInfo


loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file


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

-- | Add information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule :: ModuleInfo -> Disco IErr ()
setLoadedModule mi = do
  topModInfo .= mi
  populateCurrentModuleInfo

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

handleLet :: REPLExpr -> Disco IErr ()
handleLet (Let nt t0) =
    handleLet_ nt t0
    where
      handleLet_ x t = do
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
handleLet _ = return ()

handleEval :: REPLExpr -> Disco IErr ()
handleEval (Eval e) = evalTerm e
handleEval _        = return ()

evalTerm :: Term -> Disco IErr ()
evalTerm t = do
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


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  REPL
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A text-based REPL for disco.
--
-----------------------------------------------------------------------------

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (filterM, foldM, forM_,
                                                          when)
import           Control.Monad.Except                    (MonadError,
                                                          catchError,
                                                          throwError)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Trans.Class               (MonadTrans (..))
import           Control.Monad.Trans.State
import           Data.Char                               (isSpace)
import           Data.Coerce
import           Data.List                               (find, isPrefixOf)
import           Data.Map                                ((!))
import qualified Data.Map                                as M
import           Data.Maybe                              (isJust)

import qualified Data.Set                                as S
import qualified Options.Applicative                     as O
import           System.Console.Haskeline                as H
import           System.Directory
import           System.Exit
import           System.FilePath
import           Text.Megaparsec                         hiding (runParser)
import qualified Text.Megaparsec.Char                    as C
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Interpret.Core                    (loadDefs)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Typecheck.Monad
import           Disco.Types

import           Paths_disco

------------------------------------------------------------------------
-- Parsers for the REPL                                               --
------------------------------------------------------------------------

data REPLExpr =
   Let (Name Term) Term         -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | Eval Term                    -- Evaluate a term
 | ShowDefn (Name Term)         -- Show a variable's definition
 | Parse Term                   -- Show the parsed AST
 | Pretty Term                  -- Pretty-print a term
 | Ann Term                     -- Show type-annotated typechecked term
 | Desugar Term                 -- Show a desugared term
 | Compile Term                 -- Show a compiled term
 | Load FilePath                -- Load a file.
 | Reload                       -- Reloads the most recently loaded file.
 | Doc (Name Term)              -- Show documentation.
 | Nop                          -- No-op, e.g. if the user just enters a comment
 | Help
 deriving Show

letParser :: Parser REPLExpr
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: Parser REPLExpr
commandParser = (symbol ":" *> many C.lowerChar) >>= parseCommandArgs

parseCommandArgs :: String -> Parser REPLExpr
parseCommandArgs cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    parsers =
      [ ("type",    TypeCheck <$> term)
      , ("defn",    ShowDefn  <$> (sc *> ident))
      , ("parse",   Parse     <$> term)
      , ("pretty",  Pretty    <$> term)
      , ("ann",     Ann       <$> term)
      , ("desugar", Desugar   <$> term)
      , ("compile", Compile   <$> term)
      , ("load",    Load      <$> fileParser)
      , ("reload",  return Reload)
      , ("doc",     Doc       <$> (sc *> ident))
      , ("help",    return Help)
      ]

fileParser :: Parser FilePath
fileParser = many C.spaceChar *> many (C.satisfy (not . isSpace))

lineParser :: Parser REPLExpr
lineParser
  =   commandParser
  <|> try (Nop <$ (sc <* eof))
  <|> try (Eval <$> term)
  <|> letParser

parseLine :: String -> Either String REPLExpr
parseLine s =
  case (runParser lineParser "" s) of
    Left  e -> Left $ parseErrorPretty' s e
    Right l -> Right l

-- XXX eventually this should switch from using IErr specifically to
-- an umbrella error type in which IErr can be embedded, along with
-- e.g. parse or type errors

handleCMD :: String -> Disco IErr ()
handleCMD "" = return ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l `catchError` (io . print  {- XXX pretty-print error -})
  where
    handleLine :: REPLExpr -> Disco IErr ()

    handleLine (Let x t)     = handleLet x t
    handleLine (TypeCheck t) = handleTypeCheck t        >>= iputStrLn
    handleLine (Eval t)      = evalTerm t
    handleLine (ShowDefn x)  = handleShowDefn x         >>= iputStrLn
    handleLine (Parse t)     = iprint $ t
    handleLine (Pretty t)    = renderDoc (prettyTerm t) >>= iputStrLn
    handleLine (Ann t)       = handleAnn t              >>= iputStrLn
    handleLine (Desugar t)   = handleDesugar t          >>= iputStrLn
    handleLine (Compile t)   = handleCompile t          >>= iputStrLn
    handleLine (Load file)   = handleLoad file >> lastFile .= Just file >>return ()
    handleLine (Reload)      = do
      file <- use lastFile
      case file of
        Nothing -> iputStrLn "No file to reload."
        Just f  -> handleLoad f >> return()
    handleLine (Doc x)       = handleDocs x
    handleLine Nop           = return ()
    handleLine Help          = iputStrLn "Help!"

handleLet :: Name Term -> Term -> Disco IErr ()
handleLet x t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  let mat = evalTCM (extends ctx $ withTyDefns tymap $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, sig) -> do
      let c = compileTerm at
      thnk <- mkThunk c
      topCtx   %= M.insert x sig
      topDefns %= M.insert (coerce x) c
      topEnv   %= M.insert (coerce x) thnk

handleShowDefn :: Name Term -> Disco IErr String
handleShowDefn x = do
  defns <- use topDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleAnn :: Term -> Disco IErr String
handleAnn t = do
  case evalTCM (inferTop t) of
    Left e       -> return . show $ e
    Right (at,_) -> return . show $ at

handleDesugar :: Term -> Disco IErr String
handleDesugar t = do
  case evalTCM (inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at

handleCompile :: Term -> Disco IErr String
handleCompile t = do
  case evalTCM (inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> return.show.compileTerm $ at

loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file

-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling recCheckMod. If no errors are thrown, any tests present
--   in the parent module are executed.
handleLoad :: FilePath -> Disco IErr Bool
handleLoad fp = catchAndPrintErrors False $ do
  let (directory, modName) = splitFileName fp
  modMap <- execStateT (recCheckMod directory S.empty modName) M.empty
  let m@(ModuleInfo _ props _ _ _) = modMap M.! modName
  addModInfo m
  t <- withTopEnv $ runAllTests props
  io . putStrLn $ "Loaded."
  return t

-- | Added information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
addModInfo :: ModuleInfo -> Disco IErr ()
addModInfo (ModuleInfo docs _ tys tyds tmds) = do
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  topDocs  .= docs
  topCtx   .= tys
  topTyDefns .= tyds
  loadDefs cdefns
  return ()

-- | Typechecks a given module by first recursively typechecking it's imported modules,
--   adding the obtained module infos to a map from module names to module infos, and then
--   typechecking the parent module in an environment with access to this map. This is really just a
--   depth-first search.
recCheckMod :: FilePath -> S.Set ModName -> ModName -> StateT (M.Map ModName ModuleInfo) (Disco IErr) ModuleInfo
recCheckMod directory inProcess modName  = do
  when (S.member modName inProcess) (throwError $ CyclicImport modName)
  modMap <- get
  case M.lookup modName modMap of
    Just mi -> return mi
    Nothing -> do
      file <- resolveModule directory modName
      io . putStrLn $ "Loading " ++ (modName -<.> "disco") ++ "..."
      cm@(Module _ mns _ _) <- lift $ parseDiscoModule file

      -- mis only contains the module info from direct imports.
      mis <- mapM (recCheckMod directory (S.insert modName inProcess)) mns
      imports@(ModuleInfo _ _ tyctx tydefns _) <- combineModuleInfo mis
      m  <- lift $ typecheckDisco tyctx tydefns (checkModule cm)
      m' <- combineModuleInfo [imports, m]
      modify (M.insert modName m')
      return m'

-- | Merges a list of ModuleInfos into one ModuleInfo. Two ModuleInfos are merged by
--   joining their doc, type, type definition, and term contexts. The property context
--   of the new module is the obtained from the second module. If threre are any duplicate
--   type definitions or term definitions, a Typecheck error is thrown.
combineModuleInfo :: (MonadError IErr m) => [ModuleInfo] -> m ModuleInfo
combineModuleInfo mis = foldM combineMods emptyModuleInfo mis
  where combineMods :: (MonadError IErr m) => ModuleInfo -> ModuleInfo -> m ModuleInfo
        combineMods (ModuleInfo d1 _ ty1 tyd1 tm1) (ModuleInfo d2 p2 ty2 tyd2 tm2) =
          case (M.keys $ M.intersection tyd1 tyd2, M.keys $ M.intersection tm1 tm2) of
            ([],[]) -> return $ ModuleInfo (joinCtx d1 d2) p2 (joinCtx ty1 ty2) (M.union tyd1 tyd2) (joinCtx tm1 tm2)
            (x:_, _) -> throwError $ TypeCheckErr $ DuplicateTyDefns (coerce x)
            (_, y:_) -> throwError $ TypeCheckErr $ DuplicateDefns (coerce y)

-- | Given a directory and a module name, relavent directories are searched for the file
--   containing the provided module name. Currently, Disco searches for the module in
--   the standard library directory (lib), and the directory passed in to resolveModule.
resolveModule :: (MonadError IErr m, MonadIO m) => FilePath -> ModName -> m FilePath
resolveModule directory modname = do
  datadir <- io getDataDir
  let fps = map (</> replaceExtension modname "disco") [directory, datadir]
  fexists <- io $ filterM doesFileExist fps
  case fexists of
    []     -> throwError $ ModuleNotFound modname
    (fp:_) -> return fp

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
      prettyValue (ctx ! coerce x) v

handleDocs :: Name Term -> Disco IErr ()
handleDocs x = do
  ctx  <- use topCtx
  docs <- use topDocs
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettySigma ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _                       -> return ()

evalTerm :: Term -> Disco IErr ()
evalTerm t = do
  ctx   <- use topCtx
  tymap <- use topTyDefns
  case evalTCM (extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = compileTerm at
      in (withTopEnv $ mkThunk c) >>= prettyValue ty

handleTypeCheck :: Term -> Disco IErr String
handleTypeCheck t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e        -> return.show $ e    -- XXX pretty-print
    Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettySigma sig

banner :: String
banner = "Welcome to Disco!\n\nA language for programming discrete mathematics.\n\n"

data DiscoOpts = DiscoOpts
  { evaluate  :: Maybe String
  , cmdFile   :: Maybe String
  , checkFile :: Maybe String
  }

discoOpts :: O.Parser DiscoOpts
discoOpts = DiscoOpts
  <$> optional (
        O.strOption (mconcat
          [ O.long "evaluate"
          , O.short 'e'
          , O.help "evaluate an expression"
          , O.metavar "TERM"
          ])
      )
  <*> optional (
        O.strOption (mconcat
          [ O.long "file"
          , O.short 'f'
          , O.help "execute the commands in a file"
          , O.metavar "FILE"
          ])
      )
  <*> optional (
        O.strOption (mconcat
          [ O.long "check"
          , O.help "check a file without starting the interactive REPL"
          , O.metavar "FILE"
          ])
      )

discoInfo :: O.ParserInfo DiscoOpts
discoInfo = O.info (O.helper <*> discoOpts) $ mconcat
  [ O.fullDesc
  , O.progDesc "Command-line interface for Disco, a programming language for discrete mathematics."
  , O.header "disco v0.1"
  ]

main :: IO ()
main = do
  opts <- O.execParser discoInfo

  let batch = any isJust [evaluate opts, cmdFile opts, checkFile opts]
      settings = defaultSettings
            { historyFile = Just ".disco_history" }
  when (not batch) $ putStr banner
  res <- runDisco $ do
    case checkFile opts of
      Just file -> do
        res <- handleLoad file
        io $ if res then exitSuccess else exitFailure
      Nothing   -> return ()
    case cmdFile opts of
      Just file -> do
        mcmds <- loadFile file
        case mcmds of
          Nothing   -> return ()
          Just cmds -> mapM_ handleCMD (lines cmds)
      Nothing   -> return ()
    case evaluate opts of
      Just str -> handleCMD str
      Nothing  -> return ()

    when (not batch) $ runInputT settings loop

  case res of

    -- All disco exceptions should be caught and handled by this point.
    Left e   -> do
      putStrLn $ "Uncaught error: " ++ show e
      putStrLn $ "Please report this as a bug: https://github.com/disco-lang/disco/issues"
    Right () -> return ()

  -- XXX pretty-print log messages here

  where

    ctrlC :: InputT (Disco e) a -> SomeException -> InputT (Disco e) a
    ctrlC act e = do
      io $ putStrLn (show e)
      act

    withCtrlC resume act = H.catch (H.withInterrupt act) (ctrlC resume)

    loop :: InputT (Disco IErr) ()
    loop = do
      minput <- withCtrlC (return $ Just "") (getInputLine "Disco> ")
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | otherwise -> do
              withCtrlC (return ()) $ (lift . handleCMD $ input)
              loop

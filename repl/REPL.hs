{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (forM_, when)
import           Control.Monad.Except                    (catchError)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Trans.Class               (MonadTrans (..))
import           Data.Char                               (isSpace)
import           Data.Coerce
import           Data.List                               (find, isPrefixOf)
import           Data.Map                                ((!))
import qualified Data.Map                                as M
import           Data.Maybe                              (isJust)

import qualified Options.Applicative                     as O
import           System.Console.Haskeline                as H
import           System.Exit
import           Text.Megaparsec                         hiding (runParser)
import qualified Text.Megaparsec.Char                    as C
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Interpret.Core                    (loadDefs)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Types

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
 | Desugar Term                 -- Show a desugared term
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
      , ("desugar", Desugar   <$> term)
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
    handleLine (Desugar t)   = handleDesugar t          >>= iputStrLn
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
  let mat = runTCM (extends ctx $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right ((at, sig), _) -> do
      topCtx   %= M.insert x sig
      topDefns %= M.insert (coerce x) (runDSM $ desugarTerm at)

handleShowDefn :: Name Term -> Disco IErr String
handleShowDefn x = do
  defns <- use topDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleDesugar :: Term -> Disco IErr String
handleDesugar t = do
  case evalTCM (inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> return.show.runDSM.desugarTerm $ at

loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file

handleLoad :: FilePath -> Disco IErr Bool
handleLoad file = do
  io . putStrLn $ "Loading " ++ file ++ "..."
  str <- io $ readFile file
  let mp = runParser wholeModule file str
  case mp of
    Left e   -> io $ putStrLn (parseErrorPretty' str e) >> return False
    Right p  ->
      case runTCM (checkModule p) of
        Left tcErr         -> io $ print tcErr >> return False
        Right ((docMap, aprops, ctx), (defns, tydefs)) -> do
          let cdefns = M.mapKeys coerce $ runDSM (mapM desugarDefn defns)
          topDocs    .= docMap
          topCtx     .= ctx
          topTyDefns .= tydefs
          loadDefs cdefns

          t <- withTopEnv $ runAllTests aprops
          io . putStrLn $ "Loaded."
          return t

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
      mapM_ (prettyBinding maxNameLen) $ M.assocs env
  where
    prettyBinding maxNameLen (x,v) = do
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
  case evalTCM (extends ctx $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = runDSM $ desugarTerm at
      in (withTopEnv $ mkThunk c) >>= prettyValue ty

handleTypeCheck :: Term -> Disco IErr String
handleTypeCheck t = do
  ctx <- use topCtx
  case (evalTCM $ extends ctx (inferTop t)) of
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

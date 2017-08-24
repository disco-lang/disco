{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (use, (%=), (.=))
import           Control.Monad                    (when, forM_, void, (>=>))
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Trans.Class        (MonadTrans(..))
import           Data.Char                        (isSpace)
import           Data.Coerce
import           Data.List                        (find, isPrefixOf)
import           Data.Map                         ((!))
import qualified Data.Map                         as M
import           Data.Maybe                       (isJust)
import           Data.Void

import qualified Options.Applicative              as O
import           System.Console.Haskeline         as H
import           System.Exit
import           Text.Megaparsec                  hiding (runParser)
import qualified Text.Megaparsec.Char             as C
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Messages
import           Disco.MessageLog
import           Disco.Interpret.Core             (loadDefs)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Types

-- XXX most of the stuff in here should be moved to libraries under
-- src/Disco/REPL/...

------------------------------------------------------------
-- Top-level errors
------------------------------------------------------------

-- | This type simply allows embedding all the different errors we can
--   get from various phases of the implementation into a single
--   top-level error type for the REPL.
data Err
  = InterpErr  IErr                            -- ^ Runtime interpreter error
  | ParseErr   String (ParseError Char Void)   -- ^ String that was being parsed + error
  | RawErr     String                          -- ^ A simple string error message
  | RawReport  Report                          -- ^ A structured error message
  deriving Show

-- | Format an 'Err' value as a 'Report', for display to the user.
formatErr :: Err -> Disco void Report
formatErr (InterpErr e)  = return . RTxt $ show e
formatErr (ParseErr s e) = return . RTxt $ parseErrorPretty' s e
formatErr (RawErr s)     = return . RTxt $ s
formatErr (RawReport r)  = return r

-- | Print all pending messages to the console, and delete them from
--   the message queue.
outputMessages :: Disco Err () -> Disco void ()
outputMessages m = do
  _ <- catchMessage formatErr $ m
  printAndClearMessages

------------------------------------------------------------------------
-- Parsers for the REPL                                               --
------------------------------------------------------------------------

data REPLExpr =
   Let (Name Term) Term         -- ^ Top-level let-expression: for the REPL
 | TypeCheck Term               -- ^ Typecheck a term
 | Eval Term                    -- ^ Evaluate a term
 | ShowDefn (Name Term)         -- ^ Show a variable's definition
 | Parse Term                   -- ^ Show the parsed AST
 | Pretty Term                  -- ^ Pretty-print a term
 | Desugar Term                 -- ^ Show a desugared term
 | Load FilePath                -- ^ Load a file.
 | Doc (Name Term)              -- ^ Show documentation.
 | Nop                          -- ^ No-op, e.g. if the user just enters a comment
 | Help                         -- ^ Display help
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

parseLine :: String -> Disco Err REPLExpr
parseLine s =
  case (runParser lineParser "" s) of
    Left  e  -> throwError $ ParseErr s e
    Right l  -> return l

------------------------------------------------------------
-- REPL command handlers
------------------------------------------------------------

-- XXX
handleCommand :: String -> Disco void ()
handleCommand "" = return ()
handleCommand s = outputMessages $ parseLine >=> handleLine $ s

  where
    handleLine :: REPLExpr -> Disco Err ()

    handleLine (Let x t)     = handleLet x t
    handleLine (TypeCheck t) = handleTypeCheck t        >>= iputStrLn
    handleLine (Eval t)      = evalTerm t
    handleLine (ShowDefn x)  = handleShowDefn x         >>= iputStrLn
    handleLine (Parse t)     = iprint $ t
    handleLine (Pretty t)    = renderDoc (prettyTerm t) >>= iputStrLn
    handleLine (Desugar t)   = handleDesugar t          >>= iputStrLn
    handleLine (Load file)   = handleLoad file >> return ()
           -- handleLoad returns a Bool to indicate success or failure
           -- of any test properties, but we don't care here.  Test
           -- failure messages will be printed, and the file will
           -- still load.

    handleLine (Doc x)       = handleDocs x
    handleLine Nop           = return ()
    handleLine Help          = iputStrLn "Help!"

-- XXX this doesn't work at the moment!  It never parses.
handleLet :: Name Term -> Term -> Disco void ()
handleLet x t = do
  ctx <- use topCtx
  let mat = runTCM (extends ctx $ infer t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, _) -> do
      topCtx   %= M.insert x (getType at)
      topDefns %= M.insert (coerce x) (runDSM $ desugarTerm at)

handleTypeCheck :: Term -> Disco Err String
handleTypeCheck t = do
  ctx <- use topCtx
  case (evalTCM $ extends ctx (infer t)) of
    Left e   -> return.show $ e    -- XXX pretty-print
    Right at -> renderDoc $ prettyTerm t <+> text ":" <+> (prettyTy.getType $ at)

evalTerm :: Term -> Disco Err ()
evalTerm t = do
  ctx   <- use topCtx
  case evalTCM (extends ctx $ infer t) of
    Left e   -> iprint e    -- XXX change TCM to run on top of Disco, and use catchMessage
    Right at ->
      let ty = getType at
          c  = runDSM $ desugarTerm at
      in  injectErrors InterpErr $
            (withTopEnv $ mkThunk c) >>= prettyValue ty

handleShowDefn :: Name Term -> Disco void String
handleShowDefn x = do
  defns <- use topDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleDesugar :: Term -> Disco void String
handleDesugar t = do
  case evalTCM (infer t) of
    Left e   -> return.show $ e
    Right at -> return.show.runDSM.desugarTerm $ at

-- XXX change to use a message instead of printing
loadFile :: FilePath -> Disco void (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file

-- XXX comment.  Return a Bool since if tests fail we don't want to
-- throw an exception, loading the file should still succeed; but we
-- need to know that tests failed so we can fail if in --check mode.
handleLoad :: FilePath -> Disco Err Bool
handleLoad file = do
  disableMessageBuffering $ info $ "Loading " ++ file ++ "..."
  str <- io $ readFile file
  let mp = runParser wholeModule file str
  case mp of
    Left e   -> throwError $ ParseErr str e
    Right p  ->
      case runTCM (checkModule p) of
        Left tcErr         -> throwError $ RawErr (show tcErr)   -- XXX
        Right ((docMap, aprops, ctx), defns) -> do
          let cdefns = M.mapKeys coerce $ runDSM (mapM desugarDefn defns)
          topDocs  .= docMap
          topCtx   .= ctx
          loadDefs cdefns

          t <- injectErrors InterpErr $ withTopEnv $ runAllTests aprops
          info $ "Loaded."
          return t

-- XXX comment
handleDocs :: Name Term -> Disco Err ()
handleDocs x = do
  ctx  <- use topCtx
  docs <- use topDocs
  case M.lookup x ctx of
    Nothing -> errR $ RSeq [ RTxt "No documentation found for "
                           , RName (AnyName x)
                           , RTxt "."
                           ]
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyTy ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _ -> return ()

------------------------------------------------------------
-- Running tests
------------------------------------------------------------

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: Ctx ATerm [AProperty] -> Disco IErr Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = disableMessageBuffering $ do
      info $ "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: Name ATerm -> [AProperty] -> Disco IErr Bool
    runTests n props = indentMessages $ do
      results <- sequenceA . fmap sequenceA $ map (id &&& runTest numSamples) props
      let failures = filter (not . testIsOK . snd) results
      case null failures of
        True  -> infoR $ RSeq [rName n, RTxt ": OK"]
        False -> do
          infoR $ RSeq [rName n, RTxt ":"]
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

     -- XXX Still figuring out how to report and format test results
     -- in the right way.  Something with collapsible messages?  In
     -- general we do want to support reports with interactively
     -- explorable levels of detail.  Maybe sub-reports can be somehow
     -- marked as "collapsed" or "expanded"?  Then instead of deleting
     -- messages we just put them in some structure so we know they
     -- have been dealt with but can still be referred to by the user.
     -- Then some sort of mechanism for expanding/re-showing certain
     -- messages.


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
      iputStr =<< (renderDoc . prettyName $ coerce x)
      iputStr (replicate (maxNameLen - length (name2String x)) ' ')
      iputStr " = "
      prettyValue (ctx ! coerce x) v

------------------------------------------------------------
-- Command-line options
------------------------------------------------------------

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
          , O.short 'c'
          , O.help "check a file without starting the interactive REPL"
          , O.metavar "FILE"
          ])
      )

discoInfo :: O.ParserInfo DiscoOpts
discoInfo = O.info (O.helper <*> discoOpts) $ mconcat
  [ O.fullDesc
  , O.progDesc "Command-line interface for Disco, a functional programming language for discrete mathematics."
  , O.header "disco v0.1"
  ]

------------------------------------------------------------
-- Main
------------------------------------------------------------

banner :: String
banner = "Welcome to Disco!\n\nA functional programming language for discrete mathematics.\n\n"

main :: IO ()
main = do
  opts <- O.execParser discoInfo

  let batch = any isJust [evaluate opts, cmdFile opts, checkFile opts]
      settings = defaultSettings
            { historyFile = Just ".disco_history" }
  when (not batch) $ putStr banner

  void $ runDisco $ do
    -- Note runDisco here returns a result of (Either void ()), so it
    -- must be Right (), so we can throw it away.

    -- At this point we have to pick the right mode based on the
    -- command-line flags:

    -- 1. Are we checking a file, i.e. making sure it loads with no
    -- type errors and all tests pass?  If so, load it, but then exit
    -- without starting the REPL, and with an exit code depending on
    -- the load results.
    case checkFile opts of
      Just file -> do
        res <- catchMessage formatErr $ handleLoad file
        printAndClearMessages
        io $ case res of
          Right True -> exitSuccess
          _          -> exitFailure

      Nothing   -> return ()

    -- 2. Are we executing a file as a sequence of REPL commands?
    case cmdFile opts of
      Just file -> do
        mcmds <- loadFile file
        case mcmds of
          Nothing -> return ()
          Just cmds -> mapM_ handleCommand (lines cmds)
      Nothing   -> return ()

    -- Are we evaluating a single expression/command?
    case evaluate opts of
      Just str -> handleCommand str
      Nothing  -> return ()

    -- Otherwise, start the REPL.
    when (not batch) $ runInputT settings loop

  where

    ctrlC :: InputT (Disco e) a -> SomeException -> InputT (Disco e) a
    ctrlC act e = do
      io $ putStrLn (show e)
      act

    -- Resume after a Ctrl-C interrupt; so Ctrl-C only stops
    -- evaluation of the current expression and drops us back to the
    -- disco prompt, rather than killing the whole process.
    withCtrlC resume act = H.catch (H.withInterrupt act) (ctrlC resume)

    -- The REPL proper.
    loop :: InputT (Disco void) ()
    loop = do
      minput <- withCtrlC (return $ Just "") (getInputLine "Disco> ")
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | otherwise -> do
              withCtrlC (return ()) $ (lift . handleCommand $ input)
              loop

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (use, (%=), (.=))
import           Control.Monad                    (when, forM_)
import           Control.Monad.Except             (catchError)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Trans.Class        (MonadTrans(..))
import           Data.Char                        (isSpace)
import           Data.Coerce
import           Data.List                        (find, isPrefixOf)
import qualified Data.Map                         as M
import           Data.Maybe                       (isJust)

import qualified Options.Applicative              as O
import           System.Console.Haskeline         as H
import           System.Exit
import           Text.Megaparsec                  hiding (runParser)
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Interpret.Core             (rnf, withDefs)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io i = liftIO i

iputStrLn :: MonadIO m => String -> m ()
iputStrLn = io . putStrLn

iprint :: (MonadIO m, Show a) => a -> m ()
iprint = io . print

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
 | Doc (Name Term)              -- Show documentation.
 | Nop                          -- No-op, e.g. if the user just enters a comment
 | Help
 deriving Show

letParser :: Parser REPLExpr
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: Parser REPLExpr
commandParser = (symbol ":" *> many lowerChar) >>= parseCommandArgs

parseCommandArgs :: String -> Parser REPLExpr
parseCommandArgs cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    parsers =
      [ ("type",    TypeCheck <$> term)
      , ("defn",    ShowDefn  <$> (whitespace *> ident))
      , ("parse",   Parse     <$> term)
      , ("pretty",  Pretty    <$> term)
      , ("desugar", Desugar   <$> term)
      , ("load",    Load      <$> fileParser)
      , ("doc",     Doc       <$> (whitespace *> ident))
      , ("help",    return Help)
      ]

fileParser :: Parser FilePath
fileParser = many spaceChar *> many (satisfy (not . isSpace))

lineParser :: Parser REPLExpr
lineParser
  =   commandParser
  <|> try (Nop <$ (consumeWhitespace <* eof))
  <|> try (Eval <$> (consumeWhitespace *> parseTerm <* eof))
  <|> letParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (runParser lineParser "" s) of
                Left err -> Left $ (parseErrorPretty err)
                Right l -> Right l

handleCMD :: String -> Disco ()
handleCMD "" = return ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l `catchError` (io . print  {- XXX pretty-print error -})
  where
    handleLine :: REPLExpr -> Disco ()

    handleLine (Let x t)     = handleLet x t
    handleLine (TypeCheck t) = handleTypeCheck t        >>= iputStrLn
    handleLine (Eval t)      = (evalTerm t)             >>= iputStrLn
    handleLine (ShowDefn x)  = handleShowDefn x         >>= iputStrLn
    handleLine (Parse t)     = iprint $ t
    handleLine (Pretty t)    = renderDoc (prettyTerm t) >>= iputStrLn
    handleLine (Desugar t)   = handleDesugar t          >>= iputStrLn
    handleLine (Load file)   = handleLoad file >> return ()
    handleLine (Doc x)       = handleDocs x
    handleLine Nop           = return ()
    handleLine Help          = iputStrLn "Help!"

handleLet :: Name Term -> Term -> Disco ()
handleLet x t = do
  ctx <- use topCtx
  let mat = runTCM (extends ctx $ infer t)
  case mat of
    Left err -> io.print $ err   -- XXX pretty print
    Right (at, _) -> do
      topCtx   %= M.insert x (getType at)
      topDefns %= M.insert (coerce x) (runDSM $ desugarTerm at)

handleShowDefn :: Name Term -> Disco String
handleShowDefn x = do
  defns <- use topDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleDesugar :: Term -> Disco String
handleDesugar t = do
  case evalTCM (infer t) of
    Left err -> return.show $ err
    Right at -> return.show.runDSM.desugarTerm $ at

loadFile :: FilePath -> Disco (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file


handleLoad :: FilePath -> Disco Bool
handleLoad file = do
  io . putStrLn $ "Loading " ++ file ++ "..."
  str <- io $ readFile file
  let mp = runParser wholeModule file str
  case mp of
    Left err -> io $ putStrLn (parseErrorPretty err) >> return False
    Right p  ->
      case runTCM (checkModule p) of
        Left tcErr         -> io $ print tcErr >> return False
        Right ((docMap, aprops, ctx), defns) -> do
          let cdefns = M.mapKeys coerce $ runDSM (mapM desugarDefn defns)
          topDefns .= cdefns
          topDocs  .= docMap
          topCtx   .= ctx
          t <- runAllTests aprops
          io . putStrLn $ "Loaded."
          return t

runAllTests :: Ctx ATerm [AProperty] -> Disco Bool
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      io $ putStrLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?
  where
    runTests :: Name ATerm -> [AProperty] -> Disco Bool
    runTests n props = do
      defns <- use topDefns
      io $ putStr ("  " ++ name2String n ++ ": ")
      results <- sequenceA . fmap sequenceA $ map (id &&& runTest defns) props
      let failures = filter (not . testIsOK . snd) results
      forM_ failures (uncurry prettyTestFailure)
      when (null failures) (io $ putStrLn "OK")
      return (null failures)

prettyTestFailure :: AProperty -> TestResult -> Disco ()
prettyTestFailure _ TestOK = return ()
prettyTestFailure prop TestFalse  = io $ print prop
prettyTestFailure prop (TestEqualityFailure v1 ty1 v2 ty2) = do
  io $ putStrLn ("While testing " ++ show prop)    -- XXX pretty-print
  io $ putStrLn ("  Expected: " ++ prettyValue ty2 v2)
  io $ putStrLn ("  But got:  " ++ prettyValue ty1 v1)

  -- XXX to pretty-print an 'AProperty' we probably want to erase it
  -- to a Property first and then pretty-print.  But to do that we
  -- probably want to unify the ASTs first.  Or, we can keep the
  -- original untypechecked Property around just so we can
  -- pretty-print it if there's an error.

handleDocs :: Name Term -> Disco ()
handleDocs x = do
  ctx  <- use topCtx
  docs <- use topDocs
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      p  <- renderDoc . hsep $ [prettyName x, text ":", prettyTy ty]
      io . putStrLn $ p
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _ -> return ()

evalTerm :: Term -> Disco String
evalTerm t = do
  ctx   <- use topCtx
  defns <- use topDefns
  case evalTCM (extends ctx $ infer t) of
    Left err -> return.show $ err    -- XXX pretty-print
    Right at ->
      let ty = getType at
          c  = runDSM $ desugarTerm at
      in prettyValue ty <$> (withDefs defns $ rnf c)

handleTypeCheck :: Term -> Disco String
handleTypeCheck t = do
  ctx <- use topCtx
  case (evalTCM $ extends ctx (infer t)) of
    Left err -> return.show $ err    -- XXX pretty-print
    Right at -> renderDoc $ prettyTerm t <+> text ":" <+> (prettyTy.getType $ at)

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
          Nothing -> return ()
          Just cmds -> mapM_ handleCMD (lines cmds)
      Nothing   -> return ()
    case evaluate opts of
      Just str -> handleCMD str
      Nothing  -> return ()

    when (not batch) $ runInputT settings loop

  case res of

    -- All disco exceptions should be caught and handled by this point.
    Left err -> do
      putStrLn $ "Uncaught error: " ++ show err
      putStrLn $ "Please report this as a bug: https://github.com/disco-lang/disco/issues"
    Right () -> return ()

  where

    ctrlC :: InputT Disco a -> SomeException -> InputT Disco a
    ctrlC act e = do
      io $ putStrLn (show e)
      act

    withCtrlC resume act = H.catch (H.withInterrupt act) (ctrlC resume)

    loop :: InputT Disco ()
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

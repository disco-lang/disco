{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (makeLenses, use, (%=), (.=))
import           Control.Monad.State
import           Data.Char                        (isSpace)
import           Data.Coerce
import           Data.List                        (find, isPrefixOf)
import qualified Data.Map                         as M
import           Data.Maybe                       (isJust)

import qualified Options.Applicative              as O
import           System.Console.Haskeline
import           System.Exit
import           Text.Megaparsec                  hiding (runParser)
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.AST.Core
import           Disco.Types
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval                       (runDisco)
import           Disco.Interpret.Core             (rnf, withDefs)
import           Disco.Parser
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck

data REPLState = REPLState
  { _replCtx   :: Ctx Term Type
  , _replDefns :: Ctx Core Core
  , _replDocs  :: DocMap
  }

makeLenses ''REPLState

initREPLState :: REPLState
initREPLState = REPLState M.empty M.empty M.empty

type REPLStateIO = StateT REPLState IO

-- HDE: Is there a way around this?
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

io :: IO a -> REPLStateIO a
io i = liftIO i

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

handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()

    handleLine (Let x t)     = handleLet x t
    handleLine (TypeCheck t) = handleTypeCheck t >>= (io.putStrLn)
    handleLine (Eval t)      = (evalTerm t) >>= (io.putStrLn)
    handleLine (ShowDefn x)  = handleShowDefn x >>= (io.putStrLn)
    handleLine (Parse t)     = io.print $ t
    handleLine (Pretty t)    = io.putStrLn $ renderDoc (prettyTerm t)
    handleLine (Desugar t)   = handleDesugar t >>= (io.putStrLn)
    handleLine (Load file)   = handleLoad file >> return ()
    handleLine (Doc x)       = handleDocs x
    handleLine Nop           = return ()
    handleLine Help          = io.putStrLn $ "Help!"

handleLet :: Name Term -> Term -> REPLStateIO ()
handleLet x t = do
  ctx <- use replCtx
  let mat = runTCM (extends ctx $ infer t)
  case mat of
    Left err -> io.print $ err   -- XXX pretty print
    Right (at, _) -> do
      replCtx   %= M.insert x (getType at)
      replDefns %= M.insert (coerce x) (runDSM $ desugarTerm at)

handleShowDefn :: Name Term -> REPLStateIO String
handleShowDefn x = do
  defns <- use replDefns
  case M.lookup (coerce x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleDesugar :: Term -> REPLStateIO String
handleDesugar t = do
  case evalTCM (infer t) of
    Left err -> return.show $ err
    Right at -> return.show.runDSM.desugarTerm $ at

handleLoad :: FilePath -> REPLStateIO Bool
handleLoad file = handle (\e -> fileNotFound file e >> return False) $ do
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
          replDefns .= cdefns
          replDocs  .= docMap
          replCtx   .= ctx
          t <- runAllTests aprops
          io . putStrLn $ "Loaded."
          return t

runAllTests :: M.Map (Name ATerm) [AProperty] -> REPLStateIO Bool
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      io $ putStrLn "Running tests..."
      and <$> mapM (uncurry runTestsIO) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?
  where
    runTestsIO :: Name ATerm -> [AProperty] -> REPLStateIO Bool
    runTestsIO n props = do
      defns <- use replDefns
      io $ putStr ("  " ++ name2String n ++ ": ")
      let results  = map (id &&& runTest defns) props
          failures = filter (not . testIsOK . snd) results
      forM_ failures (uncurry prettyTestFailure)
      when (null failures) (io $ putStrLn "OK")
      return (null failures)

prettyTestFailure :: AProperty -> TestResult -> REPLStateIO ()
prettyTestFailure _ TestOK = return ()
prettyTestFailure prop (TestRuntimeFailure interpErr) = io $ print (prop, interpErr)
prettyTestFailure prop TestFalse  = io $ print prop
prettyTestFailure prop (TestEqualityFailure v1 ty1 v2 ty2) = do
  io $ putStrLn ("While testing " ++ show prop)
  io $ putStrLn ("  Expected: " ++ prettyValue ty2 v2)
  io $ putStrLn ("  But got:  " ++ prettyValue ty1 v1)


handleDocs :: Name Term -> REPLStateIO ()
handleDocs x = do
  ctx  <- use replCtx
  docs <- use replDocs
  case M.lookup x ctx of
    Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
    Just ty -> do
      io . putStrLn $ show x ++ " : " ++ renderDoc (prettyTy ty)
      case M.lookup x docs of
        Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
        _ -> return ()

evalTerm :: Term -> REPLStateIO String
evalTerm t = do
  ctx   <- use replCtx
  defns <- use replDefns
  case evalTCM (extends ctx $ infer t) of
    Left err -> return.show $ err
    Right at ->
      let ty = getType at
          c  = runDSM $ desugarTerm at
      in case runDisco . withDefs defns $ rnf c of
           Left err -> return.show $ err
           Right v  -> return $ prettyValue ty v

handleTypeCheck :: Term -> REPLStateIO String
handleTypeCheck t = do
  ctx <- use replCtx
  case (evalTCM $ extends ctx (infer t)) of
    Left err -> return.show $ err
    Right at -> return . renderDoc $ prettyTerm t <+> text ":" <+> (prettyTy.getType $ at)

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

fileNotFound :: FilePath -> IOException -> REPLStateIO ()
fileNotFound file _ = io . putStrLn $ "File not found: " ++ file

main :: IO ()
main = do
  opts <- O.execParser discoInfo

  let batch = any isJust [evaluate opts, cmdFile opts, checkFile opts]
      settings = defaultSettings
            { historyFile = Just ".disco_history" }
  when (not batch) $ putStr banner
  flip evalStateT initREPLState $ do
    case checkFile opts of
      Just file -> do
        res <- handleLoad file
        io $ if res then exitSuccess else exitFailure
      Nothing   -> return ()
    case cmdFile opts of
      Just file -> handle (fileNotFound file) $ do
        cmds <- io $ readFile file
        mapM_ handleCMD (lines cmds)
      Nothing   -> return ()
    case evaluate opts of
      Just str -> handleCMD str
      Nothing  -> return ()

    when (not batch) $ runInputT settings loop

  where
    loop :: InputT REPLStateIO ()
    loop = do
      minput <- getInputLine "Disco> "
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | otherwise -> (lift.handleCMD $ input) >> loop

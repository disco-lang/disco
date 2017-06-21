{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Lens             (makeLenses, use, (%=), (.=))
import           Control.Monad.State
import           Data.Char                (isSpace)
import           Data.List                (find, isPrefixOf)
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)

import qualified Options.Applicative      as O
import           System.Console.Haskeline
import           Text.Megaparsec          hiding (runParser)
import           Unbound.LocallyNameless  hiding (rnf)

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Desugar
import           Disco.Interpret.Core     (Value (..), prettyValue, rnf, runIM')
import           Disco.Parser
import           Disco.Pretty
import           Disco.Typecheck

type CDefns = M.Map (Name Core) Core

data REPLState = REPLState
  { _replCtx   :: Ctx
  , _replDefns :: CDefns
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
    handleLine (Load file)   = handleLoad file
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
      replDefns %= M.insert (translate x) (runDSM $ desugarTerm at)

handleShowDefn :: Name Term -> REPLStateIO String
handleShowDefn x = do
  defns <- use replDefns
  case M.lookup (translate x) defns of
    Nothing -> return $ "No definition for " ++ show x
    Just d  -> return $ show d

handleDesugar :: Term -> REPLStateIO String
handleDesugar t = do
  case evalTCM (infer t) of
    Left err -> return.show $ err
    Right at -> return.show.runDSM.desugarTerm $ at

handleLoad :: FilePath -> REPLStateIO ()
handleLoad file = handle (fileNotFound file) $ do
  io . putStrLn $ "Loading " ++ file ++ "..."
  str <- io $ readFile file
  let mp = runParser wholeModule file str
  case mp of
    Left err -> io $ putStrLn (parseErrorPretty err)
    Right p  ->
      case runTCM (checkModule p) of
        Left tcErr         -> io $ print tcErr   -- XXX pretty-print
        Right ((docMap, aprops, ctx), defns) -> do
          let cdefns = M.mapKeys translate $ runDSM (mapM desugarDefn defns)
          replDefns .= cdefns
          replDocs  .= docMap
          replCtx   .= ctx
          runAllTests aprops
          io . putStrLn $ "Loaded."

runAllTests :: M.Map (Name ATerm) [AProperty] -> REPLStateIO ()
runAllTests aprops
  | M.null aprops = return ()
  | otherwise     = do
      io $ putStrLn "Running tests..."
      mapM_ runTests (M.assocs aprops)

runTests :: (Name ATerm, [AProperty]) -> REPLStateIO ()
runTests (n, props) =
  case props of
    [] -> return ()
    _  -> do
      io $ putStr ("  " ++ name2String n ++ ": ")
      bs <- mapM runTest props
      case and bs of
        True  -> io $ putStrLn "OK"
        False -> io $ putStrLn "One or more tests failed."
          -- XXX Get more informative test results.  If test fails
          -- pretty-print the expression that failed.  In addition, if
          -- test term looks like an equality test, report expected
          -- and actual values.

runTest :: AProperty -> REPLStateIO Bool
runTest aprop = do
  defns <- use replDefns
  let res = runIM' defns $ do
        lunbind aprop $ \(_binds, at) -> do
          rnf . runDSM $ desugarTerm at
  case res of
    Left err -> (io . print $ err) >> return False
    Right v  -> case v of
      VCons 1 [] -> return True
      _          -> return False

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
      in case runIM' defns (rnf c) of
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
  { evaluate :: Maybe String
  , cmdFile  :: Maybe String
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

discoInfo :: O.ParserInfo DiscoOpts
discoInfo = O.info (O.helper <*> discoOpts) $ mconcat
  [ O.fullDesc
  , O.progDesc "Command-line interface for Disco, a programming language for discrete mathematics."
  , O.header "disco v0.1"
  ]

fileNotFound :: FilePath -> IOException -> REPLStateIO ()
fileNotFound file _ = liftIO . putStrLn $ "File not found: " ++ file

main :: IO ()
main = do
  opts <- O.execParser discoInfo

  let batch = any isJust [evaluate opts, cmdFile opts]
      settings = defaultSettings
            { historyFile = Just ".disco_history" }
  when (not batch) $ putStr banner
  flip evalStateT initREPLState $ do
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

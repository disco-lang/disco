import           Control.Lens
import           Control.Monad.State
import           Data.List                               (find, isPrefixOf)
import qualified Data.Map                                as M
import           System.Console.Haskeline
import           System.Console.Haskeline.MonadException
import           System.Exit
import           Text.Parsec
import           Text.Parsec.String
import           Unbound.LocallyNameless                 hiding (rnf)
import           Unbound.LocallyNameless.Subst

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Desugar
import           Disco.Interpret.Core                    (prettyValue, rnf,
                                                          runIM')
import           Disco.Parser
import           Disco.Pretty
import           Disco.Typecheck

type CDefns = M.Map (Name Core) Core

type REPLStateIO = StateT (Ctx, CDefns) IO

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
 | ShowAST Term                 -- Show a terms AST
 | Parse Term                   -- Show the parsed AST
 | Desugar Term                 -- Show a desugared term
 | DumpState                    -- Trigger to dump the state for debugging.
 | Load FilePath                -- Load a file.
 | Help
 deriving Show

letParser :: Parser REPLExpr
letParser = Let
  <$> ident
  <*> (symbol "=" *> term)

commandParser :: Parser REPLExpr
commandParser = do
  symbol ":"
  ucmd <- many lower
  parseCommandArgs ucmd

parseCommandArgs :: String -> Parser REPLExpr
parseCommandArgs cmd = maybe badCmd snd $ find ((cmd `isPrefixOf`) . fst) parsers
  where
    badCmd = fail $ "Command \":" ++ cmd ++ "\" is unrecognized."
    parsers =
      [ ("type",    TypeCheck <$> term)
      , ("show",    ShowAST   <$> term)
      , ("parse",   Parse     <$> term)
      , ("desugar", Desugar   <$> term)
      , ("load",    Load      <$> fileParser)
      , ("help",    return Help)
      ]

fileParser :: Parser FilePath
fileParser = whiteSpace *> many anyChar

lineParser :: Parser REPLExpr
lineParser
  =   commandParser
  <|> try (Eval <$> (parseTerm <* eof))
  <|> letParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l

handleCMD :: String -> REPLStateIO ()
handleCMD "" = return ()
handleCMD s =
    case (parseLine s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l
  where
    handleLine :: REPLExpr -> REPLStateIO ()
    handleLine (Let x t) = handleLet x t
    handleLine (TypeCheck t) = (type_check t) >>= (io.putStrLn)
    handleLine (Eval t) = (eval t) >>= (io.putStrLn)
    handleLine (ShowAST t) = get >>= (\defs -> io.putStrLn.show $ t)
    handleLine (Parse t) = io.print $ t
    handleLine (Desugar t) = handleDesugar t >>= (io.putStrLn)
    handleLine (Load file) = handleLoad file
    handleLine Help = io.putStrLn $ "Help!"

handleLet :: Name Term -> Term -> REPLStateIO ()
handleLet x t = do
  (ctx, defns) <- get
  let mat = runTCM (extends ctx $ infer t)
  case mat of
    Left err -> io.print $ err   -- XXX pretty print
    Right (at, _) -> do
      _1 %= M.insert x (getType at)
      _2 %= M.insert (translate x) (runDSM $ desugar at)

handleDesugar :: Term -> REPLStateIO String
handleDesugar t = do
  case evalTCM (infer t) of
    Left err -> return.show $ err
    Right at -> return.show.runDSM.desugar $ at

handleLoad :: FilePath -> REPLStateIO ()
handleLoad file = do
  mp <- io (parseFile file)
  case mp of
    Left err -> io $ print err
    Right p  ->
      case runTCM (inferProg p) of
        Left tcErr         -> io $ print tcErr   -- XXX pretty-print
        Right (ctx, defns) -> do
          let cdefns = M.mapKeys translate $ runDSM (mapM desugarDefn defns)
          put (ctx, cdefns)

eval :: Term -> REPLStateIO String
eval t = do
  (ctx, defns) <- get
  case evalTCM (extends ctx $ infer t) of
    Left err -> return.show $ err
    Right at ->
      let ty = getType at
          c  = runDSM $ desugar at
      in case runIM' defns (rnf c) of
           Left err -> return.show $ err
           Right v  -> return $ prettyValue ty v

type_check :: Term -> REPLStateIO String
type_check t = do
  (ctx, _) <- get
  case (evalTCM $ extends ctx (infer t)) of
    Left err -> return.show $ err
    Right at -> return.renderDoc.prettyTy.getType $ at

banner :: String
banner = "Welcome to Disco!\n\nA language for programming discrete mathematics.\n\n"

main :: IO ()
main = do
  let settings = defaultSettings
        { historyFile = Just ".disco_history" }
  putStr banner
  evalStateT (runInputT settings loop) (M.empty, M.empty)
   where
       loop :: InputT REPLStateIO ()
       loop = do
           minput <- getInputLine "Disco> "
           case minput of
               Nothing -> return ()
               Just input | input `isPrefixOf` ":quit" -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop

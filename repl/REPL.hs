import           Control.Monad.State
import           Data.List                               (find, isPrefixOf)
import           System.Console.Haskeline
import           System.Console.Haskeline.MonadException
import           System.Exit
import           Text.Parsec
import           Text.Parsec.String
import           Unbound.LocallyNameless                 hiding (rnf)
import           Unbound.LocallyNameless.Subst

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Desugar
import           Disco.Interpret.Core
import           Disco.Parser
import           Disco.Pretty
import           Disco.Typecheck

import           Queue

type Qelm = (Name Term, Term)
type REPLStateIO = StateT (Queue Qelm) IO

-- HDE: Is there a way around this?
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

io :: IO a -> REPLStateIO a
io i = liftIO i

pop :: REPLStateIO (Name Term, Term)
pop = get >>= return.headQ

push :: Qelm -> REPLStateIO ()
push t = get >>= put.(`snoc` t)

unfoldDefsInTerm :: (Queue Qelm) -> Term -> Term
unfoldDefsInTerm q t =
    let uq = toListQ $ unfoldQueue q
     in substs uq t

unfoldQueue :: (Queue Qelm) -> (Queue Qelm)
unfoldQueue q = fixQ q emptyQ step
 where
   step e@(x,t) _ r = (mapQ (substDef x t) r) `snoc` e
    where
      substDef :: Name Term -> Term -> Qelm -> Qelm
      substDef x t (y, t') = (y, subst x t t')

------------------------------------------------------------------------
-- Parsers for the REPL                                               --
------------------------------------------------------------------------

data REPLExpr =
   Let (Name Term) Term         -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | Eval Term                    -- Evaluate a term
 | ShowAST Term                 -- Show a terms AST
 | Unfold Term                  -- Unfold the definitions in a term for debugging.
 | Parse Term                   -- Show the parsed AST
 | Desugar Term                 -- Show a desugared term
 | DumpState                    -- Trigger to dump the state for debugging.
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
      , ("unfold",  Unfold    <$> term)
      , ("dump",    return DumpState)
      , ("parse",   Parse     <$> term)
      , ("desugar", Desugar   <$> term)
      , ("help",    return Help)
      ]

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
    handleLine (Let x t) = push (x , t)
    handleLine (TypeCheck t) = (type_check t) >>= (io.putStrLn)
    handleLine (Eval t) = (eval t) >>= (io.putStrLn)
    handleLine (ShowAST t) = get >>= (\defs -> io.putStrLn.show $ unfoldDefsInTerm defs t)
    handleLine (Unfold t) = get >>= (\defs -> io.putStrLn.renderDoc.prettyTerm $ unfoldDefsInTerm defs t)
    handleLine DumpState = get >>= io.print.(mapQ prettyDef)
     where
       prettyDef (x, t) = (name2String x) ++ " = " ++ (renderDoc.prettyTerm $ t)
    handleLine (Parse t) = io.print $ t
    handleLine (Desugar t) = handleDesugar t >>= (io.putStrLn)
    handleLine Help = io.putStrLn $ "Help!"

handleDesugar :: Term -> REPLStateIO String
handleDesugar t = do
  case evalTCM (infer t) of
    Left err -> return.show $ err
    Right at -> return.show.runDSM.desugar $ at

eval :: Term -> REPLStateIO String
eval t = do
  defs <- get
  let tu = unfoldDefsInTerm defs t
   in case evalTCM (infer tu) of
        Left err -> return.show $ err
        Right at ->
            let ty = getType at
                c  = runDSM $ desugar at
             in case runIM (rnf c) of
                  Left err -> return.show $ err
                  Right v  -> return $ prettyValue ty v

type_check :: Term -> REPLStateIO String
type_check t = do
  defs <- get
  let tu = unfoldDefsInTerm defs t
   in case (evalTCM.infer $ tu) of
        Left err -> return.show $ err
        Right at -> return.renderDoc.prettyTy.getType $ at

banner :: String
banner = "Welcome to Disco!\n\nA language for programming discrete mathematics.\n\n"

main :: IO ()
main = do
  let settings = defaultSettings
        { historyFile = Just ".disco_history" }
  putStr banner
  evalStateT (runInputT settings loop) emptyQ
   where
       loop :: InputT REPLStateIO ()
       loop = do
           minput <- getInputLine "Disco> "
           case minput of
               Nothing -> return ()
               Just input | input `isPrefixOf` ":quit" -> liftIO $ putStrLn "Goodbye!" >> return ()
                          | otherwise -> (lift.handleCMD $ input) >> loop

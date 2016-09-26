module REPL where

import           Data.List                (isPrefixOf)

import           Text.Parsec

import           Desugar
import           InterpD
import           Parser
import           Typecheck
import           Types

import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just s | s `isPrefixOf` ":quit" -> return ()
        Just input   -> do
          outputStrLn $ eval input
          loop

eval :: String -> String
eval s =
  case parse (whiteSpace *> parseTerm <* eof) "" s of
    Left err -> show err
    Right t  -> case evalTCM (infer t) of
      Left err -> show err
      Right at ->
        let ty = getType at
            c  = runDSM $ desugar at
        in case runIM (rnf c) of
             Left err -> show err
             Right v  -> prettyValue ty v

import Parsing2

import Control.Monad.Except
import Data.Bifunctor
import Data.List (isPrefixOf)

import System.Console.Haskeline

import Unbound.Generics.LocallyNameless

import Text.Parsec.Error (ParseError)

import Sub2
import Types
import Constraints
import Unify
import Solve

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> putStrLn . pretty $ interp e

tcIO :: String -> IO ()
tcIO = either print (putStrLn . pretty) . tc

data Error =
  P ParseError | T TypeError
  deriving Show

(<!>) = flip first

tc :: String -> Either Error Sigma
tc s = do
  e <- parse expr s <!> P
  inferTopLevel e <!> T

main :: IO ()
main = do
  let settings = defaultSettings
        { historyFile = Just ".sub2_history" }
  runInputT settings loop

  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input
          | ":q" `isPrefixOf` input && input `isPrefixOf` ":quit" -> do
              liftIO $ putStrLn "Goodbye!"
              return ()
          | otherwise -> do
              case parse expr input of
                Left err -> liftIO (print err)
                Right e  -> do
                  liftIO (putStr $ pretty e)
                  case inferTopLevel e of
                    Left err -> liftIO (putStrLn "" >> print err)
                    Right ty -> do
                      liftIO (putStrLn $ " : " ++ pretty ty)
                      liftIO (putStrLn $ pretty (interp e))
              loop

{-
Example: (^f.f 3 + f (-5))  generates the type:

(a9 -> a10) -> a3

and the constraint graph:

1:a9  <---------
 |              \
5:Int <--  3:a11 |
 |          |    /
0:a3       4:Nat
 |
2:a10

which makes sense.  a9 is the input type of f, which must be a
supertype of both Nat and Int (because f is given 3 and (-5) as
inputs).  a10 (the output type of f) and a3 (the output type of the
whole function) must be numeric, i.e. <= Int.  In addition a10 <= a3
since whatever type f outputs has to be convertible to the output of
the whole function.

Obviously a9 has to be Int, and a11 doesn't matter (since it doesn't
show up in the output type).  But what should a3 and a10 be?  I hope
that in the real system the type of +, -, etc. will not involve a
subtyping constraint but instead a qualifier like num, sub etc., so we
can infer a qualified type.

What type do we want this to have ideally?

In Haskell it has the type

forall t a. (Num t, Num a) => (t -> a) -> a.

We would like it to have a type like

forall sub t, forall num a, (t -> a) -> a

I think.

-}

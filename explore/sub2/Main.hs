import Parsing2

import Sub2

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> print $ interp e

tc :: String -> IO ()
tc s = case parse expr s of
  Left err -> print err
  Right e -> print $ inferAndUnify e

main :: IO ()
main = getLine >>= tc

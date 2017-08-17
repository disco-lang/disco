import Parsing2

import Sub

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> print $ interp e

main :: IO ()
main = getLine >>= eval

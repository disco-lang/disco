import Parsing2

import Sub2

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> print $ interp e

tc :: String -> IO ()
tc s = case parse expr s of
  Left err -> print err
  Right e -> do
    case runTC (infer e) of
      Left err -> print err
      Right (ty, cs) -> do
        print ty
        case simplify cs of
          Left err -> print err
          Right (atoms, sub) -> do
            print (atoms, sub)
            print (mkConstraintGraph atoms)

main :: IO ()
main = getLine >>= tc

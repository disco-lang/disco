module Play where

thing :: (Int, Bool) -> ()
thing (n, True) = ()
thing (0, n) = ()

triple :: (Int, Int, Int) -> ()
triple (7, 5, 3) = ()

triple2 :: (Int, (Int, Int)) -> ()
triple2 (7, (5, 3)) = ()

foo :: (Either Int Bool, Int) -> Bool
foo (Left 1, 2) = True
foo (Right False, n) = True
foo (Right True, n) = True
foo (Left 3, n) = True
foo (Left 3, n) = True

foo2 :: (Either Int Bool, Int) -> Bool
foo2 (Left 1, n) = True
foo2 (Right False, n) = True

foo3 :: (Either Bool Bool, Bool) -> Bool
foo3 (Left True, b) = True
foo3 (Right False, b) = True

foo4 :: (Int, Int) -> Bool
foo4 (1, n) = True
foo4 (n, 2) = False

foo5 :: (Int, Int, Int) -> Bool
foo5 (1, 2, _) = False
foo5 (4, _, 6) = True
foo5 (_, 8, 9) = False

foo6 :: (Int, Int) -> Bool
foo6 (1, 2) = False

foo7 :: (Int, Int, Int) -> Bool
foo7 (1, 2, 3) = False

foo8 :: (Either Int Bool, Int) -> ()
foo8 (Left 10, 2) = ()

-- foo8 (Right True, 5) = ()

data Pat where
  Base :: Pat
  Kon :: Pat -> Pat
  DonKon :: Pat -> Pat -> Pat
  deriving (Show, Eq)

timelineSplitter :: Int -> [Pat]
timelineSplitter p = do
  case p of
    0 -> return Base
    2 -> [Base, Base]
    4 -> do
      a <- timelineSplitter (p - 1)
      b <- timelineSplitter (p - 2)

      return $ DonKon a b
    _ -> do
      n <- timelineSplitter (p - 1)
      return $ Kon n

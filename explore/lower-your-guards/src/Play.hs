module Play where

foo :: (Either Int Bool, Int) -> Bool
foo (Left  1     , 2) = True
foo (Right False , n) = True
foo (Right True  , n) = True
foo (Left  3     , n) = True
foo (Left  3     , n) = True

foo2 :: (Either Int Bool, Int) -> Bool
foo2 (Left  1    , n) = True
foo2 (Right False , n) = True

foo3 :: (Either Bool Bool, Bool) -> Bool
foo3 (Left  True    , b) = True
foo3 (Right False   , b) = True

foo4 :: (Int, Int) -> Bool
foo4 (1, n) = True
foo4 (n, 2) = False

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

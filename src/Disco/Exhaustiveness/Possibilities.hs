module Disco.Exhaustiveness.Possibilities (Possibilities, retSingle, allCombinations, anyOf, none, getPossibilities) where

import Data.Foldable (Foldable (fold))

newtype Possibilities a = Possibilities {getPossibilities :: [a]}
  deriving (Show, Eq, Ord)

instance Functor Possibilities where
  fmap f (Possibilities a) = Possibilities (f <$> a)

instance Semigroup (Possibilities a) where
  (Possibilities p1) <> (Possibilities p2) = Possibilities $ p1 <> p2

instance Monoid (Possibilities a) where
  mempty = Possibilities []

anyOf :: [Possibilities a] -> Possibilities a
anyOf = fold

none :: Possibilities a -> Bool
none = null . getPossibilities

retSingle :: (Monad m) => a -> m (Possibilities a)
retSingle i = return $ Possibilities [i]

-- | List all possible paths through the list of Possibilites
--
--   Ex.
--   > allCombinations
--       [ Possibilities [1]
--       , Possibilities [2,3]
--       , Possibilities [4]
--       , Possibilities [5,6,7]
--       ]
--   ===
--   Possibilities {getPossibilities =
--     [[1,2,4,5]
--     ,[1,2,4,6]
--     ,[1,2,4,7]
--     ,[1,3,4,5]
--     ,[1,3,4,6]
--     ,[1,3,4,7]
--     ]}
--
--         2       5
--       /   \   /
--     1       4 --6
--       \   /   \
--         3       7
--
--   If any any of the Possibilities is empty,
--   an empty Possibility is returned
--
--   In other words, this lists all elements of the
--   cartesian product of multiple sets
allCombinations :: [Possibilities a] -> Possibilities [a]
allCombinations = foldr prod nil
  where
    -- note, nil /= mempty
    -- VERY important
    nil = Possibilities [[]]

prod :: Possibilities a -> Possibilities [a] -> Possibilities [a]
prod (Possibilities xs) (Possibilities yss) = Possibilities [x : ys | x <- xs, ys <- yss]

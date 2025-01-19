{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Disco.Exhaustiveness.Possibilities
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Meaningful types and functions for describing
-- combinations of different possible options.
module Disco.Exhaustiveness.Possibilities (Possibilities, retSingle, allCombinations, anyOf, none, getPossibilities) where

import Data.Foldable (fold)

newtype Possibilities a = Possibilities {getPossibilities :: [a]}
  deriving (Show, Eq, Ord, Functor, Semigroup, Monoid, Applicative)

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
allCombinations = sequenceA

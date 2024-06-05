{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      :  Disco.Util
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Miscellaneous utilities.
module Disco.Util where

import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M

infixr 1 ==>

-- | A synonym for pairing which makes convenient syntax for
--   constructing literal maps via M.fromList.
(==>) :: a -> b -> (a, b)
(==>) = (,)

-- | Flipped variant of 'map'.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | A variant of 'Map' indexing that throws a custom error message
--   in case the key is not found, to help with debugging.
(!) :: (Show k, Ord k) => M.Map k v -> k -> v
m ! k = case M.lookup k m of
  Nothing -> error $ "key " ++ show k ++ " is not an element in the map"
  Just v -> v

-- | Find the maximum of a list of positive numbers, yielding 0 in the
--   case of an empty list.
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 [] = 0
maximum0 xs = maximum xs

-- | A variant of 'filter' that returns a @Maybe (NonEmpty a)@ instead
--   of a regular list.
filterNE :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
filterNE p = NE.nonEmpty . NE.filter p

-- | A variant of 'partition' that returns @Maybe (NonEmpty a)@s instead
--   of regular lists.
partitionNE :: (a -> Bool) -> NonEmpty a -> (Maybe (NonEmpty a), Maybe (NonEmpty a))
partitionNE p as = (filterNE p as, filterNE (not . p) as)

-- | A variant of 'partitionEithers' for nonempty lists.  If the
--   result is Left, it means all the inputs were Left.  If the result
--   is Right, we definitely have some Rights, and possibly some Lefts
--   as well.  This properly encodes the fact that at least one result
--   list must be nonempty.
partitionEithersNE :: NonEmpty (Either a b) -> Either (NonEmpty a) ([a], NonEmpty b)
partitionEithersNE = foldr1 combine . NE.map (bimap NE.singleton (([],) . NE.singleton))
 where
  combine :: Either (NonEmpty a) ([a], NonEmpty b) -> Either (NonEmpty a) ([a], NonEmpty b) -> Either (NonEmpty a) ([a], NonEmpty b)
  combine (Left as1) (Left as2) = Left (NE.append as1 as2)
  combine (Left as1) (Right (as2, bs)) = Right (NE.toList as1 ++ as2, bs)
  combine (Right (as1, bs)) (Left as2) = Right (as1 ++ NE.toList as2, bs)
  combine (Right (as1, bs1)) (Right (as2, bs2)) = Right (as1 ++ as2, NE.append bs1 bs2)

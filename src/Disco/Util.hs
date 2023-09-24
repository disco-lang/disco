-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Disco.Util
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Miscellaneous utilities.
module Disco.Util where

import qualified Data.Map as M

infixr 1 ==>

-- | A synonym for pairing which makes convenient syntax for
--   constructing literal maps via M.fromList.
(==>) :: a -> b -> (a, b)
(==>) = (,)

for :: [a] -> (a -> b) -> [b]
for = flip map

(!) :: (Show k, Ord k) => M.Map k v -> k -> v
m ! k = case M.lookup k m of
  Nothing -> error $ "key " ++ show k ++ " is not an element in the map"
  Just v -> v

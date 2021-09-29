-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Extensions
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Optional extensions to the disco language.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Extensions
  ( Ext(..), ExtSet, defaultExts, allExts, allExtsList, addExtension
  )
  where

import           Data.Set (Set)
import qualified Data.Set as S

type ExtSet = Set Ext

-- | Enumeration of optional language extensions.
data Ext
  = Primitives   -- ^ Allow primitives, i.e. @$prim@
  | PercentNames -- ^ Allow the @%@ char to be used in identifier names
  | Randomness   -- ^ Allow randomness.  This is not implemented yet.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The default set of language extensions (currently, the empty set).
defaultExts :: ExtSet
defaultExts = S.empty

-- | A set of all possible language extensions, provided for convenience.
allExts :: ExtSet
allExts = S.fromList allExtsList

-- | All possible language extensions in the form of a list.
allExtsList :: [Ext]
allExtsList = [minBound .. maxBound]

-- | Add an extension to an extension set.
addExtension :: Ext -> ExtSet -> ExtSet
addExtension = S.insert

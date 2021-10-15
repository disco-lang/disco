-- |
-- Module      :  Disco.Extensions
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Optional extensions to the disco language.
module Disco.Extensions
  ( Ext (..),
    ExtSet,
    defaultExts,
    allExts,
    allExtsList,
    addExtension,
  )
where

import Data.Set (Set)
import qualified Data.Set as S

type ExtSet = Set Ext

-- | Enumeration of optional language extensions.
data Ext
  = -- | Allow primitives, i.e. @$prim@
    Primitives
  | -- | Allow the @%@ char to be used in identifier names
    PercentNames
  | -- | Don't automatically import standard library modules
    NoStdLib
  | -- | Allow randomness.  This is not implemented yet.
    Randomness
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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Syntax.Prims
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Concrete syntax for the prims supported by the language.
--
-----------------------------------------------------------------------------

module Disco.Syntax.Prims
       ( Prim(..)
       , PrimInfo(..), primTable, primMap
       ) where

import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Data.Map                         (Map)
import qualified Data.Map                         as M

------------------------------------------------------------
-- Prims
------------------------------------------------------------

-- | Primitives, /i.e./ identifiers which are interpreted specially at
--   runtime.
data Prim where
  PrimList    :: Prim      -- ^ Container -> list conversion
  PrimBag     :: Prim      -- ^ Container -> bag conversion
  PrimSet     :: Prim      -- ^ Container -> set conversion

  PrimB2C     :: Prim      -- ^ bag -> set of counts conversion
  PrimC2B     :: Prim      -- ^ set of counts -> bag conversion

  PrimMap     :: Prim      -- ^ Map operation for containers
  PrimReduce  :: Prim      -- ^ Reduce operation for containers
  PrimFilter  :: Prim      -- ^ Filter operation for containers
  PrimJoin    :: Prim      -- ^ Monadic join for containers
  PrimMerge   :: Prim      -- ^ Generic merge operation for bags/sets

  PrimIsPrime :: Prim      -- ^ Efficient primality test
  PrimFactor  :: Prim      -- ^ Factorization

  PrimCrash   :: Prim      -- ^ Crash

  PrimForever :: Prim      -- ^ @[x, y, z .. ]@
  PrimUntil   :: Prim      -- ^ @[x, y, z .. e]@
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

instance Alpha Prim
instance Subst t Prim

------------------------------------------------------------
-- Concrete syntax for prims
------------------------------------------------------------

-- | An info record for a single primitive name, containing the
--   primitive itself, its concrete syntax, and whether it is
--   "exposed", /i.e./ available to be used in the surface syntax of
--   the basic language.  Unexposed prims can only be referenced by
--   enabling the Primitives language extension and prefixing their
--   name by @$@.
data PrimInfo =
  PrimInfo
  { thePrim     :: Prim
  , primSyntax  :: String
  , primExposed :: Bool
    -- Is the prim available in the normal syntax of the language?
    --
    --   primExposed = True means that the bare primSyntax can be used
    --   in the surface syntax, and in particular it is a reserved
    --   word; the prim will be pretty-printed as the primSyntax.
    --
    --   primExposed = False means that the only way to enter it is to
    --   enable the Primitives language extension and write a $
    --   followed by the primSyntax, and the primSyntax is not a
    --   reserved word.  The prim will be pretty-printed with a $
    --   prefix.
  }

-- | A table containing a 'PrimInfo' record for every 'Prim'
--   recognized by the language.
primTable :: [PrimInfo]
primTable =
  [ PrimInfo PrimList      "list"    True
  , PrimInfo PrimBag       "bag"     True
  , PrimInfo PrimSet       "set"     True

  , PrimInfo PrimB2C       "bagCounts" True
  , PrimInfo PrimC2B       "bagFromCounts" True

  , PrimInfo PrimMap       "map"     True
  , PrimInfo PrimReduce    "reduce"  True
  , PrimInfo PrimFilter    "filter"  True
  , PrimInfo PrimJoin      "join"    False
  , PrimInfo PrimMerge     "merge"   False

  , PrimInfo PrimIsPrime   "isPrime" False
  , PrimInfo PrimFactor    "factor"  False

  , PrimInfo PrimCrash     "crash"   False

  , PrimInfo PrimForever   "forever" False
  , PrimInfo PrimUntil     "until"   False
  ]

-- | A convenient map from each 'Prim' to its info record.
primMap :: Map Prim PrimInfo
primMap = M.fromList $
  [ (p, pinfo) | pinfo@(PrimInfo p _ _) <- primTable ]

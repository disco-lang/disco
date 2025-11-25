{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Syntax.Prims
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Concrete syntax for the prims (i.e. built-in constants) supported
-- by the language.
module Disco.Syntax.Prims (
  Prim (..),
  PrimInfo (..),
  primTable,
  toPrim,
  primMap,
) where

import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Data.Map (Map)
import qualified Data.Map as M

import Data.Data (Data)
import Disco.Syntax.Operators

------------------------------------------------------------
-- Prims
------------------------------------------------------------

-- | Primitives, /i.e./ built-in constants.
data Prim where
  -- | Unary operator
  PrimUOp :: UOp -> Prim
  -- | Binary operator
  PrimBOp :: BOp -> Prim
  -- | Left injection into a sum type.
  PrimLeft :: Prim
  -- | Right injection into a sum type.
  PrimRight :: Prim
  -- | Integer square root (@sqrt@)
  PrimSqrt :: Prim
  -- | Floor of fractional type (@floor@)
  PrimFloor :: Prim
  -- | Ceiling of fractional type (@ceiling@)
  PrimCeil :: Prim
  -- | Absolute value (@abs@)
  PrimAbs :: Prim
  -- | Min
  PrimMin :: Prim
  -- | Max
  PrimMax :: Prim
  -- | Power set (XXX or bag?)
  PrimPower :: Prim
  -- | Container -> list conversion
  PrimList :: Prim
  -- | Container -> bag conversion
  PrimBag :: Prim
  -- | Container -> set conversion
  PrimSet :: Prim
  -- | bag -> set of counts conversion
  PrimB2C :: Prim
  -- | set of counts -> bag conversion
  PrimC2B :: Prim
  -- | unsafe set of counts -> bag conversion
  --   that assumes all distinct
  PrimUC2B :: Prim
  -- | Map k v -> Set (k × v)
  PrimMapToSet :: Prim
  -- | Set (k × v) -> Map k v
  PrimSetToMap :: Prim
  -- | Get Adjacency list of Graph
  PrimSummary :: Prim
  -- | Construct a graph Vertex
  PrimVertex :: Prim
  -- | Empty graph
  PrimEmptyGraph :: Prim
  -- | Overlay two Graphs
  PrimOverlay :: Prim
  -- | Connect Graph to another with directed edges
  PrimConnect :: Prim
  -- | Insert into map
  PrimInsert :: Prim
  -- | Get value associated with key in map
  PrimLookup :: Prim
  -- | Each operation for containers
  PrimEach :: Prim
  -- | Reduce operation for containers
  PrimReduce :: Prim
  -- | Filter operation for containers
  PrimFilter :: Prim
  -- | Monadic join for containers
  PrimJoin :: Prim
  -- | Generic merge operation for bags/sets
  PrimMerge :: Prim
  -- | Efficient primality test
  PrimIsPrime :: Prim
  -- | Factorization
  PrimFactor :: Prim
  -- | Turn a rational into a pair (num, denom)
  PrimFrac :: Prim
  -- | Crash
  PrimCrash :: Prim
  -- | @[x, y, z .. e]@
  PrimUntil :: Prim
  -- | Test whether a proposition holds
  PrimHolds :: Prim
  -- | Generates a pseudorandom number generator
  PrimSeed :: Prim
  -- | Given a range and a generator, generates random number
  PrimRandom :: Prim
  deriving (Show, Read, Eq, Ord, Generic, Alpha, Subst t, Data)

------------------------------------------------------------
-- Concrete syntax for prims
------------------------------------------------------------

-- | An info record for a single primitive name, containing the
--   primitive itself, its concrete syntax, and whether it is
--   "exposed", /i.e./ available to be used in the surface syntax of
--   the basic language.  Unexposed prims can only be referenced by
--   enabling the Primitives language extension and prefixing their
--   name by @$@.
data PrimInfo = PrimInfo
  { thePrim :: Prim
  , primSyntax :: String
  , primExposed :: Bool
  -- Is the prim available in the normal syntax of the language?
  --
  --   primExposed = True means that the bare primSyntax can be used
  --   in the surface syntax, and the prim will be pretty-printed as
  --   the primSyntax.
  --
  --   primExposed = False means that the only way to enter it is to
  --   enable the Primitives language extension and write a $
  --   followed by the primSyntax.  The prim will be pretty-printed with a $
  --   prefix.
  --
  --   In no case is a prim a reserved word.
  }

-- | A table containing a 'PrimInfo' record for every non-operator
--   'Prim' recognized by the language.
primTable :: [PrimInfo]
primTable =
  [ PrimInfo PrimLeft "left" True
  , PrimInfo PrimRight "right" True
  , PrimInfo (PrimUOp Not) "not" True
  , PrimInfo PrimSqrt "sqrt" True
  , PrimInfo PrimFloor "floor" True
  , PrimInfo PrimCeil "ceiling" True
  , PrimInfo PrimAbs "abs" True
  , PrimInfo PrimMin "min" True
  , PrimInfo PrimMax "max" True
  , PrimInfo PrimPower "power" True
  , PrimInfo PrimList "list" True
  , PrimInfo PrimBag "bag" True
  , PrimInfo PrimSet "set" True
  , PrimInfo PrimB2C "bagCounts" True
  , PrimInfo PrimC2B "bagFromCounts" True
  , PrimInfo PrimUC2B "unsafeBagFromCounts" False
  , PrimInfo PrimMapToSet "mapToSet" True
  , PrimInfo PrimSetToMap "map" True
  , PrimInfo PrimSummary "summary" True
  , PrimInfo PrimVertex "vertex" True
  , PrimInfo PrimEmptyGraph "emptyGraph" True
  , PrimInfo PrimOverlay "overlay" True
  , PrimInfo PrimConnect "connect" True
  , PrimInfo PrimInsert "insert" True
  , PrimInfo PrimLookup "lookup" True
  , PrimInfo PrimEach "each" True
  , PrimInfo PrimReduce "reduce" True
  , PrimInfo PrimFilter "filter" True
  , PrimInfo PrimJoin "join" False
  , PrimInfo PrimMerge "merge" False
  , PrimInfo PrimIsPrime "isPrime" False
  , PrimInfo PrimFactor "factor" False
  , PrimInfo PrimFrac "frac" False
  , PrimInfo PrimCrash "crash" False
  , PrimInfo PrimUntil "until" False
  , PrimInfo PrimHolds "holds" True
  , PrimInfo PrimSeed "seed" True
  , PrimInfo PrimRandom "random" True
  ]

-- | Find any exposed prims with the given name.
toPrim :: String -> [Prim]
toPrim x = [p | PrimInfo p syn True <- primTable, syn == x]

-- | A convenient map from each 'Prim' to its info record.
primMap :: Map Prim PrimInfo
primMap =
  M.fromList $
    [(p, pinfo) | pinfo@(PrimInfo p _ _) <- primTable]

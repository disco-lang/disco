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
  PrimUOp ::
    UOp ->
    -- | Unary operator
    Prim
  PrimBOp ::
    BOp ->
    -- | Binary operator
    Prim
  PrimLeft ::
    -- | Left injection into a sum type.
    Prim
  PrimRight ::
    -- | Right injection into a sum type.
    Prim
  PrimSqrt ::
    -- | Integer square root (@sqrt@)
    Prim
  PrimFloor ::
    -- | Floor of fractional type (@floor@)
    Prim
  PrimCeil ::
    -- | Ceiling of fractional type (@ceiling@)
    Prim
  PrimAbs ::
    -- | Absolute value (@abs@)
    Prim
  PrimPower ::
    -- | Power set (XXX or bag?)
    Prim
  PrimList ::
    -- | Container -> list conversion
    Prim
  PrimBag ::
    -- | Container -> bag conversion
    Prim
  PrimSet ::
    -- | Container -> set conversion
    Prim
  PrimB2C ::
    -- | bag -> set of counts conversion
    Prim
  PrimC2B ::
    -- | set of counts -> bag conversion
    Prim
  PrimUC2B ::
    -- | unsafe set of counts -> bag conversion
    --   that assumes all distinct
    Prim
  PrimMapToSet ::
    -- | Map k v -> Set (k × v)
    Prim
  PrimSetToMap ::
    -- | Set (k × v) -> Map k v
    Prim
  PrimSummary ::
    -- | Get Adjacency list of Graph
    Prim
  PrimVertex ::
    -- | Construct a graph Vertex
    Prim
  PrimEmptyGraph ::
    -- | Empty graph
    Prim
  PrimOverlay ::
    -- | Overlay two Graphs
    Prim
  PrimConnect ::
    -- | Connect Graph to another with directed edges
    Prim
  PrimInsert ::
    -- | Insert into map
    Prim
  PrimLookup ::
    -- | Get value associated with key in map
    Prim
  PrimEach ::
    -- | Each operation for containers
    Prim
  PrimReduce ::
    -- | Reduce operation for containers
    Prim
  PrimFilter ::
    -- | Filter operation for containers
    Prim
  PrimJoin ::
    -- | Monadic join for containers
    Prim
  PrimMerge ::
    -- | Generic merge operation for bags/sets
    Prim
  PrimIsPrime ::
    -- | Efficient primality test
    Prim
  PrimFactor ::
    -- | Factorization
    Prim
  PrimFrac ::
    -- | Turn a rational into a pair (num, denom)
    Prim
  PrimCrash ::
    -- | Crash
    Prim
  PrimUntil ::
    -- | @[x, y, z .. e]@
    Prim
  PrimHolds ::
    -- | Test whether a proposition holds
    Prim
  PrimLookupSeq ::
    -- | Lookup OEIS sequence
    Prim
  PrimExtendSeq ::
    -- | Extend OEIS sequence
    Prim
  PrimSeed ::
    Prim 
  PrimRandom :: 
    Prim
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
  , PrimInfo PrimLookupSeq "lookupSequence" False
  , PrimInfo PrimExtendSeq "extendSequence" False
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

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Syntax.Prims
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Concrete syntax for the prims (i.e. built-in constants) supported
-- by the language.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Syntax.Prims
       ( Prim(..)
       , PrimInfo(..), primTable, primMap
       ) where

import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Data.Map                         (Map)
import qualified Data.Map                         as M

import           Disco.Syntax.Operators

------------------------------------------------------------
-- Prims
------------------------------------------------------------

-- | Primitives, /i.e./ built-in constants.
data Prim where
  PrimUOp        :: UOp -> Prim -- ^ Unary operator
  PrimBOp        :: BOp -> Prim -- ^ Binary operator

  PrimLeft       :: Prim        -- ^ Left injection into a sum type.
  PrimRight      :: Prim        -- ^ Right injection into a sum type.

  PrimSqrt       :: Prim        -- ^ Integer square root (@sqrt@)
  PrimFloor      :: Prim        -- ^ Floor of fractional type (@floor@)
  PrimCeil       :: Prim        -- ^ Ceiling of fractional type (@ceiling@)
  PrimAbs        :: Prim        -- ^ Absolute value (@abs@)

  PrimSize       :: Prim        -- ^ Size of a set (XXX should be in library)
  PrimPower      :: Prim        -- ^ Power set (XXX or bag?)

  PrimList       :: Prim        -- ^ Container -> list conversion
  PrimBag        :: Prim        -- ^ Container -> bag conversion
  PrimSet        :: Prim        -- ^ Container -> set conversion

  PrimB2C        :: Prim        -- ^ bag -> set of counts conversion
  PrimC2B        :: Prim        -- ^ set of counts -> bag conversion
  PrimMapToSet   :: Prim        -- ^ Map k v -> Set (k × v)
  PrimSetToMap   :: Prim        -- ^ Set (k × v) -> Map k v

  PrimSummary    :: Prim        -- ^ Get Adjacency list of Graph
  PrimVertex     :: Prim        -- ^ Construct a graph Vertex
  PrimEmptyGraph :: Prim        -- ^ Empty graph
  PrimOverlay    :: Prim        -- ^ Overlay two Graphs
  PrimConnect    :: Prim        -- ^ Connect Graph to another with directed edges

  PrimEmptyMap   :: Prim        -- ^ Empty Map
  PrimInsert     :: Prim        -- ^ Insert into map
  PrimLookup     :: Prim        -- ^ Get value associated with key in map

  PrimEach       :: Prim        -- ^ Each operation for containers
  PrimReduce     :: Prim        -- ^ Reduce operation for containers
  PrimFilter     :: Prim        -- ^ Filter operation for containers
  PrimJoin       :: Prim        -- ^ Monadic join for containers
  PrimMerge      :: Prim        -- ^ Generic merge operation for bags/sets

  PrimIsPrime    :: Prim        -- ^ Efficient primality test
  PrimFactor     :: Prim        -- ^ Factorization
  PrimFrac       :: Prim        -- ^ Turn a rational into a pair (num, denom)

  PrimCrash      :: Prim        -- ^ Crash

  PrimUntil      :: Prim        -- ^ @[x, y, z .. e]@

  PrimHolds      :: Prim        -- ^ Test whether a proposition holds

  PrimLookupSeq  :: Prim        -- ^ Lookup OEIS sequence
  PrimExtendSeq  :: Prim        -- ^ Extend OEIS sequence
  deriving (Show, Read, Eq, Ord, Generic)

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
  [ PrimInfo PrimLeft      "left"           True
  , PrimInfo PrimRight     "right"          True

  , PrimInfo (PrimUOp Not) "not"            True
  , PrimInfo PrimSqrt      "sqrt"           True
  , PrimInfo PrimFloor     "floor"          True
  , PrimInfo PrimCeil      "ceiling"        True
  , PrimInfo PrimAbs       "abs"            True

  , PrimInfo PrimSize      "size"           True
  , PrimInfo PrimPower     "power"          True

  , PrimInfo PrimList      "list"           True
  , PrimInfo PrimBag       "bag"            True
  , PrimInfo PrimSet       "set"            True

  , PrimInfo PrimB2C       "bagCounts"      True
  , PrimInfo PrimC2B       "bagFromCounts"  True
  , PrimInfo PrimMapToSet  "mapToSet"       True
  , PrimInfo PrimSetToMap  "map"            True

  , PrimInfo PrimSummary   "summary"        True
  , PrimInfo PrimVertex    "vertex"         True
  , PrimInfo PrimEmptyGraph "emptyGraph"     True
  , PrimInfo PrimOverlay   "overlay"        True
  , PrimInfo PrimConnect   "connect"        True

  , PrimInfo PrimEmptyMap  "emptyMap"       True
  , PrimInfo PrimInsert    "insert"         True
  , PrimInfo PrimLookup    "lookup"         True

  , PrimInfo PrimEach      "each"           True
  , PrimInfo PrimReduce    "reduce"         True
  , PrimInfo PrimFilter    "filter"         True
  , PrimInfo PrimJoin      "join"           False
  , PrimInfo PrimMerge     "merge"          False

  , PrimInfo PrimIsPrime   "isPrime"        False
  , PrimInfo PrimFactor    "factor"         False
  , PrimInfo PrimFrac      "frac"           False

  , PrimInfo PrimCrash     "crash"          False

  , PrimInfo PrimUntil     "until"          False

  , PrimInfo PrimHolds     "holds"          True

  , PrimInfo PrimLookupSeq "lookupSequence" False
  , PrimInfo PrimExtendSeq "extendSequence" False
  ]

-- | A convenient map from each 'Prim' to its info record.
primMap :: Map Prim PrimInfo
primMap = M.fromList $
  [ (p, pinfo) | pinfo@(PrimInfo p _ _) <- primTable ]

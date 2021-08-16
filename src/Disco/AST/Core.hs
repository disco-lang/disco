
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Core
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax trees representing the desugared, untyped core
-- language for Disco.
--
-----------------------------------------------------------------------------

module Disco.AST.Core
       ( -- * Core AST
         RationalDisplay(..)
       , Core(..)
       , Op(..), opArity

         -- * Case expressions and patterns
       , CBranch, CPattern(..)
       )
       where

import           GHC.Generics
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Generic                (Side)
import           Disco.AST.Surface                (Telescope)
import           Disco.Types

-- | A type of flags specifying whether to display a rational number
--   as a fraction or a decimal.
data RationalDisplay = Fraction | Decimal
  deriving (Eq, Show, Generic, Ord)

instance Semigroup RationalDisplay where
  Decimal <> _ = Decimal
  _ <> Decimal = Decimal
  _ <> _       = Fraction

-- | The 'Monoid' instance for 'RationalDisplay' corresponds to the
--   idea that the result should be displayed as a decimal if any
--   decimal literals are used in the input; otherwise, the default is
--   to display as a fraction.  So the identity element is 'Fraction',
--   and 'Decimal' always wins when combining.
instance Monoid RationalDisplay where
  mempty  = Fraction
  mappend = (<>)

-- | AST for the desugared, untyped core language.
data Core where

  -- | A variable.
  CVar   :: Name Core -> Core

  -- | A function constant.
  CConst :: Op -> Core

  -- | The unit value.
  CUnit :: Core

  -- | An injection into a sum type, i.e. a value together with a tag
  --   indicating which element of a sum type we are in.  For example,
  --   false is represented by @CSum L CUnit@; @right(v)@ is
  --   represented by @CSum R v@.  Note we do not need to remember
  --   which type the constructor came from; if the program
  --   typechecked then we will never end up comparing constructors
  --   from different types.
  CInj :: Side -> Core -> Core

  -- | A pair of values.
  CPair :: Core -> Core -> Core

  -- | A rational number.
  CNum  :: RationalDisplay -> Rational -> Core

  -- | An anonymous function.
  CAbs  :: Bind [Name Core] Core -> Core

  -- | Function application, where each argument has a strictness
  --   annotation.  The strictness is determined by the type of the
  --   application (which has been erased), and determines whether the
  --   argument should be evaluated before applying the function.
  CApp  :: Core -> [(Strictness, Core)] -> Core

  -- | A case expression.
  CCase :: [CBranch] -> Core

  -- | A "test frame" under which a test case is run. Records the
  --   types and legible names of the variables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core

  -- | A type.
  CType :: Type -> Core

  deriving (Show, Generic)

-- | Operators that can show up in the core language.  Note that not
--   all surface language operators show up here, since some are
--   desugared into combinators of the operators here.
data Op = OAdd      -- ^ Addition (@+@)
        | ONeg      -- ^ Arithmetic negation (@-@)
        | OSqrt     -- ^ Integer square root (@sqrt@)
        | OFloor    -- ^ Floor of fractional type (@floor@)
        | OCeil     -- ^ Ceiling of fractional type (@ceiling@)
        | OAbs      -- ^ Absolute value (@abs@)
        | OMul      -- ^ Multiplication (@*@)
        | ODiv      -- ^ Division (@/@)
        | OExp      -- ^ Exponentiation (@^@)
        | OMod      -- ^ Modulo (@mod@)
        | ODivides  -- ^ Divisibility test (@|@)
        | OMultinom -- ^ Multinomial coefficient (@choose@)
        | OFact     -- ^ Factorial (@!@)
        | OEq Type  -- ^ Equality test (@==@) at the given type.  At
                    --   this point, typechecking has determined that
                    --   the given type has decidable equality.  We
                    --   need to know the type in order to perform the
                    --   equality test.
        | OLt Type  -- ^ Less than (@<@).  Similarly, typechecking has
                    --   determined that the given type has a decidable
                    --   ordering relation.

        -- Type operators
        | OEnum     -- ^ Enumerate the values of a type.
        | OCount    -- ^ Count the values of a type.

        -- Arithmetic operators with special runtime behavior for finite types
        | OMDiv  Integer
        | OMExp  Integer
        | OMDivides Integer

        -- Container operations
        | OSize           -- ^ Size of two sets (@size@)
        | OPower Type     -- ^ Power set/bag of a given set/bag
                          --   (@power@). Carries the element type.
        | OBagElem Type   -- ^ Set/bag element test.
        | OListElem Type  -- ^ List element test.

        | OEachList       -- ^ Map a function over a list.
        | OEachBag Type   -- ^ Map a function over a bag.  Carries the
                          --   output type of the function.
        | OEachSet Type   -- ^ Map a function over a set. Carries the
                          --   output type of the function.

        | OReduceList     -- ^ Reduce/fold a list.
        | OReduceBag      -- ^ Reduce/fold a bag (or set).

        | OFilterList     -- ^ Filter a list.
        | OFilterBag      -- ^ Filter a bag.

        | OMerge Type     -- ^ Merge two bags/sets.

        | OConcat         -- ^ List concatenation.  (Perhaps in the
                          --   future this should get
                          --   desugared/compiled into more primitive
                          --   things.)
        | OBagUnions Type -- ^ Bag join, i.e. union a bag of bags.
        | OUnions Type    -- ^ Set join, i.e. union a set of sets.

        | OSummary        -- ^ Adjacency List of given graph
        | OEmptyGraph Type -- ^ Empty graph
        | OVertex Type    -- ^ Construct a vertex with given value
        | OOverlay Type   -- ^ Graph overlay
        | OConnect Type   -- ^ Graph connect

        | OEmptyMap       -- ^ Empty map
        | OInsert         -- ^ Map insert
        | OLookup         -- ^ Map lookup

        -- Ellipses
        | OForever        -- ^ Continue forever, @[x, y, z ..]@
        | OUntil          -- ^ Continue until end, @[x, y, z .. e]@

        -- Container conversion
        | OSetToList      -- ^ set -> list conversion (sorted order).
        | OBagToSet       -- ^ bag -> set conversion (forget duplicates).
        | OBagToList      -- ^ bag -> list conversion (sorted order).
        | OListToSet Type -- ^ list -> set conversion (forget order, duplicates).
                          --   Carries the element type.
        | OListToBag Type -- ^ list -> bag conversion (forget order).
                          --   Carries the element type.
        | OBagToCounts    -- ^ bag -> set of counts
        | OCountsToBag Type  -- ^ set of counts -> bag
        | OMapToSet Type Type -- ^ Map k v -> Set (k × v)
        | OSetToMap           -- ^ Set (k × v) -> Map k v

        -- Number theory primitives
        | OIsPrime        -- ^ Primality test
        | OFactor         -- ^ Factorization

        -- Propositions
        | OForall [Type]  -- ^ Universal quantification. Applied to a closure
                          --   @t1, ..., tn -> Prop@ it yields a @Prop@.
        | OExists [Type]  -- ^ Existential quantification. Applied to a closure
                          --   @t1, ..., tn -> Prop@ it yields a @Prop@.
        | OHolds          -- ^ Convert Prop -> Bool via exhaustive search.
        | ONotProp        -- ^ Flip success and failure for a prop.
        | OShouldEq Type  -- ^ Equality assertion, @=!=@

        -- Other primitives
        | OCrash          -- ^ Crash with a user-supplied message
        | OId             -- ^ No-op/identity function
        | OLookupSeq      -- ^ Lookup OEIS sequence
        | OExtendSeq      -- ^ Extend a List via OEIS

  deriving (Show, Generic)

-- | Get the arity (desired number of arguments) of a function
--   constant.  A few constants have arity 0; everything else is
--   uncurried and hence has arity 1.
opArity :: Op -> Int
opArity OEmptyMap       = 0
opArity (OEmptyGraph _) = 0
opArity _               = 1

-- opArity OAdd             = 2
-- opArity ONeg             = 1
-- opArity OSqrt            = 1
-- opArity OLg              = 1
-- opArity OFloor           = 1
-- opArity OCeil            = 1
-- opArity OAbs             = 1
-- opArity OMul             = 2
-- opArity ODiv             = 2
-- opArity OExp             = 2
-- opArity OMod             = 2
-- opArity ODivides         = 2
-- opArity OMultinom        = 2
-- opArity OFact            = 1
-- opArity (OEq _)          = 2
-- opArity (OLt _)          = 2
-- opArity OEnum            = 1
-- opArity OCount           = 1
-- opArity (OMDiv _)        = 2
-- opArity (OMExp _)        = 2
-- opArity (OMDivides _)    = 2
-- opArity OSize            = 1
-- opArity (OPower _)       = 1
-- opArity (OBagElem _)     = 2
-- opArity (OListElem _)    = 2
-- opArity OMapList         = 2
-- opArity (OMapBag _)      = 2
-- opArity (OMapSet _)      = 2
-- opArity OReduceList      = 3
-- opArity OReduceBag       = 3
-- opArity OFilterList      = 2
-- opArity OFilterBag       = 2
-- opArity OConcat          = 1
-- opArity (OBagUnions _)   = 1
-- opArity (OUnions _)      = 1
-- opArity (OMerge _)       = 3
-- opArity OForever         = 1
-- opArity OUntil           = 2
-- opArity OSetToList       = 1
-- opArity OBagToSet        = 1
-- opArity OBagToList       = 1
-- opArity (OListToSet _)   = 1
-- opArity (OListToBag _)   = 1
-- opArity OBagToCounts     = 1
-- opArity (OCountsToBag _) = 1
-- opArity OIsPrime         = 1
-- opArity OFactor          = 1
-- opArity OCrash           = 1
-- opArity OId              = 1

-- | A branch, consisting of a list of guards and a term.
type CBranch = Bind (Telescope (Embed Core, CPattern)) Core

-- | Core (desugared) patterns.  We only need variables, wildcards,
--   natural numbers, and constructors.
data CPattern where

  -- | A variable pattern, which matches anything and binds the name.
  CPVar  :: Name Core -> CPattern

  -- | A wildcard pattern @_@, which matches anything.
  CPWild :: CPattern

  -- | The unit pattern.
  CPUnit :: CPattern

  -- | An injection into a sum type, with no argument (i.e. the
  --   argument is a unit value).  We could use CPInj with a fresh
  --   variable that we know will bind to the unit value; this pattern
  --   is thus just an optimization to avoid introducing a useless
  --   variable.
  CPTag  :: Side -> CPattern

  -- | An injection into a sum type.
  CPInj  :: Side -> Name Core -> CPattern

  -- | A pair.
  CPPair :: Name Core -> Name Core -> CPattern

  -- | A natural number pattern.
  CPNat  :: Integer -> CPattern

  -- | A fraction pattern, @x/y@.
  CPFrac :: Name Core -> Name Core -> CPattern

  deriving (Show, Generic)

instance Alpha RationalDisplay
instance Alpha Core
instance Alpha Op
instance Alpha CPattern

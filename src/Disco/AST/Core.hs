{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
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
-----------------------------------------------------------------------------

module Disco.AST.Core
       ( -- * Core AST
         RationalDisplay(..)
       , Core(..)
       , Op(..), opArity, substQC, substsQC
       )
       where

import           Control.Lens.Plated
import           Data.Data                        (Data)
import           Data.Data.Lens                   (uniplate)
import           GHC.Generics
import           Prelude                          as P
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Generic                (Side)
import           Disco.Names                      (QName)
import           Disco.Syntax.Operators           (BOp)
import           Disco.Types

-- | A type of flags specifying whether to display a rational number
--   as a fraction or a decimal.
data RationalDisplay = Fraction | Decimal
  deriving (Eq, Show, Generic, Data, Ord, Alpha)

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
  mempty = Fraction
  mappend = (<>)

-- | AST for the desugared, untyped core language.
data Core where
  -- | A variable.
  CVar :: QName Core -> Core
  -- | A rational number.
  CNum :: RationalDisplay -> Rational -> Core
  -- | A built-in constant.
  CConst :: Op -> Core
  -- | An injection into a sum type, i.e. a value together with a tag
  --   indicating which element of a sum type we are in.  For example,
  --   false is represented by @CSum L CUnit@; @right(v)@ is
  --   represented by @CSum R v@.  Note we do not need to remember
  --   which type the constructor came from; if the program
  --   typechecked then we will never end up comparing constructors
  --   from different types.
  CInj :: Side -> Core -> Core
  -- | A primitive case expression on a value of a sum type.
  CCase :: Core -> Bind (Name Core) Core -> Bind (Name Core) Core -> Core
  -- | The unit value.
  CUnit :: Core
  -- | A pair of values.
  CPair :: Core -> Core -> Core
  -- | A projection from a product type, i.e. @fst@ or @snd@.
  CProj :: Side -> Core -> Core
  -- | An anonymous function.
  CAbs :: Bind [Name Core] Core -> Core
  -- | Function application.
  CApp :: Core -> Core -> Core
  -- | A "test frame" under which a test case is run. Records the
  --   types and legible names of the variables that should
  --   be reported to the user if the test fails.
  CTest :: [(String, Type, Name Core)] -> Core -> Core
  -- | A type.
  CType :: Type -> Core
  -- | Introduction form for a lazily evaluated value of type Lazy T
  --   for some type T.  We can have multiple bindings to multiple
  --   terms to create a simple target for compiling mutual recursion.
  CDelay :: Bind [Name Core] [Core] -> Core
  -- | Force evaluation of a lazy value.
  CForce :: Core -> Core
  deriving (Show, Generic, Data, Alpha)

instance Plated Core where
  plate = uniplate

-- | Operators that can show up in the core language.  Note that not
--   all surface language operators show up here, since some are
--   desugared into combinators of the operators here.
data Op
  = -- | Addition (@+@)
    OAdd
  | -- | Arithmetic negation (@-@)
    ONeg
  | -- | Integer square root (@sqrt@)
    OSqrt
  | -- | Floor of fractional type (@floor@)
    OFloor
  | -- | Ceiling of fractional type (@ceiling@)
    OCeil
  | -- | Absolute value (@abs@)
    OAbs
  | -- | Multiplication (@*@)
    OMul
  | -- | Division (@/@)
    ODiv
  | -- | Exponentiation (@^@)
    OExp
  | -- | Modulo (@mod@)
    OMod
  | -- | Divisibility test (@|@)
    ODivides
  | -- | Multinomial coefficient (@choose@)
    OMultinom
  | -- | Factorial (@!@)
    OFact
  | -- | Equality test (@==@)
    OEq
  | -- | Less than (@<@)
    OLt
  | -- Type operators

    -- | Enumerate the values of a type.
    OEnum
  | -- | Count the values of a type.
    OCount
  | -- Container operations

    -- | Size of two sets (@size@)
    OSize
  | -- | Power set/bag of a given set/bag
    --   (@power@).
    OPower
  | -- | Set/bag element test.
    OBagElem
  | -- | List element test.
    OListElem
  | -- | Map a function over a bag.  Carries the
    --   output type of the function.
    OEachBag
  | -- | Map a function over a set. Carries the
    --   output type of the function.
    OEachSet
  | -- | Filter a bag.
    OFilterBag
  | -- | Merge two bags/sets.
    OMerge
  | -- | Bag join, i.e. union a bag of bags.
    OBagUnions
  | -- | Adjacency List of given graph
    OSummary
  | -- | Empty graph
    OEmptyGraph Type
  | -- | Construct a vertex with given value
    OVertex Type
  | -- | Graph overlay
    OOverlay Type
  | -- | Graph connect
    OConnect Type
  | -- | Empty map
    OEmptyMap
  | -- | Map insert
    OInsert
  | -- | Map lookup
    OLookup
  | -- Ellipses

    -- | Continue until end, @[x, y, z .. e]@
    OUntil
  | -- Container conversion

    -- | set -> list conversion (sorted order).
    OSetToList
  | -- | bag -> set conversion (forget duplicates).
    OBagToSet
  | -- | bag -> list conversion (sorted order).
    OBagToList
  | -- | list -> set conversion (forget order, duplicates).
    OListToSet
  | -- | list -> bag conversion (forget order).
    OListToBag
  | -- | bag -> set of counts
    OBagToCounts
  | -- | set of counts -> bag
    OCountsToBag
  | -- | Map k v -> Set (k × v)
    OMapToSet
  | -- | Set (k × v) -> Map k v
    OSetToMap
  | -- Number theory primitives

    -- | Primality test
    OIsPrime
  | -- | Factorization
    OFactor
  | -- | Turn a rational into a (num, denom) pair
    OFrac
  | -- Propositions

    -- | Universal quantification. Applied to a closure
    --   @t1, ..., tn -> Prop@ it yields a @Prop@.
    OForall [Type]
  | -- | Existential quantification. Applied to a closure
    --   @t1, ..., tn -> Prop@ it yields a @Prop@.
    OExists [Type]
  | -- | Convert Prop -> Bool via exhaustive search.
    OHolds
  | -- | Flip success and failure for a prop.
    ONotProp
  | -- | Equality assertion, @=!=@
    OShouldEq Type
  | -- Other primitives

    -- | Error for non-exhaustive pattern match
    OMatchErr
  | -- | Crash with a user-supplied message
    OCrash
  | -- | No-op/identity function
    OId
  | -- | Lookup OEIS sequence
    OLookupSeq
  | -- | Extend a List via OEIS
    OExtendSeq
  deriving (Show, Generic, Data, Alpha)

-- | Get the arity (desired number of arguments) of a function
--   constant.  A few constants have arity 0; everything else is
--   uncurried and hence has arity 1.
opArity :: Op -> Int
opArity OEmptyMap       = 0
opArity (OEmptyGraph _) = 0
opArity OMatchErr       = 0
opArity _               = 1

substQC :: QName Core -> Core -> Core -> Core
substQC x s = transform $ \case
  CVar y
    | x == y -> s
    | otherwise -> CVar y
  t -> t

substsQC :: [(QName Core, Core)] -> Core -> Core
substsQC xs = transform $ \case
  CVar y -> case P.lookup y xs of
    Just c -> c
    _      -> CVar y
  t -> t

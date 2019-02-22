{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

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
       , CBranch, CPattern(..), CQual(..)
       )
       where

import           GHC.Generics
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface                (Telescope)
import           Disco.Types

-- | A type of flags specifying whether to display a rational number
--   as a fraction or a decimal.
data RationalDisplay = Fraction | Decimal
  deriving (Eq, Show, Generic)

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

  -- | A saturated constructor application.  For example, false and
  --   true are represented by @CCons 0 []@ and @CCons 1 []@,
  --   respectively; a pair is represented by @CCons 0 [c1, c2]@.
  --   Note we do not need to remember which type the constructor came
  --   from; if the program typechecked then we will never end up
  --   comparing constructors from different types.
  CCons :: Int -> [Core] -> Core

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

  -- | A type.
  CType :: Type -> Core

  deriving (Show, Generic)

-- | Operators that can show up in the core language.  Note that not
--   all surface language operators show up here, since some are
--   desugared into combinators of the operators here.
data Op = OAdd     -- ^ Addition (@+@)
        | ONeg     -- ^ Arithmetic negation (@-@)
        | OSqrt    -- ^ Integer square root (@sqrt@)
        | OLg      -- ^ Floor of base-2 logarithm (@lg@)
        | OFloor   -- ^ Floor of fractional type (@floor@)
        | OCeil    -- ^ Ceiling of fractional type (@ceiling@)
        | OAbs     -- ^ Absolute value (@abs@)
        | OMul     -- ^ Multiplication (@*@)
        | ODiv     -- ^ Division (@/@)
        | OExp     -- ^ Exponentiation (@^@)
        | OMod     -- ^ Modulo (@mod@)
        | ODivides -- ^ Divisibility test (@|@)
        | OBinom   -- ^ Binomial coefficient (@choose@)
        | OMultinom -- ^ Multinomial coefficient (@choose@)
        | OFact    -- ^ Factorial (@!@)
        | OEq Type -- ^ Equality test (@==@) at the given type.  At
                   --   this point, typechecking has determined that
                   --   the given type has decidable equality.  We
                   --   need to know the type in order to perform the
                   --   equality test.
        | OLt Type -- ^ Less than (@<@).  Similarly, typechecking has
                   --   determined that the given type has a decidable
                   --   ordering relation.

        -- Type operators
        | OEnum    -- ^ Enumerate the values of a type.
        | OCount   -- ^ Count the values of a type.

        -- Arithmetic operators with special runtime behavior for finite types
        | OMDiv  Integer
        | OMExp  Integer
        | OMDivides Integer

        -- Container operations
        | OSize           -- ^ Size of two sets (@size@)
        | OPowerSet Type  -- ^ Power set of a given set
                          --   (@powerSet@). Carries the element type.
        | OSubset Type    -- ^ Subset test for two sets (@⊆@). Carries
                          --   the element type.
        | OUnion  Type    -- ^ Union of two sets (@union@ /
                          --   @∪@). Carries the element type.
        | OInter  Type    -- ^ Intersection of two sets (@intersect@ /
                          --   @∩@). Carries the element type.
        | ODiff   Type    -- ^ Difference of two sets (@\@). Carries
                          --   the element type.
        | ORep            -- ^ Primitive bag constructor (replicate).

        | OMapList        -- ^ Map a function over a list.
        | OMapBag Type    -- ^ Map a function over a bag.  Carries the
                          --   output type of the function.
        | OMapSet Type    -- ^ Map a function over a set. Carries the
                          --   output type of the function.

        | OReduceList     -- ^ Reduce/fold a list.
        | OReduceBag      -- ^ Reduce/fold a bag (or set).

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

        -- Number theory primitives
        | OIsPrime        -- ^ Primality test
        | OFactor         -- ^ Factorization

        -- Other primitives
        | OCrash
        | OId

  deriving (Show, Generic)

-- | Get the arity (desired number of arguments) of a function
--   constant.
opArity :: Op -> Int
opArity OAdd           = 2
opArity ONeg           = 1
opArity OSqrt          = 1
opArity OLg            = 1
opArity OFloor         = 1
opArity OCeil          = 1
opArity OAbs           = 1
opArity OMul           = 2
opArity ODiv           = 2
opArity OExp           = 2
opArity OMod           = 2
opArity ODivides       = 2
opArity OBinom         = 2
opArity OMultinom      = 2
opArity OFact          = 1
opArity (OEq _)        = 2
opArity (OLt _)        = 2
opArity OEnum          = 1
opArity OCount         = 1
opArity (OMDiv _)      = 2
opArity (OMExp _)      = 2
opArity (OMDivides _)  = 2
opArity OSize          = 1
opArity (OPowerSet _)  = 1
opArity (OSubset _)    = 2
opArity (OUnion _)     = 2
opArity (OInter _)     = 2
opArity (ODiff _)      = 2
opArity ORep           = 2
opArity OMapList       = 2
opArity (OMapBag _)    = 2
opArity (OMapSet _)    = 2
opArity OReduceList    = 3
opArity OReduceBag     = 3
opArity OForever       = 1
opArity OUntil         = 2
opArity OSetToList     = 1
opArity OBagToSet      = 1
opArity OBagToList     = 1
opArity (OListToSet _) = 1
opArity (OListToBag _) = 1
opArity OIsPrime       = 1
opArity OFactor        = 1
opArity OCrash         = 1
opArity OId            = 1

-- | A branch, consisting of a list of guards and a term.
type CBranch = Bind (Telescope (Embed Core, CPattern)) Core

-- | A single qualifier in a list comprehension.
data CQual where

  -- | A binding qualifier (i.e. @x <- t@)
  CQBind   :: Name Core -> Embed Core -> CQual

  -- | A boolean guard qualfier (i.e. @x + y > 4@)
  CQGuard  :: Embed Core -> CQual

  deriving (Show, Generic)

-- | Core (desugared) patterns.  We only need variables, wildcards,
--   natural numbers, and constructors.
data CPattern where

  -- | A variable pattern, which matches anything and binds the name.
  CPVar  :: Name Core -> CPattern

  -- | A wildcard pattern @_@, which matches anything.
  CPWild :: CPattern

  -- | A cons-pattern.  @CPCons i pats@ matches @CCons j xs@ if @i ==
  --   j@.
  CPCons :: Int -> [Name Core] -> CPattern

  -- | A natural number pattern.
  CPNat  :: Integer -> CPattern

  -- | A fraction pattern, @x/y@.
  CPFrac :: Name Core -> Name Core -> CPattern

  deriving (Show, Generic)

instance Alpha RationalDisplay
instance Alpha Core
instance Alpha Op
instance Alpha CPattern
instance Alpha CQual

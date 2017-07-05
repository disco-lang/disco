{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Core
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Abstract syntax trees representing the desugared, untyped core
-- language for Disco.
--
-----------------------------------------------------------------------------

module Disco.AST.Core
       ( -- * Core AST
         RationalDisplay(..)
       , Core(..)
       , Op(..)

         -- * Case expressions and patterns
       , CBranch, CGuards(..), CPattern(..)
       )
       where

import           Unbound.LocallyNameless

import           Disco.Types

-- | A type of flags specifying whether to display a rational number
--   as a fraction or a decimal.
data RationalDisplay = Fraction | Decimal
  deriving (Eq, Show)

-- | The 'Monoid' instance for 'RationalDisplay' corresponds to the
--   idea that the result should be displayed as a decimal if any
--   decimal literals are used in the input; otherwise, the default is
--   to display as a fraction.  So the identity element is 'Fraction',
--   and 'Decimal' always wins when combining.
instance Monoid RationalDisplay where
  mempty = Fraction
  Decimal `mappend` _ = Decimal
  _ `mappend` Decimal = Decimal
  _ `mappend` _       = Fraction

-- | AST for the desugared, untyped core language.
data Core where

  -- | A variable.
  CVar  :: Name Core -> Core

  -- | A constructor, identified by number, with arguments.  For
  --   example, false and true are represented by @CCons 0 []@ and
  --   @CCons 1 []@, respectively; a pair is represented by @CCons 0
  --   [c1, c2]@.  Note we do not need to remember which type the
  --   constructor came from; if the program typechecked then we will
  --   never end up comparing constructors from different types.
  CCons :: Int -> [Core] -> Core

  -- | A rational number.
  CNum  :: RationalDisplay -> Rational -> Core

  -- | An anonymous function.
  CAbs  :: Bind (Name Core) Core -> Core

  -- | Function application, with a strictness annotation.  The
  --   strictness is determined by the type of the application (which
  --   has been erased), and determines whether the argument should be
  --   evaluated before applying the function.
  CApp  :: Strictness -> Core -> Core -> Core

  -- | Operator application, with a list of arguments.  Note there is
  --   no longer any distinction between unary and binary operators.
  --   Assuming this was correctly desugared from a successfully
  --   parsed and typechecked program, operators will always have the
  --   correct number of arguments.
  COp   :: Op -> [Core] -> Core

  -- | Non-recursive let @let x = t1 in t2@, with a strictness
  --   annotation to determine whether @t1@ should be evaluated before
  --   evaluating @t2@.
  CLet  :: Strictness -> Bind (Name Core, Embed Core) Core -> Core

  -- | A case expression.
  CCase :: [CBranch] -> Core

  -- | A type.
  CType :: Type -> Core

  deriving Show

-- | Operators that can show up in the core language.  Note that not
--   all surface language operators show up here, since some are
--   desugared into combinators of the operators here.
data Op = OAdd     -- ^ Addition (@+@)
        | ONeg     -- ^ Arithmetic negation (@-@)
        | OSqrt    -- ^ Integer square root (@sqrt@)
        | OLg      -- ^ Floor of base-2 logarithm (@lg@)
        | OFloor   -- ^ Floor of fractional type (@floor@)
        | OCeil    -- ^ Ceiling of fractional type (@ceiling@)
        | OMul     -- ^ Multiplication (@*@)
        | ODiv     -- ^ Division (@/@)
        | OExp     -- ^ Exponentiation (@^@)
        | OAnd     -- ^ Logical and (@&&@ / @and@)
        | OOr      -- ^ Logical or (@||@ / @or@)
        | ONot     -- ^ Arithmetic negation (@not@)
        | OMod     -- ^ Modulo (@mod@)
        | ODivides -- ^ Divisibility test (@|@)
        | ORelPm   -- ^ Relative primality test (@#@)
        | OBinom   -- ^ Binomial coefficient (@choose@)
        | OFact    -- ^ Factorial (@!@)
        | OEq Type -- ^ Equality test (@==@) at the given type.  At
                   --   this point, typechecking has determined that
                   --   the given type has decidable equality.  We
                   --   need to know the type in order to perform the
                   --   equality test.
        | OLt Type -- ^ Less than (@<@).  Similarly, typechecking has
                   --   determined that the given type has a decidable
                   --   ordering relation.
        | OEnum
        | OCount
  deriving Show

-- | A branch, consisting of a list of guards and a term.
type CBranch = Bind CGuards Core

-- | A list of guards.
data CGuards where

  -- | Empty list of guards (/i.e./ @otherwise@).
  CGEmpty :: CGuards

  -- | A single guard followed by a list of guards.  Note there is
  --   only one kind of guard in the core language, corresponding to
  --   pattern guards (@when@) in the surface language.  Boolean
  --   (@if@) guards are desugared to pattern matching on @true@.
  CGCons  :: Rebind (Embed Core, CPattern) CGuards -> CGuards
  deriving Show

-- | Core (desugared) patterns.  We only need variables, wildcards,
--   natural numbers, and constructors.
data CPattern where

  -- | A variable pattern, which matches anything and binds the name.
  CPVar  :: Name Core -> CPattern

  -- | A wildcard pattern @_@, which matches anything.
  CPWild :: CPattern

  -- | A cons-pattern.  @CPCons i pats@ matches @CCons j tms@ if @i ==
  --   j@ and @pats@ all match @tms@ recursively.
  CPCons :: Int -> [CPattern] -> CPattern

  -- | A natural number pattern.
  CPNat  :: Integer -> CPattern

  -- | A successor pattern, @S p@.
  CPSucc :: CPattern -> CPattern

  deriving Show

derive [''RationalDisplay, ''Core, ''Op, ''CPattern, ''CGuards]

instance Alpha Rational   -- XXX duplicate orphan!
instance Alpha RationalDisplay
instance Alpha Core
instance Alpha Op
instance Alpha CPattern
instance Alpha CGuards

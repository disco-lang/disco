{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types.Qualifiers
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Type qualifiers and sorts.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Types.Qualifiers where

import           Data.Data
import           GHC.Generics
import           Unbound.Generics.LocallyNameless

import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Disco.Pretty
import           Disco.Syntax.Operators

------------------------------------------------------------
-- Qualifiers
------------------------------------------------------------

-- | A "qualifier" is kind of like a type class in Haskell; but unlike
--   Haskell, disco users cannot define their own.  Rather, there is a
--   finite fixed list of qualifiers supported by disco.  For example,
--   @QSub@ denotes types which support a subtraction operation.  Each
--   qualifier corresponds to a set of types which satisfy it (see
--   'hasQual' and 'qualRules').
--
--   These qualifiers generally arise from uses of various operations.
--   For example, the expression @\\x y. x - y@ would be inferred to
--   have a type @a -> a -> a [subtractive a]@, that is, a function of
--   type @a -> a -> a@ where @a@ is any type that supports
--   subtraction.
--
--   These qualifiers can appear in a 'CQual' constraint; see
--   "Disco.Typecheck.Constraint".
data Qualifier
  = QNum       -- ^ Numeric, i.e. a semiring supporting + and *
  | QSub       -- ^ Subtractive, i.e. supports -
  | QDiv       -- ^ Divisive, i.e. supports /
  | QCmp       -- ^ Comparable, i.e. supports decidable ordering/comparison (see Note [QCmp])
  | QEnum      -- ^ Enumerable, i.e. supports ellipsis notation [x .. y]
  | QBool      -- ^ Boolean, i.e. supports and, or, not (Bool or Prop)
  | QBasic     -- ^ Things that do not involve Prop.
  | QSimple    -- ^ Things for which we can derive a *Haskell* Ord instance
  deriving (Show, Eq, Ord, Generic, Data, Alpha)

instance Pretty Qualifier where
  pretty = \case
    QNum    -> "num"
    QSub    -> "sub"
    QDiv    -> "div"
    QCmp    -> "cmp"
    QEnum   -> "enum"
    QBool   -> "bool"
    QBasic  -> "basic"
    QSimple -> "simple"

-- ~~~~ Note [QCmp]
--
-- XXX edit this!  I don't think we actually need type info for
-- comparisons at runtime any more, if we disallow functions from
-- being QCmp.  With the switch to eager semantics + disallowing
-- function comparison, it's now the case that QCmp should mean
-- *decidable* (terminating) comparison.
--
-- It used to be the case that every type in disco supported
-- (semi-decidable) linear ordering, so in one sense the QCmp
-- constraint was unnecessary.  However, in order to do a comparison we
-- need to know the type at runtime.  Currently, we use QCmp to track
-- which types have comparisons done on them, and reject any type
-- variables with a QCmp constraint (just as we reject any other type
-- variables with remaining constraints).  Every type with comparisons
-- done on it must be statically known at compile time.
--
-- However, there's now another reason: the Prop type does not support
-- comparisons at all.
--
-- Eventually, one could imagine compiling to something like System F
-- with explicit type lambdas and applications; then the QCmp
-- constraints would tell us which type applications need to be kept
-- and which can be erased.

-- | A helper function that returns the appropriate qualifier for a
--   binary arithmetic operation.
bopQual :: BOp -> Qualifier
bopQual Add  = QNum
bopQual Mul  = QNum
bopQual Div  = QDiv
bopQual Sub  = QSub
bopQual SSub = QNum
-- bopQual And  = QBool
-- bopQual Or   = QBool
-- bopQual Impl = QBool
bopQual _    = error "No qualifier for binary operation"

------------------------------------------------------------
-- Sorts
------------------------------------------------------------

-- | A 'Sort' represents a set of qualifiers, and also represents a
--   set of types (in general, the intersection of the sets
--   corresponding to the qualifiers).
type Sort = Set Qualifier

-- | The special sort \(\top\) which includes all types.
topSort :: Sort
topSort = S.empty

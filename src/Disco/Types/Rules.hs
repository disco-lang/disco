{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types.Rules
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- "Disco.Types.Rules" defines some generic rules about arity,
-- subtyping, and sorts for disco base types.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Types.Rules
  ( -- * Arity

    Variance(..), arity

    -- * Qualifiers
  , Qualifier(..), bopQual

    -- * Sorts
  , Sort, topSort

    -- * Subtyping rules

  , Dir(..), other

  , isSubA, isSubB, isDirB
  , supertypes, subtypes, dirtypes

    -- * Qualifier and sort rules

  , hasQual, hasSort
  , qualRules, sortRules
  , pickSortBaseTy
  )
  where

import           GHC.Generics
import           Unbound.Generics.LocallyNameless

import           Math.NumberTheory.Primes.Testing (isPrime)

import           Data.List                        (foldl')
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Disco.Syntax.Operators
import           Disco.Types

------------------------------------------------------------
-- Arity
------------------------------------------------------------

-- | A particular type argument can be either co- or contravariant
--   with respect to subtyping.
data Variance = Co | Contra
  deriving (Show, Read, Eq, Ord)

-- | The arity of a type constructor is a list of variances,
--   expressing both how many type arguments the constructor takes,
--   and the variance of each argument.  This is used to decompose
--   subtyping constraints.
--
--   For example, @arity CArr = [Contra, Co]@ since function arrow is
--   contravariant in its first argument and covariant in its second.
--   That is, @S1 -> T1 <: S2 -> T2@ (@<:@ means "is a subtype of") if
--   and only if @S2 <: S1@ and @T1 <: T2@.
arity :: Con -> [Variance]
arity CArr           = [Contra, Co]
arity CPair          = [Co, Co]
arity CSum           = [Co, Co]
arity (CContainer _) = [Co]
arity (CUser _)      = error "Impossible! arity CUser"
  -- CUsers should always be replaced by their definitions before arity
  -- is called.

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
--   have a type @∀ a. QSub a => a -> a -> a@, that is, a function of type
--   @a -> a -> a@ where @a@ is any type that supports subtraction.
--
--   These qualifiers can appear in a 'CQual' constraint; see
--   "Disco.Typecheck.Constraint".
data Qualifier
  = QNum       -- ^ Numeric, i.e. a semiring supporting + and *
  | QSub       -- ^ Subtractive, i.e. supports -
  | QDiv       -- ^ Divisive, i.e. supports /
  | QCmp       -- ^ Comparable, i.e. supports ordering/comparison (see Note [QCmp])
  | QEnum      -- ^ Enumerable, i.e. supports ellipsis notation [x .. y]
  deriving (Show, Eq, Ord, Generic)

instance Alpha Qualifier
instance Subst Type Qualifier

-- ~~~~ Note [QCmp]
--
-- Every type in disco supports (semi-decidable) linear ordering, so
-- in one sense the QCmp constraint is unnecessary.  However, in order
-- to do a comparison we need to know the type at runtime.  Currently,
-- we use QCmp to track which types have comparisons done on them, and
-- reject any type variables with a QCmp constraint (just as we reject
-- any other type variables with remaining constraints).  Every type
-- with comparisons done on it must be statically known at compile
-- time.
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

------------------------------------------------------------
-- Subtyping rules
------------------------------------------------------------

-- | A "direction" for the subtyping relation (either subtype or
--   supertype).
data Dir = SubTy | SuperTy
  deriving (Eq, Ord, Read, Show)

-- | Swap directions.
other :: Dir -> Dir
other SubTy   = SuperTy
other SuperTy = SubTy

--------------------------------------------------
-- Subtype checks

-- | Check whether one atomic type is a subtype of the other. Returns
--   @True@ if either they are equal, or if they are base types and
--   'isSubB' returns true.
isSubA :: Atom -> Atom -> Bool
isSubA a1 a2                 | a1 == a2 = True
isSubA (ABase t1) (ABase t2) = isSubB t1 t2
isSubA _ _                   = False

-- | Check whether one base type is a subtype of another.
isSubB :: BaseTy -> BaseTy -> Bool
isSubB b1 b2 | b1 == b2 = True
isSubB N Z   = True
isSubB N F   = True
isSubB N Q   = True
isSubB Z Q   = True
isSubB F Q   = True
isSubB _ _   = False

-- | Check whether one base type is a sub- or supertype of another.
isDirB :: Dir -> BaseTy -> BaseTy -> Bool
isDirB SubTy   b1 b2 = isSubB b1 b2
isDirB SuperTy b1 b2 = isSubB b2 b1

-- | List all the supertypes of a given base type.
supertypes :: BaseTy -> [BaseTy]
supertypes N  = [N, Z, F, Q]
supertypes Z  = [Z, Q]
supertypes F  = [F, Q]
supertypes ty = [ty]

-- | List all the subtypes of a given base type.
subtypes :: BaseTy -> [BaseTy]
subtypes Q  = [Q, F, Z, N]
subtypes F  = [F, N]
subtypes Z  = [Z, N]
subtypes ty = [ty]

-- | List all the sub- or supertypes of a given base type.
dirtypes :: Dir -> BaseTy -> [BaseTy]
dirtypes SubTy   = subtypes
dirtypes SuperTy = supertypes

------------------------------------------------------------
-- Qualifier and sort rules
------------------------------------------------------------

-- | Check whether a given base type satisfies a qualifier.
hasQual :: BaseTy -> Qualifier -> Bool
hasQual _ QCmp        = True
hasQual (Fin _) q     | q `elem` [QNum, QSub, QEnum] = True
hasQual (Fin n) QDiv  = isPrime n
hasQual b       QNum  = b `elem` [N, Z, F, Q]
hasQual b       QSub  = b `elem` [Z, Q]
hasQual b       QDiv  = b `elem` [F, Q]
hasQual b       QEnum = b `elem` [N, Z, F, Q, C]

-- | Check whether a base type has a certain sort, which simply
--   amounts to whether it satisfies every qualifier in the sort.
hasSort :: BaseTy -> Sort -> Bool
hasSort = all . hasQual

-- | 'qualRules' encodes the rules by which applications of type
--   constructors can satisfy various qualifiers.
--
--   Each constructor maps to a set of rules.  Each rule is a mapping
--   from a qualifier to the list of qualifiers needed on the type
--   constructor's arguments for the bigger type to satisfy the
--   qualifier.
--
--   Note in Disco we can get away with any given qualifier requiring
--   /at most one/ qualifier on each type argument.  Then we can
--   derive the 'sortRules' by combining 'qualRules'.  In general,
--   however, you could imagine some particular qualifier requiring a
--   set of qualifiers (i.e. a general sort) on a type argument.  In
--   that case one would just have to encode 'sortRules' directly.
qualRules :: Map Con (Map Qualifier [Maybe Qualifier])
qualRules = M.fromList
  [ CArr  ==> M.fromList
    [ QCmp ==> [Nothing, Just QCmp]
    ]
  , CPair ==> M.fromList
    [ QCmp ==> [Just QCmp, Just QCmp]
    ]
  , CSum ==> M.fromList
    [ QCmp ==> [Just QCmp, Just QCmp]
    ]
  , CList ==> M.fromList
    [ QCmp ==> [Just QCmp]
    ]
  , CBag ==> M.fromList
    [ QCmp ==> [Just QCmp]
    ]
  , CSet ==> M.fromList
    [ QCmp ==> [Just QCmp]
    ]
  ]
  where
    (==>) :: a -> b -> (a,b)
    (==>) = (,)

  -- Eventually we can easily imagine adding an opt-in mode where
  -- numeric operations can be used on pairs and functions, then the
  -- qualRules would become dependent on what language extension/mode
  -- was chosen.  For example we could have rules like
  --
  -- [ CArr ==> M.fromList
  --   [ QNum ==> [Nothing, Just QNum]  -- (a -> b) can be +, * iff b can
  --   , QSub ==> [Nothing, Just QSub]  -- ditto for subtraction
  --   , QDiv ==> [Nothing, Just QDiv]  -- and division
  --   ]
  -- , CPair ==> M.fromList
  --   [ QNum ==> [Just QNum, Just QNum] -- (a,b) can be +, * iff a and b can
  --   , QSub ==> [Just QSub, Just QSub] -- etc.
  --   , QDiv ==> [Just QDiv, Just QDiv]
  --   ]
  -- ]

-- | @sortRules T s = [s1, ..., sn]@ means that sort @s@ holds of
--   type @(T t1 ... tn)@ if and only if  @s1 t1 /\ ... /\ sn tn@.
--   For now this is just derived directly from 'qualRules'.
--
--   This is the @arity@ function described in section 4.1 of Traytel et
--   al.
sortRules :: Con -> Sort -> Maybe [Sort]
sortRules c s = do
  -- If tycon c is not in the qualRules map, there's no way to make it
  -- an instance of any sort, so fail
  qualMap   <- M.lookup c qualRules

  -- If any of the quals q in sort s are not in the map corresponding
  -- to tycon c, there's no way to make c an instance of q, so fail
  -- (the mapM will succeed only if all lookups succeed)
  needQuals <- mapM (flip M.lookup qualMap) (S.toList s)

  -- Otherwise we are left with a list (corresponding to all the quals
  -- in sort s) of lists (each one corresponds to the type args of c).
  -- We zip them together to produce a list of sorts.
  return $ foldl' (zipWith (\srt -> maybe srt (`S.insert` srt))) (repeat topSort) needQuals

-- | Pick a base type that satisfies a given sort.
pickSortBaseTy :: Sort -> BaseTy
pickSortBaseTy s
  | QDiv  `S.member` s && QSub `S.member` s = Q
  | QDiv  `S.member` s = F
  | QSub  `S.member` s = Z
  | QNum  `S.member` s = N
  | QCmp  `S.member` s = N
  | QEnum `S.member` s = N
  | otherwise          = Unit

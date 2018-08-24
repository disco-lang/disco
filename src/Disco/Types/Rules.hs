{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Types.Rules
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rules about arity, subtyping, and sorts for disco base types.
--
-----------------------------------------------------------------------------

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

data Variance = Co | Contra
  deriving (Show, Read, Eq, Ord)

-- | The arity of a type constructor is a list of variances,
--   expressing both how many type arguments the constructor takes,
--   and the variance of each argument.  This is used to decompose
--   subtyping constraints.
arity :: Con -> [Variance]
arity CArr  = [Contra, Co]
arity CPair = [Co, Co]
arity CSum  = [Co, Co]
arity CList = [Co]
arity CSet  = [Co]

------------------------------------------------------------
-- Qualifiers
------------------------------------------------------------

-- | Qualifiers that may appear in the CQual constraint.
data Qualifier = QNum | QSub | QDiv | QFin
  deriving (Show, Eq, Ord, Generic)

instance Alpha Qualifier

instance Subst Type Qualifier

-- | A helper function that returns the appropriate qualifier for a binary operation.
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

-- | The special sort "top" which includes all types.
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

-- | Check whether one atomic type is a subtype of the other. True if
--   either they are equal, or if they are base types and 'isSubB'
--   returns true.
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
hasQual (Fin _) q    | q `elem` [QNum, QSub, QFin] = True
hasQual (Fin n) QDiv = isPrime n
hasQual b       QNum = b `elem` [N, Z, F, Q]
hasQual b       QSub = b `elem` [Z, Q]
hasQual b       QDiv = b `elem` [F, Q]
hasQual _ _          = False

-- | Check whether a base type has a certain sort.
hasSort :: BaseTy -> Sort -> Bool
hasSort = all . hasQual

-- | qualRules encodes the rules by which applications of type constructors can satisfy
--   various qualifiers.
--
--   @(c, (q, qs))@ means that  @q (TyCon c t1 t2 ... tn)@ if and only if
--   @q1 t1 /\ q2 t2 /\ ... /\ qn tn@.
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
    [ QFin ==> [Just QFin, Just QFin]
    ]
  , CPair ==> M.fromList
    [ QFin ==> [Just QFin, Just QFin]
    ]
  , CSum ==> M.fromList
    [ QFin ==> [Just QFin, Just QFin]
    ]
  -- no rules for CList
  ]
  where
    (==>) :: a -> b -> (a,b)
    (==>) = (,)

  -- Once we get rid of QFin, there won't be any rules left!  But
  -- that's OK, we'll leave the machinery here for now.  Eventually we
  -- can easily imagine adding an opt-in mode where numeric operations
  -- can be used on pairs and functions, then the qualRules would
  -- become dependent on what mode was chosen.  For example we could
  -- have rules like
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
  | QDiv `S.member` s && QSub `S.member` s = Q
  | QDiv `S.member` s = F
  | QSub `S.member` s = Z
  | QNum `S.member` s = N
  | otherwise         = Unit

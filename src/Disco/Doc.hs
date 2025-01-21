-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Doc
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Built-in documentation.
module Disco.Doc (
  DocKey (..),
  RefType (..),
  Reference (..),
  mkRef,
  mkIntro,
  docMap,
) where

import Data.Map (Map)
import qualified Data.Map as M

import Disco.Syntax.Operators
import Disco.Syntax.Prims
import Disco.Util ((==>))

-- | Lookup keys for documentation.
data DocKey where
  PrimKey :: Prim -> DocKey
  OtherKey :: String -> DocKey
  deriving (Eq, Ord, Show)

-- | An enumeration of different types of documentation references.
data RefType where
  -- | A reference to the Gentle Introduction (https://disco-lang.readthedocs.io/en/latest/introduction/index.html)
  Intro :: RefType
  -- | A reference to the Language Reference (https://disco-lang.readthedocs.io/en/latest/reference/index.html)
  Ref :: RefType
  -- | An arbitrary free-form URL
  URL :: RefType
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | A reference for further reading.
data Reference = Reference {refType :: RefType, ref :: String}
  deriving (Eq, Ord, Show)

mkRef :: String -> Reference
mkRef = Reference Ref

mkIntro :: String -> Reference
mkIntro = Reference Intro

-- | A map storing documentation for various things that can be looked
--   up with :doc.  Each key is mapped to a short descriptive string,
--   plus references for further reading.
docMap :: Map DocKey (String, [Reference])
docMap =
  M.fromList
    [ PrimKey (PrimUOp Neg)
        ==> "Arithmetic negation."
        ==> []
    , PrimKey (PrimBOp Add)
        ==> "The sum of two numbers, types, or graphs."
        ==> [mkIntro "arithmetic", mkRef "addition"]
    , PrimKey (PrimBOp Sub)
        ==> "The difference of two numbers."
        ==> [mkIntro "arithmetic", mkRef "subtraction"]
    , PrimKey (PrimBOp SSub)
        ==> "The difference of two numbers, with a lower bound of 0."
        ==> [mkIntro "arithmetic", mkRef "subtraction", mkRef "symbols"]
    , PrimKey (PrimBOp Mul)
        ==> "The product of two numbers, types, or graphs."
        ==> [mkIntro "arithmetic", mkRef "multiplication"]
    , PrimKey (PrimBOp Div)
        ==> "Divide two numbers."
        ==> [mkIntro "arithmetic", mkRef "division"]
    , PrimKey (PrimBOp IDiv)
        ==> "The integer quotient of two numbers, rounded down."
        ==> [mkIntro "arithmetic", mkRef "integerdiv"]
    , PrimKey (PrimBOp Mod)
        ==> "a mod b is the remainder when a is divided by b."
        ==> [mkRef "mod"]
    , PrimKey (PrimBOp Exp)
        ==> "Exponentiation.  a ^ b is a raised to the b power."
        ==> [mkIntro "arithmetic", mkRef "exponentiation"]
    , PrimKey (PrimUOp Fact)
        ==> "n! computes the factorial of n, that is, 1 * 2 * ... * n."
        ==> [mkRef "factorial"]
    , PrimKey PrimFloor
        ==> "floor(x) is the largest integer which is <= x."
        ==> [mkIntro "arithmetic", mkRef "round", mkRef "symbols"]
    , PrimKey PrimCeil
        ==> "ceiling(x) is the smallest integer which is >= x."
        ==> [mkIntro "arithmetic", mkRef "round", mkRef "symbols"]
    , PrimKey PrimAbs
        ==> "abs(x) is the absolute value of x."
        ==> [mkIntro "arithmetic", mkRef "abs"]
    , PrimKey PrimMin
        ==> "min(x,y) is the minimum of x and y, i.e. whichever is smaller."
        ==> [mkRef "compare"]
    , PrimKey PrimMax
        ==> "max(x,y) is the maximum of x and y, i.e. whichever is larger."
        ==> [mkRef "compare"]
    , PrimKey (PrimUOp Not)
        ==> "Logical negation: not(T) = F and not(F) = T."
        ==> [mkRef "logic-ops", mkRef "symbols"]
    , PrimKey (PrimBOp And)
        ==> "Logical conjunction (and): T /\\ T = T; otherwise x /\\ y = F."
        ==> [mkRef "logic-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Or)
        ==> "Logical disjunction (or): F \\/ F = F; otherwise x \\/ y = T."
        ==> [mkRef "logic-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Impl)
        ==> "Logical implication (implies): T -> F = F; otherwise x -> y = T."
        ==> [mkRef "logic-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Iff)
        ==> "Biconditional (if and only if)."
        ==> [mkRef "logic-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Eq)
        ==> "Equality test.  x == y is T if x and y are equal."
        ==> [mkRef "compare"]
    , PrimKey (PrimBOp Neq)
        ==> "Inequality test.  x /= y is T if x and y are unequal."
        ==> [mkRef "compare", mkRef "symbols"]
    , PrimKey (PrimBOp Lt)
        ==> "Less-than test. x < y is T if x is less than (but not equal to) y."
        ==> [mkRef "compare"]
    , PrimKey (PrimBOp Gt)
        ==> "Greater-than test. x > y is T if x is greater than (but not equal to) y."
        ==> [mkRef "compare"]
    , PrimKey (PrimBOp Leq)
        ==> "Less-than-or-equal test. x <= y is T if x is less than or equal to y."
        ==> [mkRef "compare", mkRef "symbols"]
    , PrimKey (PrimBOp Geq)
        ==> "Greater-than-or-equal test. x >= y is T if x is greater than or equal to y."
        ==> [mkRef "compare", mkRef "symbols"]
    , PrimKey (PrimBOp CartProd)
        ==> "Cartesian product, i.e. the collection of all pairs.  Also works on bags and sets."
        ==> [mkRef "cp", mkRef "symbols"]
    , PrimKey PrimPower
        ==> "Power set, i.e. the set of all subsets.  Also works on bags."
        ==> [mkRef "power"]
    , PrimKey (PrimBOp Union)
        ==> "Union of two sets (or bags)."
        ==> [mkRef "set-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Inter)
        ==> "Intersection of two sets (or bags)."
        ==> [mkRef "set-ops", mkRef "symbols"]
    , PrimKey (PrimBOp Diff)
        ==> "Difference of two sets (or bags)."
        ==> [mkRef "set-ops"]
    , OtherKey "N" ==> docN
    , OtherKey "ℕ" ==> docN
    , OtherKey "Nat" ==> docN
    , OtherKey "Natural" ==> docN
    , OtherKey "Z" ==> docZ
    , OtherKey "ℤ" ==> docZ
    , OtherKey "Int" ==> docZ
    , OtherKey "Integer" ==> docZ
    , OtherKey "F" ==> docF
    , OtherKey "𝔽" ==> docF
    , OtherKey "Frac" ==> docF
    , OtherKey "Fractional" ==> docF
    , OtherKey "Q" ==> docQ
    , OtherKey "ℚ" ==> docQ
    , OtherKey "Rational" ==> docQ
    , OtherKey "Bool" ==> docB
    , OtherKey "Boolean" ==> docB
    , OtherKey "Unit"
        ==> "The unit type, i.e. a type with only a single value."
        ==> [mkRef "unit", mkRef "symbols"]
    , OtherKey "Prop"
        ==> "The type of propositions."
        ==> [mkRef "prop"]
    , OtherKey "List"
        ==> "List(T) is the type of lists whose elements have type T."
        ==> [mkRef "list", mkRef "list-lib"]
    , OtherKey "Bag"
        ==> "Bag(T) is the type of bags (i.e. sets with multiplicity) whose elements have type T."
        ==> [mkRef "bag", mkRef "symbols"]
    , OtherKey "Set"
        ==> "Set(T) is the type of finite sets whose elements have type T."
        ==> [mkRef "set", mkRef "symbols"]
    , OtherKey "|~|"
        ==> "Absolute value, or the size of a collection."
        ==> [mkIntro "arithmetic", mkRef "size"]
    , OtherKey "{?"
        ==> "{? ... ?} is a case expression, for choosing a result based on conditions."
        ==> [mkRef "case"]
    , OtherKey "λ"
        ==> "λ (aka lambda, alternatively `\\`) introduces an anonymous function."
        ==> [mkRef "anonymous-func", mkRef "symbols"]
    , OtherKey "#"
        ==> "The # character is used to denote the cardinality of an element in a bag."
        ==> [mkRef "bag"]
    ]
 where
  docN = ("The type of natural numbers: 0, 1, 2, ...", refsN)
  refsN = [mkIntro "types", mkRef "natural", mkRef "symbols"]

  docZ = ("The type of integers: ..., -2, -1, 0, 1, 2, ...", refsZ)
  refsZ = [mkIntro "types", mkRef "integer", mkRef "symbols"]

  docF = ("The type of fractional numbers p/q >= 0.", refsF)
  refsF = [mkIntro "types", mkRef "fraction", mkRef "symbols"]

  docQ = ("The type of rational numbers p/q.", refsQ)
  refsQ = [mkIntro "types", mkRef "rational", mkRef "symbols"]

  docB = ("The type of Booleans (T or F).", refsB)
  refsB = [mkRef "bool"]

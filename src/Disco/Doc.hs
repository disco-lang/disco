-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Doc
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Built-in documentation.
--
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.Doc
       ( primDoc, primReference, otherDoc, otherReference
       ) where

import           Data.Map               (Map)
import qualified Data.Map               as M

import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Util             ((==>))

-- | A map from some primitives to a short descriptive string,
--   to be shown by the :doc command.
primDoc :: Map Prim String
primDoc = M.fromList
  [ PrimUOp Neg  ==> "Arithmetic negation."
  , PrimBOp Add  ==> "The sum of two numbers, types, or graphs."
  , PrimBOp Sub  ==> "The difference of two numbers."
  , PrimBOp SSub ==> "The difference of two numbers, with a lower bound of 0."
  , PrimBOp Mul  ==> "The product of two numbers, types, or graphs."
  , PrimBOp Div  ==> "Divide two numbers."
  , PrimBOp IDiv ==> "The integer quotient of two numbers, rounded down."
  , PrimBOp Mod  ==> "a mod b is the remainder when a is divided by b."
  , PrimBOp Exp  ==> "Exponentiation.  a ^ b is a raised to the b power."
  , PrimUOp Fact ==> "n! computes the factorial of n, that is, 1 * 2 * ... * n."
  , PrimFloor    ==> "floor(x) is the largest integer which is <= x."
  , PrimCeil     ==> "ceiling(x) is the smallest integer which is >= x."
  , PrimAbs      ==> "abs(x) is the absolute value of x.  Also written |x|."
  , PrimUOp Not  ==> "Logical negation: not(true) = false and not(false) = true."
  , PrimBOp And  ==> "Logical conjunction (and): true /\\ true = true; otherwise x /\\ y = false."
  , PrimBOp Or   ==> "Logical disjunction (or): false \\/ false = false; otherwise x \\/ y = true."
  , PrimBOp Impl ==> "Logical implication (implies): true -> false = false; otherwise x -> y = true."
  , PrimBOp Iff  ==> "Biconditional (if and only if)."
  , PrimBOp Eq   ==> "Equality test.  x == y is true if x and y are equal."
  , PrimBOp Neq  ==> "Inequality test.  x /= y is true if x and y are unequal."
  , PrimBOp Lt   ==> "Less-than test. x < y is true if x is less than (but not equal to) y."
  , PrimBOp Gt   ==> "Greater-than test. x > y is true if x is greater than (but not equal to) y."
  , PrimBOp Leq  ==> "Less-than-or-equal test. x <= y is true if x is less than or equal to y."
  , PrimBOp Geq  ==> "Greater-than-or-equal test. x >= y is true if x is greater than or equal to y."

  , PrimBOp CartProd ==> "Cartesian product, i.e. the collection of all pairs.  Also works on bags and sets."

  ]

-- | A map from some primitives to their corresponding page in the
--   Disco language reference
--   (https://disco-lang.readthedocs.io/en/latest/reference/index.html).
primReference :: Map Prim String
primReference = M.fromList
  [ PrimBOp Add  ==> "addition"
  , PrimBOp Sub  ==> "subtraction"
  , PrimBOp SSub ==> "subtraction"
  , PrimBOp Mul  ==> "multiplication"
  , PrimBOp Div  ==> "division"
  , PrimBOp IDiv ==> "integerdiv"
  , PrimBOp Mod  ==> "mod"
  , PrimBOp Exp  ==> "exponentiation"
  , PrimUOp Fact ==> "factorial"
  , PrimFloor    ==> "round"
  , PrimCeil     ==> "round"
  , PrimAbs      ==> "abs"
  , PrimUOp Not  ==> "logic-ops"
  , PrimBOp And  ==> "logic-ops"
  , PrimBOp Or   ==> "logic-ops"
  , PrimBOp Impl ==> "logic-ops"
  , PrimBOp Iff  ==> "logic-ops"
  , PrimBOp CartProd ==> "cp"
  ]

otherDoc :: Map String String
otherDoc = M.fromList
  [ "N"          ==> docN
  , "ℕ"          ==> docN
  , "Nat"        ==> docN
  , "Natural"    ==> docN
  , "Z"          ==> docZ
  , "ℤ"          ==> docZ
  , "Int"        ==> docZ
  , "Integer"    ==> docZ
  , "F"          ==> docF
  , "𝔽"          ==> docF
  , "Frac"       ==> docF
  , "Fractional" ==> docF
  , "Q"          ==> docQ
  , "ℚ"          ==> docQ
  , "Rational"   ==> docQ
  , "Bool"       ==> docB
  , "Boolean"    ==> docB
  , "Prop"       ==> "The type of propositions."
  , "Set"        ==> "The type of finite sets."
  , "|~|"        ==> "Absolute value, or the size of a collection."
  ]
  where
    docN = "The type of natural numbers: 0, 1, 2, ..."
    docZ = "The type of integers: ..., -2, -1, 0, 1, 2, ..."
    docF = "The type of fractional numbers p/q >= 0."
    docQ = "The type of rational numbers p/q."
    docB = "The type of Booleans (true or false)."

otherReference :: Map String String
otherReference = M.fromList
  [ "N"          ==> "natural"
  , "ℕ"          ==> "natural"
  , "Nat"        ==> "natural"
  , "Natural"    ==> "natural"
  , "Z"          ==> "integer"
  , "ℤ"          ==> "integer"
  , "Int"        ==> "integer"
  , "Integer"    ==> "integer"
  , "F"          ==> "fraction"
  , "𝔽"          ==> "fraction"
  , "Frac"       ==> "fraction"
  , "Fractional" ==> "fraction"
  , "Q"          ==> "rational"
  , "ℚ"          ==> "rational"
  , "Rational"   ==> "rational"
  , "Bool"       ==> "bool"
  , "Boolean"    ==> "bool"
  , "Prop"       ==> "prop"
  , "Set"        ==> "set"
  , "|~|"        ==> "size"
  ]

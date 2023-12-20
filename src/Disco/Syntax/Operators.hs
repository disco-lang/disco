{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Syntax.Operators
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Unary and binary operators along with information like precedence,
-- fixity, and concrete syntax.
module Disco.Syntax.Operators (
  -- * Operators
  UOp (..),
  BOp (..),
  TyOp (..),

  -- * Operator info
  UFixity (..),
  BFixity (..),
  OpFixity (..),
  OpInfo (..),

  -- * Operator tables and lookup
  opTable,
  uopMap,
  bopMap,
  uPrec,
  bPrec,
  assoc,
  funPrec,
) where

import Data.Data (Data)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Data.Map (Map, (!))
import qualified Data.Map as M

------------------------------------------------------------
-- Operators
------------------------------------------------------------

-- | Unary operators.
data UOp
  = -- | Arithmetic negation (@-@)
    Neg
  | -- | Logical negation (@not@)
    Not
  | -- | Factorial (@!@)
    Fact
  deriving (Show, Read, Eq, Ord, Generic, Data, Alpha, Subst t)

-- | Binary operators.
data BOp
  = -- | Addition (@+@)
    Add
  | -- | Subtraction (@-@)
    Sub
  | -- | Saturating Subtraction (@.-@ / @∸@)
    SSub
  | -- | Multiplication (@*@)
    Mul
  | -- | Division (@/@)
    Div
  | -- | Exponentiation (@^@)
    Exp
  | -- | Integer division (@//@)
    IDiv
  | -- | Equality test (@==@)
    Eq
  | -- | Not-equal (@/=@)
    Neq
  | -- | Less than (@<@)
    Lt
  | -- | Greater than (@>@)
    Gt
  | -- | Less than or equal (@<=@)
    Leq
  | -- | Greater than or equal (@>=@)
    Geq
  | -- | Minimum (@min@)
    Min
  | -- | Maximum (@max@)
    Max
  | -- | Logical and (@&&@ / @and@)
    And
  | -- | Logical or (@||@ / @or@)
    Or
  | -- | Logical implies (@->@ / @implies@)
    Impl
  | -- | Logical biconditional (@<->@ / @iff@)
    Iff
  | -- | Modulo (@mod@)
    Mod
  | -- | Divisibility test (@|@)
    Divides
  | -- | Binomial and multinomial coefficients (@choose@)
    Choose
  | -- | List cons (@::@)
    Cons
  | -- | Cartesian product of sets (@**@ / @⨯@)
    CartProd
  | -- | Union of two sets (@union@ / @∪@)
    Union
  | -- | Intersection of two sets (@intersect@ / @∩@)
    Inter
  | -- | Difference between two sets (@\@)
    Diff
  | -- | Element test (@∈@)
    Elem
  | -- | Subset test (@⊆@)
    Subset
  | -- | Equality assertion (@=!=@)
    ShouldEq
  | -- | Less than assertion (@!<@)
    ShouldLt
  deriving (Show, Read, Eq, Ord, Generic, Data, Alpha, Subst t)

-- | Type operators.
data TyOp
  = -- | List all values of a type
    Enumerate
  | -- | Count how many values there are of a type
    Count
  deriving (Show, Eq, Ord, Generic, Data, Alpha, Subst t)

------------------------------------------------------------
-- Operator info
------------------------------------------------------------

-- | Fixities of unary operators (either pre- or postfix).
data UFixity
  = -- | Unary prefix.
    Pre
  | -- | Unary postfix.
    Post
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Fixity/associativity of infix binary operators (either left,
--   right, or non-associative).
data BFixity
  = -- | Left-associative infix.
    InL
  | -- | Right-associative infix.
    InR
  | -- | Infix.
    In
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Operators together with their fixity.
data OpFixity
  = UOpF UFixity UOp
  | BOpF BFixity BOp
  deriving (Eq, Show, Generic)

-- | An @OpInfo@ record contains information about an operator, such
--   as the operator itself, its fixity, a list of concrete syntax
--   representations, and a numeric precedence level.
data OpInfo = OpInfo
  { opFixity :: OpFixity
  , opSyns :: [String]
  , opPrec :: Int
  }
  deriving (Show)

------------------------------------------------------------
-- Operator table
------------------------------------------------------------

-- | The @opTable@ lists all the operators in the language, in order
--   of precedence (highest precedence first).  Operators in the same
--   list have the same precedence.  This table is used by both the
--   parser and the pretty-printer.
opTable :: [[OpInfo]]
opTable =
  assignPrecLevels
    [
      [ uopInfo Pre Not ["not", "¬"]
      ]
    ,
      [ uopInfo Post Fact ["!"]
      ]
    ,
      [ bopInfo InR Exp ["^"]
      ]
    ,
      [ uopInfo Pre Neg ["-"]
      ]
    ,
      [ bopInfo In Choose ["choose"]
      ]
    ,
      [ bopInfo InR CartProd ["><", "⨯"]
      ]
    ,
      [ bopInfo InL Union ["union", "∪"]
      , bopInfo InL Inter ["intersect", "∩"]
      , bopInfo InL Diff ["\\"]
      ]
    ,
      [ bopInfo InL Min ["min"]
      , bopInfo InL Max ["max"]
      ]
    ,
      [ bopInfo InL Mul ["*"]
      , bopInfo InL Div ["/"]
      , bopInfo InL Mod ["mod", "%"]
      , bopInfo InL IDiv ["//"]
      ]
    ,
      [ bopInfo InL Add ["+"]
      , bopInfo InL Sub ["-"]
      , bopInfo InL SSub [".-", "∸"]
      ]
    ,
      [ bopInfo InR Cons ["::"]
      ]
    ,
      [ bopInfo InR Eq ["=="]
      , bopInfo InR ShouldEq ["=!="]
      , bopInfo InR ShouldLt ["!<"]
      , bopInfo InR Neq ["/=", "≠", "!="]
      , bopInfo InR Lt ["<"]
      , bopInfo InR Gt [">"]
      , bopInfo InR Leq ["<=", "≤", "=<"]
      , bopInfo InR Geq [">=", "≥", "=>"]
      , bopInfo InR Divides ["divides"]
      , bopInfo InL Subset ["subset", "⊆"]
      , bopInfo InL Elem ["elem", "∈"]
      ]
    ,
      [ bopInfo InR And ["/\\", "and", "∧", "&&"]
      ]
    ,
      [ bopInfo InR Or ["\\/", "or", "∨", "||"]
      ]
    ,
      [ bopInfo InR Impl ["->", "==>", "→", "implies"]
      , bopInfo InR Iff ["<->", "<==>", "↔", "iff"]
      ]
    ]
 where
  uopInfo fx op syns = OpInfo (UOpF fx op) syns (-1)
  bopInfo fx op syns = OpInfo (BOpF fx op) syns (-1)

  -- Start at precedence level 2 so we can give level 1 to ascription, and level 0
  -- to the ambient context + parentheses etc.
  assignPrecLevels table = zipWith assignPrecs (reverse [2 .. length table + 1]) table
  assignPrec p op = op {opPrec = p}
  assignPrecs p = map (assignPrec p)

-- | A map from all unary operators to their associated 'OpInfo' records.
uopMap :: Map UOp OpInfo
uopMap =
  M.fromList $
    [(op, info) | opLevel <- opTable, info@(OpInfo (UOpF _ op) _ _) <- opLevel]

-- | A map from all binary operators to their associatied 'OpInfo' records.
bopMap :: Map BOp OpInfo
bopMap =
  M.fromList $
    [(op, info) | opLevel <- opTable, info@(OpInfo (BOpF _ op) _ _) <- opLevel]

-- | A convenient function for looking up the precedence of a unary operator.
uPrec :: UOp -> Int
uPrec = opPrec . (uopMap !)

-- | A convenient function for looking up the precedence of a binary operator.
bPrec :: BOp -> Int
bPrec = opPrec . (bopMap !)

-- | Look up the \"fixity\" (/i.e./ associativity) of a binary operator.
assoc :: BOp -> BFixity
assoc op =
  case M.lookup op bopMap of
    Just (OpInfo (BOpF fx _) _ _) -> fx
    _ -> error $ "BOp " ++ show op ++ " not in bopMap!"

-- | The precedence level of function application (higher than any
--   other precedence level).
funPrec :: Int
funPrec = length opTable + 1

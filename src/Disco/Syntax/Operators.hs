{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Syntax.Operators
-- Copyright   :  (c) 2017 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Unary and binary operators along with information like precedence,
-- fixity, and concrete syntax.
--
-----------------------------------------------------------------------------

module Disco.Syntax.Operators
       ( -- * Operators
         UOp(..), BOp(..), TyOp(..)

         -- * Operator info
       , UFixity(..), BFixity(..), OpFixity(..), OpInfo(..)

         -- * Operator tables and lookup
       , opTable, uopMap, bopMap
       , uPrec, bPrec, assoc, funPrec

       ) where

import           GHC.Generics                     (Generic)
import           Unbound.Generics.LocallyNameless

import           Data.Map                         (Map, (!))
import qualified Data.Map                         as M

------------------------------------------------------------
-- Operators
------------------------------------------------------------

-- | Unary operators.
data UOp = Neg   -- ^ Arithmetic negation (@-@)
         | Not   -- ^ Logical negation (@not@)
         | Fact  -- ^ Factorial (@!@)
         | Sqrt  -- ^ Integer square root (@sqrt@)
         | Lg    -- ^ Floor of base-2 logarithm (@lg@)
         | Floor -- ^ Floor of fractional type (@floor@)
         | Ceil  -- ^ Ceiling of fractional type (@ceiling@)
         | Abs   -- ^ Absolute value (@abs@)
  deriving (Show, Eq, Ord, Generic)

-- | Binary operators.
data BOp = Add     -- ^ Addition (@+@)
         | Sub     -- ^ Subtraction (@-@)
         | Mul     -- ^ Multiplication (@*@)
         | Div     -- ^ Division (@/@)
         | Exp     -- ^ Exponentiation (@^@)
         | IDiv    -- ^ Integer division (@//@)
         | Eq      -- ^ Equality test (@==@)
         | Neq     -- ^ Not-equal (@/=@)
         | Lt      -- ^ Less than (@<@)
         | Gt      -- ^ Greater than (@>@)
         | Leq     -- ^ Less than or equal (@<=@)
         | Geq     -- ^ Greater than or equal (@>=@)
         | And     -- ^ Logical and (@&&@ / @and@)
         | Or      -- ^ Logical or (@||@ / @or@)
         | Impl    -- ^ Logical implies (@==>@ / @implies@)
         | Mod     -- ^ Modulo (@mod@)
         | Divides -- ^ Divisibility test (@|@)
         | Choose  -- ^ Binomial and multinomial coefficients (@choose@)
         | Cons    -- ^ List cons (@::@)
  deriving (Show, Eq, Ord, Generic)

-- | Type operators.
data TyOp = Enumerate -- ^ List all values of a type
          | Count     -- ^ Count how many values there are of a type
  deriving (Show, Eq, Generic)

instance Alpha UOp
instance Alpha BOp
instance Alpha TyOp

instance Subst t UOp
instance Subst t BOp
instance Subst t TyOp

------------------------------------------------------------
-- Operator info
------------------------------------------------------------

-- | Fixities of unary operators (either pre- or postfix).
data UFixity
  = Pre     -- ^ Unary prefix.
  | Post    -- ^ Unary postfix.
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Fixity of infix binary operators (either left, right, or non-associative).
data BFixity
  = InL   -- ^ Left-associative infix.
  | InR   -- ^ Right-associative infix.
  | In    -- ^ Infix.
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Operators together with their fixity.
data OpFixity =
    UOpF UFixity UOp
  | BOpF BFixity BOp
  deriving (Eq, Show, Generic)

-- | An @OpInfo@ record contains information about an operator, such
--   as the operator itself, its fixity, a list of concrete syntax
--   representations of the operator, and a numeric precedence level.
data OpInfo =
  OpInfo
  { opFixity :: OpFixity
  , opSyns   :: [String]
  , opPrec   :: Int
  }
  deriving Show

------------------------------------------------------------
-- Operator table
------------------------------------------------------------

-- | The @opTable@ lists all the operators in the language, in order
--   of precedence (highest precedence first).  Operators in the same
--   list have the same precedence.  This table is used by both the
--   parser and the pretty-printer.
opTable :: [[OpInfo]]
opTable =
  assignPrecLevels $
  [ [ uopInfo Pre  Not     ["not", "¬"]
    ]
  , [ uopInfo Post Fact    ["!"]
    ]
  , [ bopInfo InR  Exp     ["^"]
    ]
  , [ uopInfo Pre  Neg     ["-"]
    ]
  , [ uopInfo Pre  Sqrt    ["sqrt"]
    ]
  , [ uopInfo Pre  Lg      ["lg"]
    ]
  , [ uopInfo Pre  Floor   ["floor"]
    , uopInfo Pre  Ceil    ["ceiling"]
    , uopInfo Pre  Abs     ["abs"]
    ]
  , [ bopInfo In   Choose   ["choose"]
    ]
  , [ bopInfo InL  Mul     ["*"]
    , bopInfo InL  Div     ["/"]
    , bopInfo InL  Mod     ["%"]
    , bopInfo InL  Mod     ["mod"]
    , bopInfo InL  IDiv    ["//"]
    ]
  , [ bopInfo InL  Add     ["+"]
    , bopInfo InL  Sub     ["-"]
    ]
  , [ bopInfo InR  Cons    ["::"]
    ]
  , [ bopInfo InR  Eq      ["="]
    , bopInfo InR  Neq     ["≠", "/="]
    , bopInfo InR  Lt      ["<"]
    , bopInfo InR  Gt      [">"]
    , bopInfo InR  Leq     ["≤", "<="]
    , bopInfo InR  Geq     ["≥", ">="]
    , bopInfo InR  Divides ["divides"]
    ]
  , [ bopInfo InR  And     ["and", "∧", "&&"]
    ]
  , [ bopInfo InR  Or      ["or", "∨", "||"]
    ]
  , [ bopInfo InR Impl     ["==>", "implies"]
    ]
  ]
  where
    uopInfo fx op syns = OpInfo (UOpF fx op) syns (-1)
    bopInfo fx op syns = OpInfo (BOpF fx op) syns (-1)

    assignPrecLevels table = zipWith assignPrecs (reverse [1 .. length table]) table
    assignPrecs p ops      = map (assignPrec p) ops
    assignPrec  p op       = op { opPrec = p }

-- | A map from all unary operators to their associated 'OpInfo' records.
uopMap :: Map UOp OpInfo
uopMap = M.fromList $
  [ (op, info) | opLevel <- opTable, info@(OpInfo (UOpF _ op) _ _) <- opLevel ]

-- | A map from all binary operators to their associatied 'OpInfo' records.
bopMap :: Map BOp OpInfo
bopMap = M.fromList $
  [ (op, info) | opLevel <- opTable, info@(OpInfo (BOpF _ op) _ _) <- opLevel ]

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
    _                             -> error $ "BOp " ++ show op ++ " not in bopMap!"

-- | The precedence level of function application (higher than any
--   other precedence level).
funPrec :: Int
funPrec = length opTable


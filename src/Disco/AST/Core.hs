{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Disco.AST.Core
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax trees representing the desugared, untyped core
-- language for Disco.
module Disco.AST.Core (
  -- * Core AST
  RationalDisplay (..),
  Core (..),
  Op (..),
  opArity,
  substQC,
  substsQC,
)
where

import Control.Lens.Plated
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import qualified Data.Set as S
import GHC.Generics
import Unbound.Generics.LocallyNameless hiding (LFresh, lunbind)
import Prelude hiding ((<>))
import qualified Prelude as P

import Disco.Effects.LFresh
import Polysemy (Members, Sem)
import Polysemy.Reader

import Data.Ratio
import Disco.AST.Generic (Side, selectSide)
import Disco.Names (QName)
import Disco.Pretty
import Disco.Types

-- | A type of flags specifying whether to display a rational number
--   as a fraction or a decimal.
data RationalDisplay = Fraction | Decimal
  deriving (Eq, Show, Generic, Data, Ord, Alpha)

instance Semigroup RationalDisplay where
  Decimal <> _ = Decimal
  _ <> Decimal = Decimal
  _ <> _ = Fraction

-- | The 'Monoid' instance for 'RationalDisplay' corresponds to the
--   idea that the result should be displayed as a decimal if any
--   decimal literals are used in the input; otherwise, the default is
--   to display as a fraction.  So the identity element is 'Fraction',
--   and 'Decimal' always wins when combining.
instance Monoid RationalDisplay where
  mempty = Fraction
  mappend = (P.<>)

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

    -- | Power set/bag of a given set/bag
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
    OEmptyGraph
  | -- | Construct a vertex with given value
    OVertex
  | -- | Graph overlay
    OOverlay
  | -- | Graph connect
    OConnect
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
  | -- | unsafe set of counts -> bag, assumes all are distinct
    OUnsafeCountsToBag
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
    OShouldLt Type
  | -- | Error for non-exhaustive pattern match
    OMatchErr
  | -- | Crash with a user-supplied message
    OCrash
  | -- | No-op/identity function
    OId
  | -- | Lookup OEIS sequence
    OLookupSeq
  | -- | Extend a List via OEIS
    OExtendSeq
  | -- | Not the Boolean `And`, but instead a propositional BOp
    -- | Should only be seen and used with Props.
    OAnd
  | -- | Not the Boolean `Or`, but instead a propositional BOp
    -- | Should only be seen and used with Props.
    OOr
  | -- | Not the Boolean `Impl`, but instead a propositional BOp
    -- | Should only be seen and used with Props.
    OImpl
  deriving (Show, Generic, Data, Alpha, Eq, Ord)

-- | Get the arity (desired number of arguments) of a function
--   constant.  A few constants have arity 0; everything else is
--   uncurried and hence has arity 1.
opArity :: Op -> Int
opArity OEmptyGraph = 0
opArity OMatchErr = 0
opArity _ = 1

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
    _ -> CVar y
  t -> t

instance Pretty Core where
  pretty = \case
    CVar qn -> pretty qn
    CNum _ r
      | denominator r == 1 -> text (show (numerator r))
      | otherwise -> text (show (numerator r)) <> "/" <> text (show (denominator r))
    CApp (CConst op) (CPair c1 c2)
      | isInfix op -> parens (pretty c1 <+> text (opToStr op) <+> pretty c2)
    CApp (CConst op) c
      | isPrefix op -> text (opToStr op) <> pretty c
      | isPostfix op -> pretty c <> text (opToStr op)
    CConst op -> pretty op
    CInj s c -> withPA funPA $ selectSide s "left" "right" <+> rt (pretty c)
    CCase c l r -> do
      lunbind l $ \(x, lc) -> do
        lunbind r $ \(y, rc) -> do
          "case"
            <+> pretty c
            <+> "of {"
            $+$ nest
              2
              ( vcat
                  [ withPA funPA $ "left" <+> rt (pretty x) <+> "->" <+> pretty lc
                  , withPA funPA $ "right" <+> rt (pretty y) <+> "->" <+> pretty rc
                  ]
              )
            $+$ "}"
    CUnit -> "unit"
    CPair c1 c2 -> setPA initPA $ parens (pretty c1 <> ", " <> pretty c2)
    CProj s c -> withPA funPA $ selectSide s "fst" "snd" <+> rt (pretty c)
    CAbs lam -> withPA initPA $ do
      lunbind lam $ \(xs, body) -> "λ" <> intercalate "," (map pretty xs) <> "." <+> lt (pretty body)
    CApp c1 c2 -> withPA funPA $ lt (pretty c1) <+> rt (pretty c2)
    CTest xs c -> "test" <+> prettyTestVars xs <+> pretty c
    CType ty -> pretty ty
    CDelay d -> withPA initPA $ do
      lunbind d $ \(xs, bodies) ->
        "delay" <+> intercalate "," (map pretty xs) <> "." <+> pretty (toTuple bodies)
    CForce c -> withPA funPA $ "force" <+> rt (pretty c)

toTuple :: [Core] -> Core
toTuple = foldr CPair CUnit

prettyTestVars :: Members '[Reader PA, LFresh] r => [(String, Type, Name Core)] -> Sem r Doc
prettyTestVars = brackets . intercalate "," . map prettyTestVar
 where
  prettyTestVar (s, ty, n) = parens (intercalate "," [text s, pretty ty, pretty n])

isInfix, isPrefix, isPostfix :: Op -> Bool
isInfix OShouldEq {} = True
isInfix OShouldLt {} = True
isInfix op =
  op
    `S.member` S.fromList
      [OAdd, OMul, ODiv, OExp, OMod, ODivides, OMultinom, OEq, OLt, OAnd, OOr, OImpl]
isPrefix ONeg = True
isPrefix _ = False
isPostfix OFact = True
isPostfix _ = False

instance Pretty Op where
  pretty (OForall tys) = "∀" <> intercalate "," (map pretty tys) <> "."
  pretty (OExists tys) = "∃" <> intercalate "," (map pretty tys) <> "."
  pretty op
    | isInfix op = "~" <> text (opToStr op) <> "~"
    | isPrefix op = text (opToStr op) <> "~"
    | isPostfix op = "~" <> text (opToStr op)
    | otherwise = text (opToStr op)

opToStr :: Op -> String
opToStr = \case
  OAdd -> "+"
  ONeg -> "-"
  OSqrt -> "sqrt"
  OFloor -> "floor"
  OCeil -> "ceil"
  OAbs -> "abs"
  OMul -> "*"
  ODiv -> "/"
  OExp -> "^"
  OMod -> "mod"
  ODivides -> "divides"
  OMultinom -> "choose"
  OFact -> "!"
  OEq -> "=="
  OLt -> "<"
  OEnum -> "enumerate"
  OCount -> "count"
  OPower -> "power"
  OBagElem -> "elem_bag"
  OListElem -> "elem_list"
  OEachBag -> "each_bag"
  OEachSet -> "each_set"
  OFilterBag -> "filter_bag"
  OMerge -> "merge"
  OBagUnions -> "unions_bag"
  OSummary -> "summary"
  OEmptyGraph -> "emptyGraph"
  OVertex -> "vertex"
  OOverlay -> "overlay"
  OConnect -> "connect"
  OInsert -> "insert"
  OLookup -> "lookup"
  OUntil -> "until"
  OSetToList -> "set2list"
  OBagToSet -> "bag2set"
  OBagToList -> "bag2list"
  OListToSet -> "list2set"
  OListToBag -> "list2bag"
  OBagToCounts -> "bag2counts"
  OCountsToBag -> "counts2bag"
  OUnsafeCountsToBag -> "ucounts2bag"
  OMapToSet -> "map2set"
  OSetToMap -> "set2map"
  OIsPrime -> "isPrime"
  OFactor -> "factor"
  OFrac -> "frac"
  OHolds -> "holds"
  ONotProp -> "not"
  OShouldEq _ -> "=!="
  OShouldLt _ -> "!<"
  OMatchErr -> "matchErr"
  OCrash -> "crash"
  OId -> "id"
  OLookupSeq -> "lookupSeq"
  OExtendSeq -> "extendSeq"
  OForall {} -> "∀"
  OExists {} -> "∃"
  OAnd -> "and"
  OOr -> "or"
  OImpl -> "implies"

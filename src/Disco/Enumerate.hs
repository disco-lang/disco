{-# LANGUAGE NondecreasingIndentation #-}

-- |
-- Module      :  Disco.Enumerate
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Enumerate values inhabiting Disco types.
module Disco.Enumerate (
  ValueEnumeration,

  -- * Base types
  enumVoid,
  enumUnit,
  enumBool,
  enumN,
  enumZ,
  enumF,
  enumQ,
  enumC,

  -- * Containers
  enumSet,
  --  , enumBag
  enumList,

  -- * Any type
  enumType,
  enumTypes,

  -- * Lifted functions that return lists
  enumerateType,
  enumerateTypes,
)
where

import qualified Data.Enumeration.Invertible as E
import Disco.AST.Generic (Side (..))
import Disco.Types
import Disco.Value

type ValueEnumeration = E.IEnumeration Value

-- | Enumerate all values of type @Void@ (none).
enumVoid :: ValueEnumeration
enumVoid = E.void

-- | Enumerate all values of type @Unit@ (the single value @unit@).
enumUnit :: ValueEnumeration
enumUnit = E.singleton VUnit

-- | Enumerate the values of type @Bool@ as @[false, true]@.
enumBool :: ValueEnumeration
enumBool = E.mapE toV fromV $ E.finiteList [L, R]
 where
  toV i = VInj i VUnit
  fromV (VInj i VUnit) = i
  fromV _ = error "enumBool.fromV: value isn't a bool"

-- | Enumerate all values of type @Nat@ (0, 1, 2, ...).
enumN :: ValueEnumeration
enumN = E.mapE (ratv . fromInteger) (floor . vrat) E.nat

-- | Enumerate all values of type @Integer@ (0, 1, -1, 2, -2, ...).
enumZ :: ValueEnumeration
enumZ = E.mapE (ratv . fromInteger) (floor . vrat) E.int

-- | Enumerate all values of type @Fractional@ in the Calkin-Wilf
--   order (1, 1/2, 2, 1/3, 3/2, 2/3, 3, ...).
enumF :: ValueEnumeration
enumF = E.mapE ratv vrat E.cw

-- | Enumerate all values of type @Rational@ in the Calkin-Wilf order,
--   with negatives interleaved (0, 1, -1, 1/2, -1/2, 2, -2, ...).
enumQ :: ValueEnumeration
enumQ = E.mapE ratv vrat E.rat

-- | Enumerate all Unicode characters.
enumC :: ValueEnumeration
enumC = E.mapE toV fromV (E.boundedEnum @Char)
 where
  toV = ratv . fromIntegral . fromEnum
  fromV = toEnum . floor . vrat

-- | Enumerate all *finite* sets over a certain element type, given an
--   enumeration of the elements.  If we think of each finite set as a
--   binary string indicating which elements in the enumeration are
--   members, the sets are enumerated in order of the binary strings.
enumSet :: ValueEnumeration -> ValueEnumeration
enumSet e = E.mapE toV fromV (E.finiteSubsetOf e)
 where
  toV = VBag . map (,1)
  fromV (VBag vs) = map fst vs
  fromV _ = error "enumSet.fromV: value isn't a set"

-- | Enumerate all *finite* lists over a certain element type, given
--   an enumeration of the elements.  It is very difficult to describe
--   the order in which the lists are generated.
enumList :: ValueEnumeration -> ValueEnumeration
enumList e = E.mapE toV fromV (E.listOf e)
 where
  toV = foldr VCons VNil
  fromV (VCons h t) = h : fromV t
  fromV VNil = []
  fromV _ = error "enumList.fromV: value isn't a list"

-- | Enumerate all functions from a finite domain, given enumerations
--   for the domain and codomain.
enumFunction :: ValueEnumeration -> ValueEnumeration -> ValueEnumeration
enumFunction xs ys =
  case (E.card xs, E.card ys) of
    (E.Finite 0, _) -> E.singleton (VFun $ \_ -> error "enumFunction: void function called")
    (_, E.Finite 0) -> E.void
    (_, E.Finite 1) -> E.singleton (VFun $ \_ -> E.select ys 0)
    _ -> E.mapE toV fromV (E.functionOf xs ys)
 where
  -- XXX TODO: better error message on functions with an infinite domain

  toV = VFun
  fromV (VFun f) = f
  fromV _ = error "enumFunction.fromV: value isn't a VFun"

-- | Enumerate all values of a product type, given enumerations of the
--   two component types.  Uses a fair interleaving for infinite
--   component types.
enumProd :: ValueEnumeration -> ValueEnumeration -> ValueEnumeration
enumProd xs ys = E.mapE toV fromV $ (E.><) xs ys
 where
  toV (x, y) = VPair x y
  fromV (VPair x y) = (x, y)
  fromV _ = error "enumProd.fromV: value isn't a pair"

-- | Enumerate all values of a sum type, given enumerations of the two
--   component types.
enumSum :: ValueEnumeration -> ValueEnumeration -> ValueEnumeration
enumSum xs ys = E.mapE toV fromV $ (E.<+>) xs ys
 where
  toV (Left x) = VInj L x
  toV (Right y) = VInj R y
  fromV (VInj L x) = Left x
  fromV (VInj R y) = Right y
  fromV _ = error "enumSum.fromV: value isn't a sum"

-- | Enumerate the values of a given type.
enumType :: Type -> ValueEnumeration
enumType TyVoid = enumVoid
enumType TyUnit = enumUnit
enumType TyBool = enumBool
enumType TyN = enumN
enumType TyZ = enumZ
enumType TyF = enumF
enumType TyQ = enumQ
enumType TyC = enumC
enumType (TySet t) = enumSet (enumType t)
enumType (TyList t) = enumList (enumType t)
enumType (a :*: b) = enumProd (enumType a) (enumType b)
enumType (a :+: b) = enumSum (enumType a) (enumType b)
enumType (a :->: b) = enumFunction (enumType a) (enumType b)
enumType ty = error $ "enumType: can't enumerate " ++ show ty

-- | Enumerate a finite product of types.
enumTypes :: [Type] -> E.IEnumeration [Value]
enumTypes [] = E.singleton []
enumTypes (t : ts) = E.mapE toL fromL $ (E.><) (enumType t) (enumTypes ts)
 where
  toL (x, xs) = x : xs
  fromL (x : xs) = (x, xs)
  fromL [] = error "enumTypes.fromL: empty list not in enumeration range"

-- | Produce an actual list of the values of a type.
enumerateType :: Type -> [Value]
enumerateType = E.enumerate . enumType

-- | Produce an actual list of values enumerated from a finite product
--   of types.
enumerateTypes :: [Type] -> [[Value]]
enumerateTypes = E.enumerate . enumTypes

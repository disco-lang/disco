-- |
-- Module      :  Disco.Exhaustiveness.TypeInfo
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for converting Disco types into 'Constructors'
-- that the exhaustiveness checker understands, and utilities
-- for working with 'TypedVar's and their names.
module Disco.Exhaustiveness.TypeInfo where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List ((\\))
import qualified Data.Map as M
import Disco.AST.Typed (ATerm)
import Disco.Effects.Fresh (Fresh, fresh)
import qualified Disco.Types as Ty
import Polysemy
import Unbound.Generics.LocallyNameless (Name, s2n)

newtype TypedVar = TypedVar (Name ATerm, Ty.Type)
  deriving (Show, Ord)

instance Eq TypedVar where
  TypedVar (n1, _) == TypedVar (n2, _) = n1 == n2

getType :: TypedVar -> Ty.Type
getType (TypedVar (_, t)) = t

data DataCon = DataCon
  { dcIdent :: Ident,
    dcTypes :: [Ty.Type]
  }
  deriving (Ord, Show)

-- This is very important, as we have (sometimes recursive) type aliases
-- Equality of dcTypes doesn't measure equality of dataconstructors,
-- because we could have two different aliases for the same type
-- (And we can't just resolve all aliases up front, because they can be recursive)
instance Eq DataCon where
  (==) = (==) `on` dcIdent

data Ident where
  KUnit :: Ident
  KBool :: Bool -> Ident
  KNat :: Integer -> Ident
  KInt :: Integer -> Ident
  KPair :: Ident
  KCons :: Ident
  KNil :: Ident
  KChar :: Char -> Ident
  KLeft :: Ident
  KRight :: Ident
  KUnknown :: Ident
  deriving (Eq, Ord)

instance Show Ident where
  show i = case i of
    KBool b -> show b
    KChar c -> show c
    KNat n -> show n
    KInt z ->
      if z < 0
        then "(" ++ show z ++ ")"
        else show z
    KNil -> "[]"
    KUnit -> "unit"
    KUnknown -> "_"
    -- These should never actually be printed in warnings
    KPair -> ","
    KCons -> "::"
    KLeft -> "left()"
    KRight -> "right()"

-- | 'Finite' constructors are used in the LYG checker
--   'Infinite' constructors are used when reporting
--   examples of uncovered patterns, we only pick out a few of them
data Constructors where
  Finite :: [DataCon] -> Constructors
  Infinite :: [DataCon] -> Constructors

unknown :: DataCon
unknown = DataCon {dcIdent = KUnknown, dcTypes = []}

unit :: DataCon
unit = DataCon {dcIdent = KUnit, dcTypes = []}

bool :: Bool -> DataCon
bool b = DataCon {dcIdent = KBool b, dcTypes = []}

natural :: Integer -> DataCon
natural n = DataCon {dcIdent = KNat n, dcTypes = []}

integer :: Integer -> DataCon
integer z = DataCon {dcIdent = KInt z, dcTypes = []}

char :: Char -> DataCon
char c = DataCon {dcIdent = KChar c, dcTypes = []}

cons :: Ty.Type -> Ty.Type -> DataCon
cons tHead tTail = DataCon {dcIdent = KCons, dcTypes = [tHead, tTail]}

nil :: DataCon
nil = DataCon {dcIdent = KNil, dcTypes = []}

pair :: Ty.Type -> Ty.Type -> DataCon
pair a b = DataCon {dcIdent = KPair, dcTypes = [a, b]}

left :: Ty.Type -> DataCon
left tl = DataCon {dcIdent = KLeft, dcTypes = [tl]}

right :: Ty.Type -> DataCon
right tr = DataCon {dcIdent = KRight, dcTypes = [tr]}

tyDataCons :: Ty.Type -> Ty.TyDefCtx -> Constructors
tyDataCons ty ctx = tyDataConsHelper $ resolveAlias ty ctx

-- Type aliases that would cause infinite recursion here are
-- not possible to construct, so we don't have to worry about that.
-- (aka cyclic type definitions are not allowed in Disco)
resolveAlias :: Ty.Type -> Ty.TyDefCtx -> Ty.Type
resolveAlias (Ty.TyUser name args) ctx = case M.lookup name ctx of
  Nothing -> error $ show ctx ++ "\nType definition not found for: " ++ show name
  Just (Ty.TyDefBody _argNames typeCon) -> resolveAlias (typeCon args) ctx
resolveAlias t _ = t

-- | Assuming type aliases have been resolved, this
--   function converts Disco types into lists of DataCons
--   that are compatible with the LYG checker.
--
--   A list of constructors is 'Infinite' if the only way to fully
--   match against the type is with a wildcard or variable pattern.
--   Otherwise, it is 'Finite'.
--
--   The LYG checker only reads the list of constructors if
--   a type is 'Finite'. From the point of view of the checker,
--   'Infinite' is a synonym for opaque, and the constructors are discarded.
--   The dataconstructors in an `Infinite` list are only
--   used when generating the 3 positive examples of what
--   you haven't matched against.
--   This will probably need to change a bit when bringing
--   exhaustiveness checking to the new arithmetic patterns.
--
--   Notice the last case of this function, which a wildcard handling the types:
--   (_ Ty.:->: _) (Ty.TySet _) (Ty.TyBag _) (Ty.TyVar _)
--   (Ty.TySkolem _) (Ty.TyProp) (Ty.TyMap _ _) (Ty.TyGraph _)
--
--   I believe all of these are impossible to pattern match against
--   with anything other than a wildcard (or variable pattern) in Disco, so they should always
--   be fully covered. But if they are in a pair, for example, Set(Int)*Int,
--   we still need to generate 3 examples of the pair if that Int part isn't covered.
--   So how do we fill the concrete part of Set(Int), (or a generic type "a", or a function, etc.)?
--   I'm calling that 'unknown', and printing an underscore.
--   (Also, I'm using 'Infinite' for reasons metioned above).
tyDataConsHelper :: Ty.Type -> Constructors
tyDataConsHelper (a Ty.:*: b) = Finite [pair a b]
tyDataConsHelper (l Ty.:+: r) = Finite [left l, right r]
tyDataConsHelper t@(Ty.TyList a) = Finite [nil, cons a t]
tyDataConsHelper Ty.TyVoid = Finite []
tyDataConsHelper Ty.TyUnit = Finite [unit]
tyDataConsHelper Ty.TyBool = Finite [bool True, bool False]
tyDataConsHelper Ty.TyN = Infinite $ map natural [0, 1 ..]
-- Many thanks to this answer and its comment for a convenient way to list the integers
-- https://stackoverflow.com/a/9749957
tyDataConsHelper Ty.TyZ = Infinite $ map integer $ 0 : [y | x <- [1 ..], y <- [x, -x]]
tyDataConsHelper Ty.TyF = Infinite []
tyDataConsHelper Ty.TyQ = Infinite []
-- The Char constructors are all unicode characters, but
-- given in a very particular order that I think will
-- make the most sense for students.
-- Many thanks to Dr. Yorgey for mentioning [minBound .. maxBound] and \\
tyDataConsHelper Ty.TyC =
  Infinite $
    map char $
      alphanum ++ (allUnicodeNicelyOrdered \\ alphanum)
  where
    allUnicodeNicelyOrdered = [(toEnum 32) .. (toEnum 126)] ++ [(toEnum 161) .. maxBound] ++ [minBound .. (toEnum 31)] ++ [(toEnum 127) .. (toEnum 160)]
    alphanum = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
tyDataConsHelper _ = Infinite [unknown]

newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newVar :: (Member Fresh r) => Ty.Type -> Sem r TypedVar
newVar types = do
  names <- newName
  return $ TypedVar (names, types)

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

newVars :: (Member Fresh r) => [Ty.Type] -> Sem r [TypedVar]
newVars types = do
  names <- newNames (length types)
  return $ zipWith (curry TypedVar) names types

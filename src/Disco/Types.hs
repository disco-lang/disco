{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      :  Disco.Types
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- The "Disco.Types" module defines the set of types used in the disco
-- language type system, along with various utility functions.
module Disco.Types (
  -- * Disco language types

  -- ** Atomic types
  BaseTy (..),
  isCtr,
  Var (..),
  Ilk (..),
  pattern U,
  pattern S,
  Atom (..),
  isVar,
  isBase,
  isSkolem,
  UAtom (..),
  uisVar,
  uatomToAtom,
  uatomToEither,

  -- ** Type constructors
  Con (..),
  pattern CList,
  pattern CBag,
  pattern CSet,

  -- ** Types
  Type (..),
  pattern TyVar,
  pattern TySkolem,
  pattern TyVoid,
  pattern TyUnit,
  pattern TyBool,
  pattern TyProp,
  pattern TyN,
  pattern TyZ,
  pattern TyF,
  pattern TyQ,
  pattern TyC,
  -- , pattern TyFin
  pattern (:->:),
  pattern (:*:),
  pattern (:+:),
  pattern TyList,
  pattern TyBag,
  pattern TySet,
  pattern TyGraph,
  pattern TyMap,
  pattern TyContainer,
  pattern TyUser,
  pattern TyString,

  -- ** Quantified types
  PolyType (..),
  toPolyType,
  closeType,

  -- * Type predicates
  isNumTy,
  isEmptyTy,
  isFiniteTy,
  isSearchable,

  -- * Type substitutions
  Substitution,
  atomToTypeSubst,
  uatomToTypeSubst,

  -- * Strictness
  Strictness (..),
  strictness,

  -- * Utilities
  isTyVar,
  containerVars,
  countType,
  unpair,
  S,
  TyDefBody (..),
  TyDefCtx,

  -- * HasType class
  HasType (..),
)
where

import Data.Coerce
import Data.Data (Data)
import Disco.Data ()
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless hiding (lunbind)

import Control.Lens (toListOf)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Math.Combinatorics.Exact.Binomial (choose)

import Disco.Effects.LFresh

import Disco.Pretty hiding ((<>))
import Disco.Subst (Substitution)
import Disco.Types.Qualifiers

--------------------------------------------------
-- Disco types
--------------------------------------------------

----------------------------------------
-- Base types

-- | Base types are the built-in types which form the basis of the
--   disco type system, out of which more complex types can be built.
data BaseTy where
  -- | The void type, with no inhabitants.
  Void :: BaseTy
  -- | The unit type, with one inhabitant.
  Unit :: BaseTy
  -- | Booleans.
  B :: BaseTy
  -- | Propositions.
  P :: BaseTy
  -- | Natural numbers.
  N :: BaseTy
  -- | Integers.
  Z :: BaseTy
  -- | Fractionals (i.e. nonnegative rationals).
  F :: BaseTy
  -- | Rationals.
  Q :: BaseTy
  -- | Unicode characters.
  C :: BaseTy
  -- Finite types. The single argument is a natural number defining
  -- the exact number of inhabitants.
  -- Fin  :: Integer -> BaseTy

  -- | Set container type.  It's a bit odd putting these here since
  --   they have kind * -> * and all the other base types have kind *;
  --   but there's nothing fundamentally wrong with it and in
  --   particular this allows us to reuse all the existing constraint
  --   solving machinery for container subtyping.
  CtrSet :: BaseTy
  -- | Bag container type.
  CtrBag :: BaseTy
  -- | List container type.
  CtrList :: BaseTy
  deriving (Show, Eq, Ord, Generic, Data, Alpha, Subst BaseTy, Subst Atom, Subst UAtom, Subst Type)

instance Pretty BaseTy where
  pretty = \case
    Void -> text "Void"
    Unit -> text "Unit"
    B -> text "Bool"
    P -> text "Prop"
    N -> text "ℕ"
    Z -> text "ℤ"
    Q -> text "ℚ"
    F -> text "𝔽"
    C -> text "Char"
    CtrList -> text "List"
    CtrBag -> text "Bag"
    CtrSet -> text "Set"

-- | Test whether a 'BaseTy' is a container (set, bag, or list).
isCtr :: BaseTy -> Bool
isCtr = (`elem` [CtrSet, CtrBag, CtrList])

----------------------------------------
-- Type variables

-- | 'Var' represents /type variables/, that is, variables which stand
--   for some type. There are two kinds of type variables:
--
--   * /Unification variables/ stand for an unknown type, about which
--     we might learn additional information during the typechecking
--     process.  For example, given a function of type @List a -> List
--     a@, if we typecheck an application of the function to the list
--     @[1,2,3]@, we would learn that @List a@ has to be @List N@, and
--     hence that @a@ has to be @N@.
--
--   * /Skolem variables/ stand for a fixed generic type, and are used
--     to typecheck universally quantified type signatures (/i.e./
--     type signatures which contain type variables).  For example, if
--     a function has the declared type @List a -> N@, it amounts to a
--     claim that the function will work no matter what type is
--     substituted for @a@. We check this by making up a new skolem
--     variable for @a@.  Skolem variables are equal to themselves,
--     but nothing else.  In contrast to a unification variable,
--     "learning something" about a skolem variable is an error: it
--     means that the function will only work for certain types, in
--     contradiction to its claim to work for any type at all.
data Ilk = Skolem | Unification
  deriving (Eq, Ord, Read, Show, Generic, Data, Alpha, Subst Atom, Subst Type)

instance Pretty Ilk where
  pretty = \case
    Skolem -> "S"
    Unification -> "U"

-- | 'Var' represents /type variables/, that is, variables which stand
--   for some type.
data Var where
  V :: Ilk -> Name Type -> Var
  deriving (Show, Eq, Ord, Generic, Data, Alpha, Subst Atom, Subst Type)

pattern U :: Name Type -> Var
pattern U v = V Unification v

pattern S :: Name Type -> Var
pattern S v = V Skolem v

{-# COMPLETE U, S #-}

----------------------------------------
-- Atomic types

-- | An /atomic type/ is either a base type or a type variable.  The
--   alternative is a /compound type/ which is built out of type
--   constructors.  The reason we split out the concept of atomic
--   types into its own data type 'Atom' is because constraints
--   involving compound types can always be simplified/translated into
--   constraints involving only atomic types.  After that
--   simplification step, we want to be able to work with collections
--   of constraints that are guaranteed to contain only atomic types.
data Atom where
  AVar :: Var -> Atom
  ABase :: BaseTy -> Atom
  deriving (Show, Eq, Ord, Generic, Data, Alpha, Subst Type)

instance Subst Atom Atom where
  isvar (AVar (U x)) = Just (SubstName (coerce x))
  isvar _ = Nothing

instance Pretty Atom where
  pretty = \case
    AVar (U v) -> pretty v
    AVar (S v) -> text "$" <> pretty v
    ABase b -> pretty b

-- | Is this atomic type a variable?
isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _ = False

-- | Is this atomic type a base type?
isBase :: Atom -> Bool
isBase = not . isVar

-- | Is this atomic type a skolem variable?
isSkolem :: Atom -> Bool
isSkolem (AVar (S _)) = True
isSkolem _ = False

-- | /Unifiable/ atomic types are the same as atomic types but without
--   skolem variables.  Hence, a unifiable atomic type is either a base
--   type or a unification variable.
--
--   Again, the reason this has its own type is that at some stage of
--   the typechecking/constraint solving process, these should be the
--   only things around; we can get rid of skolem variables because
--   either they impose no constraints, or result in an error if they
--   are related to something other than themselves.  After checking
--   these things, we can just focus on base types and unification
--   variables.
data UAtom where
  UB :: BaseTy -> UAtom
  UV :: Name Type -> UAtom
  deriving (Show, Eq, Ord, Generic, Alpha, Subst BaseTy)

instance Subst UAtom UAtom where
  isvar (UV x) = Just (SubstName (coerce x))
  isvar _ = Nothing

instance Pretty UAtom where
  pretty (UB b) = pretty b
  pretty (UV n) = pretty n

-- | Is this unifiable atomic type a (unification) variable?
uisVar :: UAtom -> Bool
uisVar (UV _) = True
uisVar _ = False

-- | Convert a unifiable atomic type into a regular atomic type.
uatomToAtom :: UAtom -> Atom
uatomToAtom (UB b) = ABase b
uatomToAtom (UV x) = AVar (U x)

-- | Convert a unifiable atomic type to an explicit @Either@ type.
uatomToEither :: UAtom -> Either BaseTy (Name Type)
uatomToEither (UB b) = Left b
uatomToEither (UV v) = Right v

----------------------------------------
-- Type constructors

-- | /Compound types/, such as functions, product types, and sum
--   types, are an application of a /type constructor/ to one or more
--   argument types.
data Con where
  -- | Function type constructor, @T1 -> T2@.
  CArr :: Con
  -- | Product type constructor, @T1 * T2@.
  CProd :: Con
  -- | Sum type constructor, @T1 + T2@.
  CSum :: Con
  -- | Container type (list, bag, or set) constructor.  Note this
  --   looks like it could contain any 'Atom', but it will only ever
  --   contain either a type variable or a 'CtrList', 'CtrBag', or
  --   'CtrSet'.
  --
  --   See also 'CList', 'CBag', and 'CSet'.
  CContainer :: Atom -> Con
  -- | Key value maps, Map k v
  CMap :: Con
  -- | Graph constructor, Graph a
  CGraph :: Con
  -- | The name of a user defined algebraic datatype.
  CUser :: String -> Con
  deriving (Show, Eq, Ord, Generic, Data, Alpha)

instance Pretty Con where
  pretty = \case
    CMap -> text "Map"
    CGraph -> text "Graph"
    CUser s -> text s
    CList -> text "List"
    CBag -> text "Bag"
    CSet -> text "Set"
    CContainer v -> pretty v
    c -> error $ "Impossible: got Con " ++ show c ++ " in pretty @Con"

-- | 'CList' is provided for convenience; it represents a list type
--   constructor (/i.e./ @List a@).
pattern CList :: Con
pattern CList = CContainer (ABase CtrList)

-- | 'CBag' is provided for convenience; it represents a bag type
--   constructor (/i.e./ @Bag a@).
pattern CBag :: Con
pattern CBag = CContainer (ABase CtrBag)

-- | 'CSet' is provided for convenience; it represents a set type
--   constructor (/i.e./ @Set a@).
pattern CSet :: Con
pattern CSet = CContainer (ABase CtrSet)

{-# COMPLETE CArr, CProd, CSum, CList, CBag, CSet, CGraph, CMap, CUser #-}

----------------------------------------
-- Types

-- | The main data type for representing types in the disco language.
--   A type can be either an atomic type, or the application of a type
--   constructor to one or more type arguments.
--
--   @Type@s are broken down into two cases (@TyAtom@ and @TyCon@) for
--   ease of implementation: there are many situations where all atoms
--   can be handled generically in one way and all type constructors
--   can be handled generically in another.  However, using this
--   representation to write down specific types is tedious; for
--   example, to represent the type @N -> a@ one must write something
--   like @TyCon CArr [TyAtom (ABase N), TyAtom (AVar (U a))]@.  For
--   this reason, pattern synonyms such as ':->:', 'TyN', and
--   'TyVar' are provided so that one can use them to construct and
--   pattern-match on types when convenient.  For example, using these
--   synonyms the foregoing example can be written @TyN :->: TyVar a@.
data Type where
  -- | Atomic types (variables and base types), /e.g./ @N@, @Bool@, /etc./
  TyAtom :: Atom -> Type
  -- | Application of a type constructor to type arguments, /e.g./ @N
  --   -> Bool@ is the application of the arrow type constructor to the
  --   arguments @N@ and @Bool@.
  TyCon :: Con -> [Type] -> Type
  deriving (Show, Eq, Ord, Generic, Data, Alpha)

instance Pretty Type where
  pretty (TyAtom a) = pretty a
  pretty (ty1 :->: ty2) =
    withPA tarrPA $
      lt (pretty ty1) <+> text "→" <+> rt (pretty ty2)
  pretty (ty1 :*: ty2) =
    withPA tmulPA $
      lt (pretty ty1) <+> text "×" <+> rt (pretty ty2)
  pretty (ty1 :+: ty2) =
    withPA taddPA $
      lt (pretty ty1) <+> text "+" <+> rt (pretty ty2)
  pretty (TyCon c []) = pretty c
  pretty (TyCon c tys) = do
    ds <- setPA initPA $ punctuate (text ",") (map pretty tys)
    pretty c <> parens (hsep ds)

instance Subst Type Qualifier
instance Subst Type Rational where
  subst _ _ = id
  substs _ = id
  substBvs _ _ = id
instance Subst Type Void where
  subst _ _ = id
  substs _ = id
instance Subst Type Con where
  isCoerceVar (CContainer (AVar (U x))) =
    Just (SubstCoerce x substCtrTy)
   where
    substCtrTy (TyAtom a) = Just (CContainer a)
    substCtrTy _ = Nothing
  isCoerceVar _ = Nothing
instance Subst Type Type where
  isvar (TyAtom (AVar (U x))) = Just (SubstName x)
  isvar _ = Nothing

pattern TyVar :: Name Type -> Type
pattern TyVar v = TyAtom (AVar (U v))

pattern TySkolem :: Name Type -> Type
pattern TySkolem v = TyAtom (AVar (S v))

pattern TyVoid :: Type
pattern TyVoid = TyAtom (ABase Void)

pattern TyUnit :: Type
pattern TyUnit = TyAtom (ABase Unit)

pattern TyBool :: Type
pattern TyBool = TyAtom (ABase B)

pattern TyProp :: Type
pattern TyProp = TyAtom (ABase P)

pattern TyN :: Type
pattern TyN = TyAtom (ABase N)

pattern TyZ :: Type
pattern TyZ = TyAtom (ABase Z)

pattern TyF :: Type
pattern TyF = TyAtom (ABase F)

pattern TyQ :: Type
pattern TyQ = TyAtom (ABase Q)

pattern TyC :: Type
pattern TyC = TyAtom (ABase C)

-- pattern TyFin :: Integer -> Type
-- pattern TyFin n = TyAtom (ABase (Fin n))

infixr 5 :->:

pattern (:->:) :: Type -> Type -> Type
pattern (:->:) ty1 ty2 = TyCon CArr [ty1, ty2]

infixr 7 :*:

pattern (:*:) :: Type -> Type -> Type
pattern (:*:) ty1 ty2 = TyCon CProd [ty1, ty2]

infixr 6 :+:

pattern (:+:) :: Type -> Type -> Type
pattern (:+:) ty1 ty2 = TyCon CSum [ty1, ty2]

pattern TyList :: Type -> Type
pattern TyList elTy = TyCon CList [elTy]

pattern TyBag :: Type -> Type
pattern TyBag elTy = TyCon CBag [elTy]

pattern TySet :: Type -> Type
pattern TySet elTy = TyCon CSet [elTy]

pattern TyContainer :: Atom -> Type -> Type
pattern TyContainer c elTy = TyCon (CContainer c) [elTy]

pattern TyGraph :: Type -> Type
pattern TyGraph elTy = TyCon CGraph [elTy]

pattern TyMap :: Type -> Type -> Type
pattern TyMap tyKey tyValue = TyCon CMap [tyKey, tyValue]

-- | An application of a user-defined type.
pattern TyUser :: String -> [Type] -> Type
pattern TyUser nm args = TyCon (CUser nm) args

pattern TyString :: Type
pattern TyString = TyList TyC

{-# COMPLETE
  TyVar
  , TySkolem
  , TyVoid
  , TyUnit
  , TyBool
  , TyProp
  , TyN
  , TyZ
  , TyF
  , TyQ
  , TyC
  , (:->:)
  , (:*:)
  , (:+:)
  , TyList
  , TyBag
  , TySet
  , TyGraph
  , TyMap
  , TyUser
  #-}

-- | Is this a type variable?
isTyVar :: Type -> Bool
isTyVar (TyAtom (AVar _)) = True
isTyVar _ = False

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s = S.map (substs s)
  substBvs c bs = S.map (substBvs c bs)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s = M.map (substs s)
  substBvs c bs = M.map (substBvs c bs)

-- | The definition of a user-defined type contains:
--
--   * The actual names of the type variable arguments used in the
--     definition (we keep these around only to help with
--     pretty-printing)
--   * A function representing the body of the definition.  It takes a
--     list of type arguments and returns the body of the definition
--     with the type arguments substituted.
--
--   We represent type definitions this way (using a function, as
--   opposed to a chunk of abstract syntax) because it makes some
--   things simpler, and we don't particularly need to do anything
--   more complicated.
data TyDefBody = TyDefBody [String] ([Type] -> Type)

instance Show TyDefBody where
  show _ = "<tydef>"

-- | A 'TyDefCtx' is a mapping from type names to their corresponding
--   definitions.
type TyDefCtx = M.Map String TyDefBody

-- | Pretty-print a type definition.
instance Pretty (String, TyDefBody) where
  pretty (tyName, TyDefBody ps body) =
    "type" <+> (text tyName <> prettyArgs ps) <+> text "=" <+> pretty (body (map (TyVar . string2Name) ps))
   where
    prettyArgs [] = empty
    prettyArgs _ = do
      ds <- punctuate (text ",") (map text ps)
      parens (hsep ds)

---------------------------------
--  Universally quantified types

-- | 'PolyType' represents a polymorphic type of the form @forall a1
--   a2 ... an. ty@ (note, however, that n may be 0, that is, we can
--   have a "trivial" polytype which quantifies zero variables).
newtype PolyType = Forall (Bind [Name Type] Type)
  deriving (Show, Generic, Data, Alpha, Subst Type)

-- | Pretty-print a polytype.  Note that we never explicitly print
--   @forall@; quantification is implicit, as in Haskell.
instance Pretty PolyType where
  pretty (Forall bnd) = lunbind bnd $
    \(_, body) -> pretty body

-- | Convert a monotype into a trivial polytype that does not quantify
--   over any type variables.  If the type can contain free type
--   variables, use 'closeType' instead.
toPolyType :: Type -> PolyType
toPolyType ty = Forall (bind [] ty)

-- | Convert a monotype into a polytype by quantifying over all its
--   free type variables.
closeType :: Type -> PolyType
closeType ty = Forall (bind (nub $ toListOf fv ty) ty)

--------------------------------------------------
-- Counting inhabitants
--------------------------------------------------

-- | Compute the number of inhabitants of a type.  @Nothing@ means the
--   type is countably infinite.
countType :: Type -> Maybe Integer
countType TyVoid = Just 0
countType TyUnit = Just 1
countType TyBool = Just 2
-- countType (TyFin n)     = Just n
countType TyC = Just (17 * 2 ^ (16 :: Integer))
countType (ty1 :+: ty2) = (+) <$> countType ty1 <*> countType ty2
countType (ty1 :*: ty2)
  | isEmptyTy ty1 = Just 0
  | isEmptyTy ty2 = Just 0
  | otherwise = (*) <$> countType ty1 <*> countType ty2
countType (ty1 :->: ty2) =
  case (countType ty1, countType ty2) of
    (Just 0, _) -> Just 1
    (_, Just 0) -> Just 0
    (_, Just 1) -> Just 1
    (c1, c2) -> (^) <$> c2 <*> c1
countType (TyList ty)
  | isEmptyTy ty = Just 1
  | otherwise = Nothing
countType (TyBag ty)
  | isEmptyTy ty = Just 1
  | otherwise = Nothing
countType (TySet ty) = (2 ^) <$> countType ty
-- t = number of elements in vertex type.
-- n = number of vertices in the graph.
-- For each n in [0..t], we can choose which n values to use for the
--   vertices; then for each ordered pair of vertices (u,v)
--   (including the possibility that u = v), we choose whether or
--   not there is a directed edge u -> v.
--
-- https://oeis.org/A135748

countType (TyGraph ty) =
  (\t -> sum $ map (\n -> (t `choose` n) * 2 ^ (n * n)) [0 .. t])
    <$> countType ty
countType (TyMap tyKey tyValue)
  | isEmptyTy tyKey = Just 1 -- If we can't have any keys or values,
  | isEmptyTy tyValue = Just 1 -- only option is empty map
  | otherwise = (\k v -> (v + 1) ^ k) <$> countType tyKey <*> countType tyValue
-- (v+1)^k since for each key, we can choose among v values to associate with it,
-- or we can choose to not have the key in the map.

-- All other types are infinite. (TyN, TyZ, TyQ, TyF)
countType _ = Nothing

--------------------------------------------------
-- Type predicates
--------------------------------------------------

-- | Check whether a type is a numeric type (@N@, @Z@, @F@, @Q@, or @Zn@).
isNumTy :: Type -> Bool
-- isNumTy (TyFin _) = True
isNumTy ty = ty `elem` [TyN, TyZ, TyF, TyQ]

-- | Decide whether a type is empty, /i.e./ uninhabited.
isEmptyTy :: Type -> Bool
isEmptyTy ty
  | Just 0 <- countType ty = True
  | otherwise = False

-- | Decide whether a type is finite.
isFiniteTy :: Type -> Bool
isFiniteTy ty
  | Just _ <- countType ty = True
  | otherwise = False

-- XXX coinductively check whether user-defined types are searchable
--   e.g.  L = Unit + N * L  ought to be searchable.
--   See https://github.com/disco-lang/disco/issues/318.

-- | Decide whether a type is searchable, i.e. effectively enumerable.
isSearchable :: Type -> Bool
isSearchable TyProp = False
isSearchable ty
  | isNumTy ty = True
  | isFiniteTy ty = True
isSearchable (TyList ty) = isSearchable ty
isSearchable (TySet ty) = isSearchable ty
isSearchable (ty1 :+: ty2) = isSearchable ty1 && isSearchable ty2
isSearchable (ty1 :*: ty2) = isSearchable ty1 && isSearchable ty2
isSearchable (ty1 :->: ty2) = isFiniteTy ty1 && isSearchable ty2
isSearchable _ = False

--------------------------------------------------
-- Strictness
--------------------------------------------------

-- | @Strictness@ represents the strictness (either strict or lazy) of
--   a function application or let-expression.
data Strictness = Strict | Lazy
  deriving (Eq, Show, Generic, Alpha)

-- | Numeric types are strict; others are lazy.
strictness :: Type -> Strictness
strictness ty
  | isNumTy ty = Strict
  | otherwise = Lazy

--------------------------------------------------
-- Utilities
--------------------------------------------------

-- | Decompose a nested product @T1 * (T2 * ( ... ))@ into a list of
--   types.
unpair :: Type -> [Type]
unpair (ty1 :*: ty2) = ty1 : unpair ty2
unpair ty = [ty]

-- | Define @S@ as a substitution on types (the most common kind)
--   for convenience.
type S = Substitution Type

-- | Convert a substitution on atoms into a substitution on types.
atomToTypeSubst :: Substitution Atom -> Substitution Type
atomToTypeSubst = fmap TyAtom

-- | Convert a substitution on unifiable atoms into a substitution on
--   types.
uatomToTypeSubst :: Substitution UAtom -> Substitution Type
uatomToTypeSubst = atomToTypeSubst . fmap uatomToAtom

-- | Return a set of all the free container variables in a type.
containerVars :: Type -> Set (Name Type)
containerVars (TyCon (CContainer (AVar (U x))) tys) =
  x `S.insert` foldMap containerVars tys
containerVars (TyCon _ tys) = foldMap containerVars tys
containerVars _ = S.empty

------------------------------------------------------------
-- HasType class
------------------------------------------------------------

-- | A type class for things whose type can be extracted or set.
class HasType t where
  -- | Get the type of a thing.
  getType :: t -> Type

  -- | Set the type of a thing, when that is possible; the default
  --   implementation is for 'setType' to do nothing.
  setType :: Type -> t -> t
  setType _ = id

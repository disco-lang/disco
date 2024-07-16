module Disco.Exhaustiveness.TypeInfo where

import qualified Disco.Types as Ty

{-

Motivation:

Imagine the function

foo : N * N * N -> Unit
foo (x,y,z) -> unit
foo (x,(y,z)) -> unit

So, we must make the decision on how we want to report back
the uncovered patterns
I am choosing to preserve the visual structure of what the user put in,
So I must turn these product types into tuples, in order to get
back out a similar pattern.

I cannot look to Haskell for guidance, as
(Int,Int,Int) /= (Int,(Int,Int)) in Haskell

Also having convient lists of types and constructors
Makes the algorithm easier to implement
-}

data TypeName = TBool | TUnit | TPair | TEither | TInt | TThrool
  deriving (Eq, Ord, Show)

data Type = Type
  { tyIdent :: Ty.Type,
    tyDataCons :: Maybe [DataCon] -- Set to Nothing for opaque types
  }
  deriving (Eq, Ord, Show)

data DataCon = DataCon
  { dcIdent :: Ident,
    dcTypes :: [Type]
  }
  deriving (Eq, Ord, Show)

data Ident where
  KBool :: Bool -> Ident
  KUnit :: Ident
  KDummy :: Ident
  KTuple :: Ident
  KNat :: Integer -> Ident
  KList :: Ident
  KCons :: Ident
  deriving (Eq, Ord, Show)

unit :: DataCon
unit = DataCon {dcIdent = KUnit, dcTypes = []}

bool :: Bool -> DataCon
bool b = DataCon {dcIdent = KBool b, dcTypes = []}

tuple :: [Type] -> DataCon
tuple types = DataCon {dcIdent = KTuple, dcTypes = types}

natural :: Integer -> DataCon
natural n = DataCon {dcIdent = KNat n, dcTypes = []}

-- Don't mix and match types here,
-- we are going on the honor system
list :: [Type] -> DataCon
list types = DataCon {dcIdent = KList, dcTypes = types }

cons tHead tTail = DataCon {dcIdent = KCons, dcTypes = [tHead, tTail]}

extractRelevant :: Ty.Type -> Type
-- extractRelevant Ty.TyVoid = Just []
-- extractRelevant (a Ty.:*: b) = enumProd (enumType a) (enumType b)
-- extractRelevant (a Ty.:+: b) = enumSum (enumType a) (enumType b)
-- extractRelevant (a Ty.:->: b) = enumFunction (enumType a) (enumType b)
-- extractRelevant (Ty.TySet t) = ?
-- extractRelevant (Ty.TyList t) = ?
extractRelevant Ty.TyBool =
  Type
    { tyIdent = Ty.TyBool,
      tyDataCons = Just [bool True, bool False]
    }
extractRelevant Ty.TyUnit =
  Type
    { tyIdent = Ty.TyUnit,
      tyDataCons = Just [unit]
    }
extractRelevant t@Ty.TyN = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyZ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyF = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyQ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyC = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t = Type {tyIdent = t, tyDataCons = Nothing}

-- extractRelevant ty = error $ "Bad type in exhaust" ++ show ty

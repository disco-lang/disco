module Disco.Exhaustiveness.TypeInfo where

import Control.Monad (replicateM)
import Disco.AST.Typed (ATerm)
import Disco.Effects.Fresh (Fresh, fresh)
import qualified Disco.Types as Ty
import Polysemy
import Unbound.Generics.LocallyNameless (Name, s2n)

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

newtype TypedVar = TypedVar (Name ATerm, Type)
  deriving (Show, Ord)

-- For now, equality is always in terms of the name
-- We will see if the causes problems later
instance Eq TypedVar where
  TypedVar (n1, _t1) == TypedVar (n2, _t2) = n1 == n2

getType :: TypedVar -> Type
getType (TypedVar (_, t)) = t

-- data TypeName = TBool | TUnit | TPair | TEither | TInt | TThrool
--   deriving (Eq, Ord, Show)

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
  KUnit :: Ident
  KBool :: Bool -> Ident
  KNat :: Integer -> Ident
  KPair :: Ident
  -- KTuple :: Ident
  KList :: Ident
  KCons :: Ident
  KDummy :: Ident
  deriving (Eq, Ord, Show)

unit :: DataCon
unit = DataCon {dcIdent = KUnit, dcTypes = []}

bool :: Bool -> DataCon
bool b = DataCon {dcIdent = KBool b, dcTypes = []}

natural :: Integer -> DataCon
natural n = DataCon {dcIdent = KNat n, dcTypes = []}

-- Don't mix and match types here,
-- we are going on the honor system
list :: [Type] -> DataCon
list types = DataCon {dcIdent = KList, dcTypes = types}

cons :: Type -> Type -> DataCon
cons tHead tTail = DataCon {dcIdent = KCons, dcTypes = [tHead, tTail]}

pair :: Type -> Type -> DataCon
pair a b = DataCon {dcIdent = KPair, dcTypes = [a, b]}

extractRelevant :: Ty.Type -> Type
-- extractRelevant Ty.TyVoid = Just []
-- extractRelevant t@(a Ty.:*: b) = Type {tyIdent = t, tyDataCons = Just [tuple [natT, natT, natT]]}
extractRelevant t@(a Ty.:*: b) =
  Type
    { tyIdent = t,
      tyDataCons =
        Just
          [ DataCon {dcIdent = KPair, dcTypes = [extractRelevant a, extractRelevant b]}
          ]
    }
-- extractRelevant (a Ty.:+: b) = enumSum (enumType a) (enumType b)
-- extractRelevant (a Ty.:->: b) = enumFunction (enumType a) (enumType b)
-- extractRelevant (Ty.TySet t) = ?
-- extractRelevant (Ty.TyList t) = ?
extractRelevant t@Ty.TyBool =
  Type
    { tyIdent = t,
      tyDataCons = Just [bool True, bool False]
    }
extractRelevant t@Ty.TyUnit =
  Type
    { tyIdent = t,
      tyDataCons = Just [unit]
    }
extractRelevant t@Ty.TyN = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyZ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyF = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyQ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyC = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t = Type {tyIdent = t, tyDataCons = Nothing}

-- extractRelevant ty = error $ "Bad type in exhaust" ++ show ty

-- TODO: should these really just be blank names?
newName :: (Member Fresh r) => Sem r (Name ATerm)
newName = fresh $ s2n ""

newVar :: (Member Fresh r) => Type -> Sem r TypedVar
newVar types = do
  names <- newName
  return $ TypedVar $ (names, types)

newNames :: (Member Fresh r) => Int -> Sem r [Name ATerm]
newNames i = replicateM i newName

newVars :: (Member Fresh r) => [Type] -> Sem r [TypedVar]
newVars types = do
  names <- newNames (length types)
  return $ map TypedVar $ zip names types

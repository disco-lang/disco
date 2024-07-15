module Disco.Exhaustiveness.TypeInfo where

import qualified Disco.Types as Ty

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
  deriving (Eq, Ord, Show)

data TypeName = TBool | TUnit | TPair | TEither | TInt | TThrool
  deriving (Eq, Ord, Show)

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
      tyDataCons =
        Just
          [ DataCon {dcIdent = KBool True, dcTypes = []},
            DataCon {dcIdent = KBool False, dcTypes = []}
          ]
    }
extractRelevant Ty.TyUnit =
  Type
    { tyIdent = Ty.TyUnit,
      tyDataCons =
        Just
          [DataCon {dcIdent = KUnit, dcTypes = []}]
    }
extractRelevant t@Ty.TyN = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyZ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyF = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyQ = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t@Ty.TyC = Type {tyIdent = t, tyDataCons = Nothing}
extractRelevant t = Type {tyIdent = t, tyDataCons = Nothing}
-- extractRelevant ty = error $ "Bad type in exhaust" ++ show ty

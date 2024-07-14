{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T

data TypeConstructor = TBool | TPair | TEither | TInt | TThrool
  deriving (Show, Eq, Ord)

data Type = Type
  { typeCons :: TypeConstructor,
    dataCons :: Maybe [DataConstructor]
  }
  deriving (Eq, Ord)

instance Show Type where
  show Type { typeCons = t } = "Type: " ++ show t

data DataConstructor = DataConstructor
  { dcIdent :: DataConName,
    dcTypes :: [Type]
  }
  deriving (Eq, Ord)

dcName :: DataConstructor -> Text
dcName dc = case dcIdent dc of
  NameText t -> t
  NameInt i -> T.pack $ show i

data DataConName where
  NameText :: Text -> DataConName
  NameInt :: Int -> DataConName
  deriving (Show, Eq, Ord)

instance Show DataConstructor where
  show dc = "(\'" ++ T.unpack (dcName dc) ++ "\' <" ++ (show . length $ dcTypes dc) ++ ">)"

intCon :: Int -> DataConstructor
intCon i = DataConstructor { dcIdent = NameInt i, dcTypes = []}

bool :: Type
bool =
  Type
    { typeCons = TBool,
      dataCons = Just
          [ DataConstructor {dcIdent = NameText "True", dcTypes = []},
            DataConstructor {dcIdent = NameText "False", dcTypes = []}
          ]
    }

throol :: Type
throol =
  Type
    { typeCons = TThrool,
      dataCons = Just
          [ DataConstructor {dcIdent = NameText "Foo", dcTypes = []},
            DataConstructor {dcIdent = NameText "Bar", dcTypes = []},
            DataConstructor {dcIdent = NameText "Baz", dcTypes = []}
          ]
    }

pair :: Type -> Type -> Type
pair a b =
  Type
    { typeCons = TPair,
      dataCons = Just
          [ DataConstructor {dcIdent = NameText ",", dcTypes = [a, b]}
          ]
    }

either :: Type -> Type -> Type
either a b =
  Type
    { typeCons = TEither,
      dataCons = Just
          [ DataConstructor {dcIdent = NameText "Left", dcTypes = [a]},
            DataConstructor {dcIdent = NameText "Right", dcTypes = [b]}
          ]
    }

int :: Type
int =
  Type
    { typeCons = TInt,
      -- int is an opaque type
      dataCons = Nothing
    }

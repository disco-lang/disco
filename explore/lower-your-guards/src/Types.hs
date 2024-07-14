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
  { dcName :: Text,
    dcTypes :: [Type]
  }
  deriving (Eq, Ord)

instance Show DataConstructor where
  show DataConstructor { dcName = n, dcTypes = t } = "(\'" ++ T.unpack n ++ "\' <" ++ (show . length $ t) ++ ">)"

-- intCon i = DataConstcutor { dcName = show i, }


bool :: Type
bool =
  Type
    { typeCons = TBool,
      dataCons = Just
          [ DataConstructor {dcName = "True", dcTypes = []},
            DataConstructor {dcName = "False", dcTypes = []}
          ]
    }

throol :: Type
throol =
  Type
    { typeCons = TThrool,
      dataCons = Just
          [ DataConstructor {dcName = "Foo", dcTypes = []},
            DataConstructor {dcName = "Bar", dcTypes = []},
            DataConstructor {dcName = "Baz", dcTypes = []}
          ]
    }

pair :: Type -> Type -> Type
pair a b =
  Type
    { typeCons = TPair,
      dataCons = Just
          [ DataConstructor {dcName = ",", dcTypes = [a, b]}
          ]
    }

either :: Type -> Type -> Type
either a b =
  Type
    { typeCons = TEither,
      dataCons = Just
          [ DataConstructor {dcName = "Left", dcTypes = [a]},
            DataConstructor {dcName = "Right", dcTypes = [b]}
          ]
    }

int :: Type
int =
  Type
    { typeCons = TInt,
      -- int is an opaque type
      dataCons = Nothing
    }

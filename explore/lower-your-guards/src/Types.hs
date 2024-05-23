{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T

data TypeConstructor = TBool | TPair | TEither | TInt
  deriving (Show, Eq, Ord)

data Type = Type
  { typeCons :: TypeConstructor,
    dataCons :: [DataConstructor]
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



bool :: Type
bool =
  Type
    { typeCons = TBool,
      dataCons =
          [ DataConstructor {dcName = "True", dcTypes = []},
            DataConstructor {dcName = "False", dcTypes = []}
          ]
    }

pair :: Type -> Type -> Type
pair a b =
  Type
    { typeCons = TPair,
      dataCons =
          [ DataConstructor {dcName = ",", dcTypes = [a, b]}
          ]
    }

either :: Type -> Type -> Type
either a b =
  Type
    { typeCons = TEither,
      dataCons =
          [ DataConstructor {dcName = "Left", dcTypes = [a]},
            DataConstructor {dcName = "Right", dcTypes = [b]}
          ]
    }

int :: Type
int =
  Type
    { typeCons = TInt,
      dataCons = []
    }

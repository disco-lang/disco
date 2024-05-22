module Types where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data Type = Type
  { typeName :: Text,
    dataCons :: Either [DataConstructor] ()
    -- ,member :: Text -> Maybe [Type]
  }
  deriving (Show, Eq, Ord)

data DataConstructor = DataConstructor
  { dcName :: Text,
    dcTypes :: [Type]
  }
  deriving (Show, Eq, Ord)

bool :: Type
bool =
  Type
    { typeName = "Bool",
      dataCons =
        Left
          [ DataConstructor {dcName = "True", dcTypes = []},
            DataConstructor {dcName = "False", dcTypes = []}
          ]
    }

pair :: Type -> Type -> Type
pair a b =
  Type
    { typeName = ",",
      dataCons =
        Left
          [ DataConstructor {dcName = ",", dcTypes = [a, b]}
          ]
    }

either :: Type -> Type -> Type
either a b =
  Type
    { typeName = "Either",
      dataCons =
        Left
          [ DataConstructor {dcName = "Left", dcTypes = [a]},
            DataConstructor {dcName = "Right", dcTypes = [b]}
          ]
    }

int :: Type
int =
  Type
    { typeName = "Int",
      dataCons =
        Right ()
          -- ( \case
          --     DataConstructor {dcName = name, dcTypes = types} ->
          --       null types && isJust (readMaybe (T.unpack name) :: Maybe Int)
          -- )
    }

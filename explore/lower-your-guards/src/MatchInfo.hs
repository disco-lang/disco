module MatchInfo (MatchInfo (..), HerebyBe(..), TypedVar) where

import qualified Fresh as F
import qualified Types as Ty

-- type VarMatchInfo = (F.VarID, MatchInfo)
data MatchInfo where
  Match :: Ty.DataConstructor -> [F.VarID] -> MatchInfo
  Not :: Ty.DataConstructor -> MatchInfo
  deriving (Show, Eq, Ord)

-- Hereby be known as
data HerebyBe where
  HerebyBe :: TypedVar -> HerebyBe
  deriving (Show, Eq, Ord)

type TypedVar = (F.VarID, Ty.Type)

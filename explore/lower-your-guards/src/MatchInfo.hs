module MatchInfo (MatchInfo (..), HerebyBe(..), TypedVar, getDataCons) where

import qualified Fresh as F
import qualified Types as Ty

-- type VarMatchInfo = (F.VarID, MatchInfo)
data MatchInfo where
  Match :: Ty.DataConstructor -> [TypedVar] -> MatchInfo
  Not :: Ty.DataConstructor -> MatchInfo
  Be :: TypedVar -> MatchInfo
  deriving (Show, Eq, Ord)

-- Hereby be known as
data HerebyBe where
  HerebyBe :: TypedVar -> HerebyBe
  deriving (Show, Eq, Ord)

type TypedVar = (F.VarID, Ty.Type)

getDataCons :: TypedVar -> Maybe [Ty.DataConstructor]
getDataCons = Ty.dataCons . snd

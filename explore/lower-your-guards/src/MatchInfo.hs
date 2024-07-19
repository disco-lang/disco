module MatchInfo (MatchInfo (..), TypedVar, getDataCons) where

import qualified Fresh as F
import qualified Types as Ty

-- type VarMatchInfo = (F.VarID, MatchInfo)
data MatchInfo where
  Match :: Ty.DataConstructor -> [TypedVar] -> MatchInfo
  Not :: Ty.DataConstructor -> MatchInfo
  WasOriginally :: TypedVar -> MatchInfo
  deriving (Show, Eq, Ord)

type TypedVar = (F.VarID, Ty.Type)

getDataCons :: TypedVar -> Maybe [Ty.DataConstructor]
getDataCons = Ty.dataCons . snd

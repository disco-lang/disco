{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UA (ua, redundantNorm, NAnt) where

import Control.Monad.Trans.Maybe
import qualified Fresh as F
import qualified GuardTree as G
import qualified Inhabitants as I
import MatchInfo
import qualified Possibilities as Poss
import qualified Uncovered as U

data NAnt where
  Grhs :: [I.NormRefType] -> Int -> NAnt
  Branch :: NAnt -> NAnt -> NAnt
  deriving (Show)

ua :: [I.NormRefType] -> G.Gdt -> F.Fresh ([I.NormRefType], NAnt)
ua nrefs gdt = case gdt of
  G.Grhs k -> return ([], Grhs nrefs k)
  G.Branch t1 t2 -> do
    (n1, u1) <- ua nrefs t1
    (n2, u2) <- ua n1 t2
    return (n2, Branch u1 u2)
  G.Guarded (var, g) t -> case g of
    G.GWas old -> do
      n <- addLitMulti nrefs $ varInfo (WasOriginally old)
      ua n t
    G.GMatch k args -> do
      n <- addLitMulti nrefs $ varInfo (Match k args)
      (n', u) <- ua n t
      n'' <- addLitMulti nrefs $ varInfo (Not k)
      return (n'' ++ n', u)
   where
    varInfo = U.Info var

addLitMulti :: [I.NormRefType] -> U.Literal -> F.Fresh [I.NormRefType]
addLitMulti [] _ = return []
addLitMulti (n : ns) lit = do
  r <- runMaybeT $ I.addLiteral n lit
  case r of
    Nothing -> addLitMulti ns lit
    Just (ctx, cfs) -> do
      ns' <- addLitMulti ns lit
      return $ (ctx, cfs) : ns'

redundantNorm :: NAnt -> U.Context -> F.Fresh [Int]
redundantNorm ant args = case ant of
  Grhs ref i -> do
    nothing <- Poss.none <$> I.genInhabNorm ref args
    return ([i | nothing])
  Branch a1 a2 -> mappend <$> redundantNorm a1 args <*> redundantNorm a2 args

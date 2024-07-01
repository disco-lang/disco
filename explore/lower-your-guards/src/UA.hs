module UA (ua, redundantNorm, NAnt) where

import Control.Monad.Trans.Maybe
import qualified Fresh as F
import qualified GuardTree as G
import qualified Inhabitants as I
import qualified Uncovered as U
import Control.Monad.State (get)

data NAnt where
  Grhs :: [I.NormRefType] -> Int -> NAnt
  Branch :: NAnt -> NAnt -> NAnt
  deriving Show

ua :: [I.NormRefType] -> G.Gdt -> F.Fresh ([I.NormRefType], NAnt)
ua nrefs gdt = case gdt of
  G.Grhs k -> return ([], Grhs nrefs k)
  G.Branch t1 t2 -> do
    (n1, u1) <- ua nrefs t1
    (n2, u2) <- ua n1 t2
    return (n2, Branch u1 u2)
  G.Guarded (G.Let x xType y) t -> do
    n <- addLitMulti nrefs (U.Let x xType y)
    ua n t
  G.Guarded (G.Match k args x) t -> do
    n <- addLitMulti nrefs (U.MatchDataCon k args x)
    (n',u) <- ua n t
    n'' <- addLitMulti nrefs (U.NotDataCon k x)
    return (n'' ++ n', u)
  G.Guarded (G.MatchLit i x) t -> do
    n <- addLitMulti nrefs (U.MatchIntLit i x)
    (n',u) <- ua n t
    n'' <- addLitMulti nrefs (U.NotIntLit i x)
    return (n'' ++ n', u)

addLitMulti :: [I.NormRefType] -> U.Literal -> F.Fresh [I.NormRefType]
addLitMulti [] _ = return []
addLitMulti (n:ns) lit = do
    r <- runMaybeT $ I.addLiteral n lit
    case r of
        Nothing -> addLitMulti ns lit
        Just (ctx, cfs) -> do
            ns' <- addLitMulti ns lit
            return $ (ctx, cfs) : ns'

redundantNorm :: NAnt -> U.Context -> F.Fresh [Int]
redundantNorm ant args = case ant of
  Grhs ref i -> do
    s <- get
    return ([i | null $ I.genInhabNorm s ref args])
  Branch a1 a2 -> mappend <$> redundantNorm a1 args <*> redundantNorm a2 args

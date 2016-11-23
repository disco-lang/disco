{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Disco.Desugar where

import           Unbound.LocallyNameless

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.Typecheck
import           Disco.Types

type DSM = LFreshM

runDSM :: DSM a -> a
runDSM = runLFreshM

desugar :: ATerm -> DSM Core
desugar (ATVar _ x)   = return $ CVar (translate x)
desugar ATUnit        = return $ CCons 0 []
desugar (ATBool b)    = return $ CCons (fromEnum b) []
desugar (ATAbs _ lam) =
  lunbind lam $ \(x,t) -> do
  dt <- desugar t
  return $ CAbs (bind (translate x) dt)
desugar (ATApp ty t1 t2) =
  CApp (strictness ty) <$> desugar t1 <*> desugar t2
desugar (ATPair _ t1 t2) =
  CCons 0 <$> mapM desugar [t1,t2]
desugar (ATInj _ s t) =
  CCons (fromEnum s) <$> mapM desugar [t]
desugar (ATNat n) = return $ CNat n
desugar (ATUn _ op t) =
  desugarUOp op <$> desugar t
desugar (ATBin ty op t1 t2) =
  desugarBOp (getType t1) op <$> desugar t1 <*> desugar t2
desugar (ATLet ty t) =
  lunbind t $ \((x, unembed -> t1), t2) -> do
  dt1 <- desugar t1
  dt2 <- desugar t2
  return $ CLet (strictness (getType t1)) (bind (translate x, embed dt1) dt2)
desugar (ATCase _ bs) = CCase <$> mapM desugarBranch bs
desugar (ATAscr t _) = desugar t
desugar (ATSub _ t)  = desugar t

desugarUOp :: UOp -> Core -> Core
desugarUOp Neg c = COp ONeg [c]
desugarUOp Not c = COp ONot [c]

desugarBOp :: Type -> BOp -> Core -> Core -> Core
desugarBOp _  Add     c1 c2 = COp OAdd [c1,c2]
desugarBOp _  Sub     c1 c2 = COp OAdd [c1, COp ONeg [c2]]
desugarBOp _  Mul     c1 c2 = COp OMul [c1, c2]
desugarBOp _  Div     c1 c2 = COp ODiv [c1, c2]
desugarBOp _  Exp     c1 c2 = COp OExp [c1, c2]
desugarBOp ty Eq      c1 c2 = COp (OEq ty) [c1, c2]
desugarBOp ty Neq     c1 c2 = COp ONot [COp (OEq ty) [c1, c2]]
desugarBOp ty Lt      c1 c2 = COp (OLt ty) [c1, c2]
desugarBOp ty Gt      c1 c2 = COp (OLt ty) [c2, c1]
desugarBOp ty Leq     c1 c2 = COp ONot [COp (OLt ty) [c2, c1]]
desugarBOp ty Geq     c1 c2 = COp ONot [COp (OLt ty) [c1, c2]]
desugarBOp _  And     c1 c2 = COp OAnd [c1, c2]
desugarBOp _  Or      c1 c2 = COp OOr  [c1, c2]
desugarBOp _  Mod     c1 c2 = COp OMod [c1, c2]
desugarBOp _  Divides c1 c2 = COp ODivides [c1, c2]
desugarBOp _  RelPm   c1 c2 = COp ORelPm [c1, c2]

desugarBranch :: ABranch -> DSM CBranch
desugarBranch b =
  lunbind b $ \(ags, at) -> do
  cgs <- desugarGuards ags
  c <- desugar at
  return $ bind cgs c

desugarGuards :: AGuards -> DSM CGuards
desugarGuards AGEmpty = return CGEmpty
desugarGuards (AGCons (unrebind -> (ag, ags))) =
  case ag of
    AGIf (unembed -> at) -> do
      c <- desugar at
      cgs <- desugarGuards ags
      return $ CGCons (rebind (embed c, CPCons (fromEnum True) []) cgs)
    AGWhen (unembed -> at) p -> do
      c <- desugar at
      cgs <- desugarGuards ags
      return $ CGCons (rebind (embed c, desugarPattern p) cgs)

desugarPattern :: Pattern -> CPattern
desugarPattern (PVar x)      = CPVar (translate x)
desugarPattern PWild         = CPWild
desugarPattern PUnit         = CPCons 0 []
desugarPattern (PBool b)     = CPCons (fromEnum b) []
desugarPattern (PPair p1 p2) = CPCons 0 [desugarPattern p1, desugarPattern p2]
desugarPattern (PInj s p)    = CPCons (fromEnum s) [desugarPattern p]
desugarPattern (PNat n)      = CPNat n
desugarPattern (PSucc p)     = CPSucc (desugarPattern p)

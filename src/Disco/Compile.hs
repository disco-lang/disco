{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Compile
-- Copyright   :  (c) 2018 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Compiling the typechecked, desugared AST to the untyped core
-- language.
--
-----------------------------------------------------------------------------

module Disco.Compile
       where

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Core
import           Disco.AST.Desugared
import           Disco.AST.Generic
import           Disco.AST.Typed
import           Disco.Desugar
import           Disco.Syntax.Operators
import           Disco.Typecheck
import           Disco.Types

import           Data.Coerce
import           Data.Map                         ((!))
import qualified Data.Map                         as M
import           Data.Ratio

-- For convenience
compileTerm :: ATerm -> Core
compileTerm = runFreshM . compileDTerm . runDSM . desugarTerm

compileDefn :: Defn -> Core
compileDefn = runFreshM . compileDTerm . runDSM . desugarDefn

compileDTerm :: DTerm -> FreshM Core
compileDTerm (DTVar _ x)  = return $ CVar (coerce x)
compileDTerm DTUnit       = return $ CCons 0 []
compileDTerm (DTBool b)   = return $ CCons (fromEnum b) []
compileDTerm (DTNat ty n) = compileNat ty n
compileDTerm (DTRat r)    = return $ CNum Decimal r

compileDTerm (DTLam _ l) = do
  (x,body) <- unbind l
  c <- compileDTerm body
  return $ CAbs (bind (coerce x) c)

compileDTerm (DTApp ty t1 t2)
  = CApp (strictness ty) <$> compileDTerm t1 <*> compileDTerm t2

compileDTerm (DTPair _ t1 t2)
  = CCons 0 <$> mapM compileDTerm [t1,t2]

compileDTerm (DTInj _ s t)
  = CCons (fromEnum s) <$> mapM compileDTerm [t]

compileDTerm (DTCase _ bs)
  = CCase <$> mapM compileBranch bs

compileDTerm (DTUn ty op t)
  = compileUOp ty op <$> compileDTerm t

compileDTerm (DTBin ty op t1 t2)
  = compileBOp (getType t1) (getType t2) ty op <$> compileDTerm t1 <*> compileDTerm t2

compileDTerm (DTTyOp _ op ty) = return $ COp (compileTyOp op) [CType ty]
compileDTerm (DTNil _)        = return $ CCons 0 []

compileDTerm (DTContainer _ ListContainer ds (Just ell))
  = CEllipsis <$> mapM compileDTerm ds <*> traverse compileDTerm ell

compileDTerm (DTContainer (TySet eltTy) SetContainer ds Nothing)
  = CoreSet eltTy <$> mapM compileDTerm ds

-- | Compile a natural number. A separate function is needed in
--   case the number is of a finite type, in which case we must
--   mod it by its type.
compileNat :: Type -> Integer -> FreshM Core
compileNat (TyFin n) x = return $ CNum Fraction ((x `mod` n) % 1)
compileNat _         x = return $ CNum Fraction (x % 1)

-- XXX
compileBranch :: DBranch -> FreshM CBranch
compileBranch b = do
  (gs, d) <- unbind b
  bind <$> traverseTelescope compileGuard gs <*> compileDTerm d

-- XXX
compileGuard :: DGuard -> FreshM (Embed Core, CPattern)
compileGuard (DGPat (unembed -> d) dpat) =
  (,)
    <$> (embed <$> compileDTerm d)
    <*> compilePattern dpat

-- XXX
compilePattern :: DPattern -> FreshM CPattern
compilePattern (DPVar _ x)      = return $ CPVar (coerce x)
compilePattern (DPWild _)       = return CPWild
compilePattern DPUnit           = return $ CPCons 0 []
compilePattern (DPBool b)       = return $ CPCons (fromEnum b) []
compilePattern (DPPair _ p1 p2) = CPCons 0 <$> mapM compilePattern [p1,p2]
compilePattern (DPInj _ s p)    = CPCons (fromEnum s) <$> mapM compilePattern [p]
compilePattern (DPNat _ n)      = return $ CPNat n
compilePattern (DPSucc p)       = CPSucc <$> compilePattern p
compilePattern (DPNil _)        = return $ CPCons 0 []
compilePattern (DPCons _ p1 p2) = CPCons 1 <$> mapM compilePattern [p1,p2]

-- | Compile a unary operator application.
compileUOp :: Type -> UOp -> Core -> Core

-- XXX
-- Special ops for modular arithmetic in finite types
compileUOp _ op c = COp (coreUOps ! op) [c]
  where
    coreUOps = M.fromList $
      [ Neg   ==> ONeg
      , Fact  ==> OFact
      , Sqrt  ==> OSqrt
      , Lg    ==> OLg
      , Floor ==> OFloor
      , Ceil  ==> OCeil
      , Abs   ==> OAbs
      ]

---   @arg1 ty -> arg2 ty -> result ty -> op -> desugared arg1 -> desugared arg2 -> result@
compileBOp :: Type -> Type -> Type -> BOp -> Core -> Core -> Core
-- Special ops for modular arithmetic in finite types
compileBOp _ _ (TyFin n) op     c1 c2
  | op `elem` [Div, Exp]
  = COp (omOp op n) [c1, c2]
  where
    omOp Div = OMDiv
    omOp Exp = OMExp
compileBOp (TyFin n) _ _ Divides c1 c2 = COp (OMDivides n) [c1, c2]

compileBOp _  _ _ Add     c1 c2 = COp OAdd [c1,c2]
compileBOp _  _ _ Mul     c1 c2 = COp OMul [c1, c2]
compileBOp _  _ _ Div     c1 c2 = COp ODiv [c1, c2]
compileBOp _  _ _ Exp     c1 c2 = COp OExp [c1, c2]
compileBOp ty _ _ Eq      c1 c2 = COp (OEq ty) [c1, c2]
compileBOp ty _ _ Lt      c1 c2 = COp (OLt ty) [c1, c2]
compileBOp _  _ _ Mod     c1 c2 = COp OMod [c1, c2]
compileBOp _  _ _ Divides c1 c2 = COp ODivides [c1, c2]
compileBOp _  _ _ Cons    c1 c2 = CCons 1 [c1, c2]

compileBOp _ TyN _ Choose c1 c2 = COp OBinom [c1, c2]
compileBOp _ _   _ Choose c1 c2 = COp OMultinom [c1, c2]
compileBOp (TySet ty) _ _ Union c1 c2        = COp (OUnion ty) [c1, c2]
compileBOp (TySet ty) _ _ Intersection c1 c2 = COp (OIntersection ty) [c1, c2]
compileBOp (TySet ty) _ _ Difference c1 c2   = COp (ODifference ty) [c1, c2]
compileBOp (TySet ty) _ _ Subset c1 c2           = COp (OSubset ty) [c1, c2]

compileBOp _  _ _ op _ _ = error $ "Impossible! " -- XXX

compileTyOp Enumerate = OEnum
compileTyOp Count     = OCount

-- XXX
(==>) = (,)

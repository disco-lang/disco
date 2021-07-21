-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Erase
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Typecheck the Disco surface language and transform it into a
-- type-annotated AST.
--
-----------------------------------------------------------------------------

module Disco.Typecheck.Erase where

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe

import           Control.Arrow                           ((***))
import           Data.Coerce

import           Disco.AST.Desugared
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Module                            (Clause)

-- | Erase all the type annotations from a term.
erase :: ATerm -> Term
erase (ATVar _ x)           = TVar (coerce x)
erase (ATPrim _ x)          = TPrim x
erase (ATLet _ bs)          = TLet $ bind (mapTelescope eraseBinding tel) (erase at)
  where (tel,at) = unsafeUnbind bs
erase ATUnit                = TUnit
erase (ATBool _ b)          = TBool b
erase (ATChar c)            = TChar c
erase (ATString s)          = TString s
erase (ATNat _ i)           = TNat i
erase (ATRat r)             = TRat r
erase (ATAbs q _ b)         = TAbs q $ bind (map erasePattern x) (erase at)
  where (x,at) = unsafeUnbind b
erase (ATApp _ t1 t2)       = TApp (erase t1) (erase t2)
erase (ATTup _ ats)         = TTup (map erase ats)
erase (ATCase _ brs)        = TCase (map eraseBranch brs)
erase (ATChain _ at lnks)   = TChain (erase at) (map eraseLink lnks)
erase (ATTyOp _ op ty)      = TTyOp op ty
erase (ATContainer _ c ats aell)   = TContainer c (map (erase *** fmap erase) ats) ((fmap . fmap) erase aell)
erase (ATContainerComp _ c b)      = TContainerComp c $ bind (mapTelescope eraseQual tel) (erase at)
  where (tel,at) = unsafeUnbind b
erase (ATTest _ x)          = erase x

eraseBinding :: ABinding -> Binding
eraseBinding (ABinding mty x (unembed -> at)) = Binding mty (coerce x) (embed (erase at))

erasePattern :: APattern -> Pattern
erasePattern (APVar _ n)        = PVar (coerce n)
erasePattern (APWild _)         = PWild
erasePattern APUnit             = PUnit
erasePattern (APBool b)         = PBool b
erasePattern (APChar c)         = PChar c
erasePattern (APString s)       = PString s
erasePattern (APTup _ alp)      = PTup $ map erasePattern alp
erasePattern (APInj _ s apt)    = PInj s (erasePattern apt)
erasePattern (APNat _ n)        = PNat n
erasePattern (APCons _ ap1 ap2) = PCons (erasePattern ap1) (erasePattern ap2)
erasePattern (APList _ alp)     = PList $ map erasePattern alp
erasePattern (APAdd _ s p t)    = PAdd s (erasePattern p) (erase t)
erasePattern (APMul _ s p t)    = PMul s (erasePattern p) (erase t)
erasePattern (APSub _ p t)      = PSub (erasePattern p) (erase t)
erasePattern (APNeg _ p)        = PNeg (erasePattern p)
erasePattern (APFrac _ p1 p2)   = PFrac (erasePattern p1) (erasePattern p2)

eraseBranch :: ABranch -> Branch
eraseBranch b = bind (mapTelescope eraseGuard tel) (erase at)
  where (tel,at) = unsafeUnbind b

eraseGuard :: AGuard -> Guard
eraseGuard (AGBool (unembed -> at))  = GBool (embed (erase at))
eraseGuard (AGPat (unembed -> at) p) = GPat (embed (erase at)) (erasePattern p)
eraseGuard (AGLet b)                 = GLet (eraseBinding b)

eraseLink :: ALink -> Link
eraseLink (ATLink bop at) = TLink bop (erase at)

eraseQual :: AQual -> Qual
eraseQual (AQBind x (unembed -> at)) = QBind (coerce x) (embed (erase at))
eraseQual (AQGuard (unembed -> at))  = QGuard (embed (erase at))

eraseProperty :: AProperty -> Property
eraseProperty = erase

eraseClause :: Clause -> Bind [Pattern] Term
eraseClause b = bind (map erasePattern ps) (erase t)
  where (ps, t) = unsafeUnbind b

------------------------------------------------------------
-- DTerm erasure

eraseDTerm :: DTerm -> Term
eraseDTerm (DTVar _ x)      = TVar (coerce x)
eraseDTerm (DTPrim _ x)     = TPrim x
eraseDTerm DTUnit           = TUnit
eraseDTerm (DTBool _ b)     = TBool b
eraseDTerm (DTChar c)       = TChar c
eraseDTerm (DTNat _ n)      = TNat n
eraseDTerm (DTRat r)        = TRat r
eraseDTerm (DTAbs q _ b)    = TAbs q $ bind [PVar . coerce $ x] (eraseDTerm dt)
  where (x, dt) = unsafeUnbind b
eraseDTerm (DTApp _ d1 d2)  = TApp (eraseDTerm d1) (eraseDTerm d2)
eraseDTerm (DTPair _ d1 d2) = TTup [eraseDTerm d1, eraseDTerm d2]
eraseDTerm (DTCase _ bs)    = TCase (map eraseDBranch bs)
eraseDTerm (DTTyOp _ op ty) = TTyOp op ty
eraseDTerm (DTNil _)        = TList [] Nothing
eraseDTerm (DTTest _ x)     = eraseDTerm x

eraseDBranch :: DBranch -> Branch
eraseDBranch b = bind (mapTelescope eraseDGuard tel) (eraseDTerm d)
  where
    (tel, d) = unsafeUnbind b

eraseDGuard :: DGuard -> Guard
eraseDGuard (DGPat (unembed -> d) p) = GPat (embed (eraseDTerm d)) (eraseDPattern p)

eraseDPattern :: DPattern -> Pattern
eraseDPattern (DPVar _ x)      = PVar (coerce x)
eraseDPattern (DPWild _)       = PWild
eraseDPattern DPUnit           = PUnit
eraseDPattern (DPBool b)       = PBool b
eraseDPattern (DPChar c)       = PChar c
eraseDPattern (DPPair _ x1 x2) = PTup (map (PVar . coerce) [x1,x2])
eraseDPattern (DPInj _ s x)    = PInj s (PVar (coerce x))
eraseDPattern (DPNat _ n)      = PNat n
eraseDPattern (DPFrac _ x1 x2) = PFrac (PVar (coerce x1)) (PVar (coerce x2))
eraseDPattern (DPNil _)        = PList []

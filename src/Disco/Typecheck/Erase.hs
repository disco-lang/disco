{-# LANGUAGE ViewPatterns #-}
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

import           Disco.AST.Surface
import           Disco.AST.Typed

-- | Erase all the type annotations from a term.
erase :: ATerm -> Term
erase (ATVar _ x)           = TVar (coerce x)
erase (ATLet _ bs)          = TLet $ bind (mapTelescope eraseBinding tel) (erase at)
  where (tel,at) = unsafeUnbind bs
erase ATUnit                = TUnit
erase (ATBool b)            = TBool b
erase (ATChar c)            = TChar c
erase (ATString s)          = TString s
erase (ATNat _ i)           = TNat i
erase (ATRat r)             = TRat r
erase (ATAbs _ b)           = TAbs $ bind (map (coerce *** (embed . Just . unembed)) x) (erase at)
  where (x,at) = unsafeUnbind b
erase (ATApp _ t1 t2)       = TApp (erase t1) (erase t2)
erase (ATTup _ ats)         = TTup (map erase ats)
erase (ATInj _ s at)        = TInj s (erase at)
erase (ATCase _ brs)        = TCase (map eraseBranch brs)
erase (ATUn _ uop at)       = TUn uop (erase at)
erase (ATBin _ bop at1 at2) = TBin bop (erase at1) (erase at2)
erase (ATChain _ at lnks)   = TChain (erase at) (map eraseLink lnks)
erase (ATTyOp _ op ty)      = TTyOp op ty
erase (ATContainer _ c ats aell)   = TContainer c (map erase ats) ((fmap . fmap) erase aell)
erase (ATContainerComp _ c b)      = TContainerComp c $ bind (mapTelescope eraseQual tel) (erase at)
  where (tel,at) = unsafeUnbind b

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
erasePattern (APSucc apt)       = PSucc $ erasePattern apt
erasePattern (APCons _ ap1 ap2) = PCons (erasePattern ap1) (erasePattern ap2)
erasePattern (APList _ alp)     = PList $ map erasePattern alp
erasePattern (APPlus _ s p t)   = PPlus s (erasePattern p) (erase t)

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
eraseProperty b = bind (coerce xs) (erase at)
  where (xs, at) = unsafeUnbind b

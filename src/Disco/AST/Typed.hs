{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Typed
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Typed abstract syntax trees representing the typechecked surface
-- syntax of the Disco language.  Each tree node is annotated with the
-- type of its subtree.
--
-----------------------------------------------------------------------------

module Disco.AST.Typed
       ( -- * Type-annotated terms
       ATerm
       , pattern ATVar
       , pattern ATUn
       , pattern ATLet
       , pattern ATUnit
       , pattern ATBool
       , pattern ATNat
       , pattern ATRat
       , pattern ATAbs
       , pattern ATApp
       , pattern ATTup
       , pattern ATInj
       , pattern ATCase
       , pattern ATBin
       , pattern ATChain
       , pattern ATTyOp
       , pattern ATContainer
       , pattern ATContainerComp
       , pattern ATList
       , pattern ATListComp

       , ALink
       , pattern ATLink

       , Container(..)
       , ABinding
         -- * Branches and guards
       , ABranch

       , AGuard
       , pattern AGBool
       , pattern AGPat

       , AQual
       , pattern AQBind
       , pattern AQGuard

       , APattern
       , pattern APVar
       , pattern APWild
       , pattern APUnit
       , pattern APBool
       , pattern APTup
       , pattern APInj
       , pattern APNat
       , pattern APSucc
       , pattern APCons
       , pattern APList
       , pattern ABinding
         -- * Utilities
       , getType
       , setType

       , AProperty
       )
       where

import           Unbound.Generics.LocallyNameless

import           Data.Void

import           Disco.AST.Generic
import           Disco.Syntax.Operators
import           Disco.Types

-- | The extension descriptor for Typed specific AST types.

data TY

type AProperty = Property_ TY

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.

type ATerm = Term_ TY

type instance X_TVar            TY = Type
type instance X_TLet            TY = Type
type instance X_TUnit           TY = ()
type instance X_TBool           TY = ()
type instance X_TNat            TY = Type
type instance X_TRat            TY = ()
type instance X_TAbs            TY = Void -- Replace TAbs with a version that
                                          -- definitely has all type annotations
type instance X_TApp            TY = Type
type instance X_TInj            TY = Type
type instance X_TCase           TY = Type
type instance X_TUn             TY = Type
type instance X_TBin            TY = Type
type instance X_TChain          TY = Type
type instance X_TTyOp           TY = Type
type instance X_TContainer      TY = Type
type instance X_TContainerComp  TY = Type
type instance X_TAscr           TY = Void -- No more type ascriptions in typechecked terms
type instance X_TTup            TY = Type
type instance X_TParens         TY = Void -- No more explicit parens

type instance X_Term TY
  = (Type, Bind [(Name ATerm, Embed Type)] ATerm)   -- ATAbs

pattern ATVar :: Type -> Name ATerm -> ATerm
pattern ATVar ty name = TVar_ ty name

pattern ATUn :: Type -> UOp -> ATerm -> ATerm
pattern ATUn ty uop term = TUn_ ty uop term

pattern ATLet :: Type -> Bind (Telescope ABinding) ATerm -> ATerm
pattern ATLet ty bind = TLet_ ty bind

pattern ATUnit :: ATerm
pattern ATUnit = TUnit_ ()

pattern ATBool :: Bool -> ATerm
pattern ATBool bool = TBool_ () bool

pattern ATNat  :: Type -> Integer -> ATerm
pattern ATNat ty int = TNat_ ty int

pattern ATRat :: Rational -> ATerm
pattern ATRat rat = TRat_ () rat

pattern ATAbs :: Type -> Bind [(Name ATerm, Embed Type)] ATerm -> ATerm
pattern ATAbs ty bind = XTerm_ (ty, bind)

pattern ATApp  :: Type -> ATerm -> ATerm -> ATerm
pattern ATApp ty term1 term2 = TApp_ ty term1 term2

pattern ATTup :: Type -> [ATerm] -> ATerm
pattern ATTup ty termlist = TTup_ ty termlist

pattern ATInj :: Type -> Side -> ATerm -> ATerm
pattern ATInj ty side term = TInj_ ty side term

pattern ATCase :: Type -> [ABranch] -> ATerm
pattern ATCase ty branch = TCase_ ty branch

pattern ATBin :: Type -> BOp -> ATerm -> ATerm -> ATerm
pattern ATBin ty bop term1 term2 = TBin_ ty bop term1 term2

pattern ATChain :: Type -> ATerm -> [ALink] -> ATerm
pattern ATChain ty term linklist = TChain_ ty term linklist

pattern ATTyOp :: Type -> TyOp -> Type -> ATerm
pattern ATTyOp ty1 tyop ty2 = TTyOp_ ty1 tyop ty2

pattern ATContainer :: Type -> Container -> [ATerm] -> Maybe (Ellipsis ATerm) -> ATerm
pattern ATContainer ty c tl mets = TContainer_ ty c tl mets

pattern ATContainerComp :: Type -> Container -> Bind (Telescope AQual) ATerm -> ATerm
pattern ATContainerComp ty c b = TContainerComp_ ty c b

{-# COMPLETE ATVar, ATUn, ATLet, ATUnit, ATBool, ATNat, ATRat,
             ATAbs, ATApp, ATTup, ATInj, ATCase, ATBin, ATChain, ATTyOp,
             ATContainer, ATContainerComp #-}

pattern ATList :: Type -> [ATerm] -> Maybe (Ellipsis ATerm) -> ATerm
pattern ATList t xs e = ATContainer t ListContainer xs e

pattern ATListComp :: Type -> Bind (Telescope AQual) ATerm -> ATerm
pattern ATListComp t b = ATContainerComp t ListContainer b


type ALink = Link_ TY

type instance X_TLink TY = ()

pattern ATLink :: BOp -> ATerm -> ALink
pattern ATLink bop term = TLink_ () bop term

{-# COMPLETE ATLink #-}


type AQual = Qual_ TY

type instance X_QBind TY = ()
type instance X_QGuard TY = ()


pattern AQBind :: Name ATerm -> Embed ATerm -> AQual
pattern AQBind namet embedt = QBind_ () namet embedt

pattern AQGuard :: Embed ATerm -> AQual
pattern AQGuard embedt = QGuard_ () embedt

{-# COMPLETE AQBind, AQGuard #-}

type ABinding = Binding_ TY

pattern ABinding :: Maybe (Embed Sigma) -> Name ATerm -> Embed ATerm -> ABinding
pattern ABinding m b n = Binding_ m b n

{-# COMPLETE ABinding #-}

type ABranch = Bind (Telescope AGuard) ATerm

type AGuard = Guard_ TY

type instance X_GBool TY = ()
type instance X_GPat TY = ()

pattern AGBool :: Embed ATerm -> AGuard
pattern AGBool embedt = GBool_ () embedt

pattern AGPat :: Embed ATerm -> APattern -> AGuard
pattern AGPat embedt pat = GPat_ () embedt pat

{-# COMPLETE AGBool, AGPat #-}

type APattern = Pattern_ TY

-- We have to use Embed Type because we don't want any type variables
-- inside the types being treated as binders!

type instance X_PVar     TY = Embed Type
type instance X_PWild    TY = Embed Type
type instance X_PUnit    TY = ()
type instance X_PBool    TY = ()
type instance X_PTup     TY = Embed Type
type instance X_PInj     TY = Embed Type
type instance X_PNat     TY = Embed Type
type instance X_PSucc    TY = ()
type instance X_PCons    TY = Embed Type
type instance X_PList    TY = Embed Type

type instance X_Pattern  TY = ()

pattern APVar :: Type -> Name ATerm -> APattern
pattern APVar ty name <- PVar_ (unembed -> ty) name
  where
    APVar ty name = PVar_ (embed ty) name

pattern APWild :: Type -> APattern
pattern APWild ty <- PWild_ (unembed -> ty)
  where
    APWild ty = PWild_ (embed ty)

pattern APUnit :: APattern
pattern APUnit = PUnit_ ()

pattern APBool :: Bool -> APattern
pattern APBool  b = PBool_ () b

pattern APTup  :: Type -> [APattern] -> APattern
pattern APTup ty lp <- PTup_ (unembed -> ty) lp
  where
    APTup ty lp = PTup_ (embed ty) lp

pattern APInj  :: Type -> Side -> APattern -> APattern
pattern APInj ty s p <- PInj_ (unembed -> ty) s p
  where
    APInj ty s p = PInj_ (embed ty) s p

pattern APNat  :: Type -> Integer -> APattern
pattern APNat ty n <- PNat_ (unembed -> ty) n
  where
    APNat ty n = PNat_ (embed ty) n

pattern APSucc :: APattern -> APattern
pattern APSucc p = PSucc_ () p

pattern APCons :: Type -> APattern -> APattern -> APattern
pattern APCons ty p1 p2 <- PCons_ (unembed -> ty) p1 p2
  where
    APCons ty p1 p2 = PCons_ (embed ty) p1 p2

pattern APList :: Type -> [APattern] -> APattern
pattern APList ty lp <- PList_ (unembed -> ty) lp
  where
    APList ty lp = PList_ (embed ty) lp

{-# COMPLETE APVar, APWild, APUnit, APBool, APTup, APInj, APNat,
    APSucc, APCons, APList #-}

------------------------------------------------------------
-- getType
------------------------------------------------------------

instance HasType ATerm where
  getType (ATVar ty _)             = ty
  getType ATUnit                   = TyUnit
  getType (ATBool _)               = TyBool
  getType (ATNat ty _)             = ty
  getType (ATRat _)                = TyF
  getType (ATAbs ty _)             = ty
  getType (ATApp ty _ _)           = ty
  getType (ATTup ty _)             = ty
  getType (ATInj ty _ _)           = ty
  getType (ATUn ty _ _)            = ty
  getType (ATBin ty _ _ _)         = ty
  getType (ATTyOp ty _ _)          = ty
  getType (ATChain ty _ _)         = ty
  getType (ATContainer ty _ _ _)   = ty
  getType (ATContainerComp ty _ _) = ty
  getType (ATLet ty _)             = ty
  getType (ATCase ty _)            = ty

  setType ty (ATVar _ x      )       = ATVar ty x
  setType _  ATUnit                  = ATUnit
  setType _  (ATBool b)              = ATBool b
  setType ty (ATNat _ x      )       = ATNat ty x
  setType _  (ATRat r)               = ATRat r
  setType ty (ATAbs _ x      )       = ATAbs ty x
  setType ty (ATApp _ x y    )       = ATApp ty x y
  setType ty (ATTup _ x      )       = ATTup ty x
  setType ty (ATInj _ x y    )       = ATInj ty x y
  setType ty (ATUn _ x y     )       = ATUn ty x y
  setType ty (ATBin _ x y z  )       = ATBin ty x y z
  setType ty (ATTyOp _ x y   )       = ATTyOp ty x y
  setType ty (ATChain _ x y  )       = ATChain ty x y
  setType ty (ATContainer _ x y z)   = ATContainer ty x y z
  setType ty (ATContainerComp _ x y) = ATContainerComp ty x y
  setType ty (ATLet _ x      )       = ATLet ty x
  setType ty (ATCase _ x     )       = ATCase ty x

instance HasType APattern where
  getType (APVar ty _)    = ty
  getType (APWild ty)     = ty
  getType APUnit          = TyUnit
  getType (APBool _)      = TyBool
  getType (APTup ty _)    = ty
  getType (APInj ty _ _)  = ty
  getType (APNat ty _)    = ty
  getType (APSucc _)      = TyN
  getType (APCons ty _ _) = ty
  getType (APList ty _)   = ty

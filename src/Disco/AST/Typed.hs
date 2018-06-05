{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Typed
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
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
       , pattern ATList
       , pattern ATListComp
       , pattern ATAscr

       , ALink
       , pattern ATLink

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
       , pattern ATList 
       , pattern ATListComp
       , pattern ATAscr

       , pattern ATLink 

       , pattern ABinding

       , pattern AGBool
       , pattern AGPat

       , pattern AQBind
       , pattern AQGuard
       )
       where

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.Syntax.Operators
import           Disco.Types
import           Disco.AST.Generic

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.

data TY

type AProperty = Property_ TY
pattern AProperty b = Property_ b

type ATerm = Term_ TY

type instance X_TVar TY = Type
type instance X_TLet TY = Type
type instance X_TUnit TY = ()
type instance X_TBool TY = () 
type instance X_TNat TY = Type
type instance X_TRat TY = ()
type instance X_TAbs TY = Type 
type instance X_TApp TY = Type
type instance X_TInj TY = Type
type instance X_TCase TY = Type
type instance X_TUn TY = Type
type instance X_TBin TY = Type 
type instance X_TChain TY = Type 
type instance X_TTyop TY = Type
type instance X_TList TY = Type 
type instance X_TListComp TY = Type 
type instance X_TAscr TY = ()
type instance X_Term TY = () 
type instance X_TTup TY = Type
type instance X_TParens TY = ()

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

pattern ATAbs :: Type -> Bind [(Name ATerm, Embed (Maybe Type))] ATerm -> ATerm
pattern ATAbs ty bind = TAbs_ ty bind

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

pattern ATList :: Type -> [ATerm] -> Maybe (Ellipsis ATerm) -> ATerm
pattern ATList ty termlist mellipses = TList_ ty termlist mellipses

pattern ATListComp :: Type -> Bind (Telescope AQual) ATerm -> ATerm
pattern ATListComp ty bind = TListComp_ ty bind

pattern ATAscr :: ATerm -> Type -> ATerm
pattern ATAscr term ty = TAscr_ () term ty

-- data ATerm where

--   -- | A variable together with its type.
--   ATVar   :: Type -> Name ATerm -> ATerm

--   -- | A (non-recursive) let expression.
--   ATLet   :: Type -> Bind (Telescope ABinding) ATerm -> ATerm

--   -- | The unit value.  We don't bother explicitly storing @TyUnit@
--   --   here.
--   ATUnit  :: ATerm

--   -- | A boolean value. Again, we don't bother explicitly storing
--   --   the type.
--   ATBool  :: Bool -> ATerm

--   -- | A natural number.
--   ATNat   :: Type -> Integer -> ATerm

--   -- | A nonnegative rational number (parsed and displayed as a
--   --   decimal).
--   ATRat   :: Rational -> ATerm

--   -- | Anonymous function, with its type.
--   ATAbs   :: Type -> Bind [(Name ATerm, Embed (Maybe Type))] ATerm -> ATerm

--   -- | A function application, with its type.
--   ATApp   :: Type -> ATerm -> ATerm -> ATerm

--   -- | An n-tuple.
--   ATTup   :: Type -> [ATerm] -> ATerm

--   -- | A sum type injection.
--   ATInj   :: Type -> Side -> ATerm -> ATerm

--   -- | A case expression.
--   ATCase  :: Type -> [ABranch] -> ATerm

--   -- | A unary operator application.
--   ATUn    :: Type -> UOp -> ATerm -> ATerm

--   -- | A binary operator application.
--   ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm

--   -- | A chained comparison.
--   ATChain :: Type -> ATerm -> [ALink] -> ATerm

--   -- | A type operator application.
--   ATTyOp  :: Type -> TyOp -> Type -> ATerm

--   -- | A literal list.  The type would be ambiguous if the list was
--   --   empty.
--   ATList :: Type -> [ATerm] -> Maybe (Ellipsis ATerm) -> ATerm

--   -- | A list comprehension.
--   ATListComp :: Type -> Bind (Telescope AQual) ATerm -> ATerm

--   -- | Type ascription.
--   ATAscr  :: ATerm -> Type -> ATerm

--   deriving (Show, Generic)

type ALink = Link_ TY

type instance X_TLink TY = ()

pattern ATLink :: BOp -> ATerm -> ALink
pattern ATLink bop term = TLink_ () bop term

-- data ALink where
--   ATLink :: BOp -> ATerm -> ALink
--   deriving (Show, Generic)

type ABinding = Binding_ TY

pattern ABinding :: (Maybe Type) -> Name ATerm -> Embed ATerm -> ABinding 
pattern ABinding m b n = Binding_ m b n 

-- data ABinding = ABinding (Maybe Type) (Name ATerm) (Embed ATerm)
--   deriving (Show, Generic)

-- | A branch of a case, consisting of a list of guards and a type-annotated term.
type ABranch = Bind (Telescope AGuard) ATerm


type AGuard = Guard_ TY

type instance X_GBool TY = ()
type instance X_GPat TY = () 

pattern AGBool :: Embed ATerm -> AGuard
pattern AGBool embedt = GBool_ () embedt

pattern AGPat :: Embed ATerm -> APattern -> AGuard
pattern AGPat embedt pat = GPat_ () embedt pat


-- -- | A single guard (@if@ or @when@) containing a type-annotated term.
-- data AGuard where

--   -- | Boolean guard (@when <test>@)
--   AGBool :: Embed ATerm -> AGuard

--   -- | Pattern guard (@when term = pat@)
--   AGPat  :: Embed ATerm -> Pattern -> AGuard

--   deriving (Show, Generic)

-- -- Note: very similar to guards
-- --  maybe some generalization in the future?

type AQual = Qual_ TY

type instance X_QBind TY = ()
type instance X_QGuard TY = ()

pattern AQBind :: Name ATerm -> Embed ATerm -> AQual
pattern AQBind namet embedt = QBind_ () namet embedt

pattern AQGuard :: Embed ATerm -> AQual
pattern AQGuard embedt = QGuard_ () embedt

-- -- | A single qualifier in a list comprehension.
-- data AQual where

--   -- | A binding qualifier (i.e. @x <- t@)
--   AQBind   :: Name ATerm -> Embed ATerm -> AQual

--   -- | A boolean guard qualfier (i.e. @x + y > 4@)
--   AQGuard  :: Embed ATerm -> AQual

--   deriving (Show, Generic)

type AProperty = Bind [(Name ATerm, Type)] ATerm

type APattern = Pattern_ TY

<<<<<<< HEAD
type APattern = Pattern_ TY

type instance X_PVar TY = ()
type instance X_PWild TY = ()
type instance X_PUnit TY = ()
type instance X_PBool TY = ()
type instance X_PTup TY = ()
type instance X_PInj TY = ()
type instance X_PNat TY = ()
type instance X_PSucc TY = ()
type instance X_PCons TY = ()
type instance X_PList TY = ()

pattern APVar :: Name ATerm -> APattern
pattern APVar name = PVar_ () name 

pattern APWild :: APattern
pattern APWild = PWild_ () 

pattern APUnit :: APattern
pattern APUnit = PUnit_ ()

pattern APBool :: Bool -> APattern
pattern APBool  b = PBool_ () b 

pattern APTup  :: [APattern] -> APattern
pattern APTup lp = PTup_ () lp 

-- | Injection pattern (@inl pat@ or @inr pat@).
pattern APInj  :: Side -> APattern -> APattern
pattern APInj s p = PInj_ () s p 

-- | Literal natural number pattern.
pattern APNat  :: Integer -> APattern
pattern APNat n = PNat_ () n 

-- | Successor pattern, @S p@.
pattern APSucc :: APattern -> APattern
pattern APSucc p = PSucc_ () p 

-- | Cons pattern @p1 :: p2@.
pattern APCons :: APattern -> APattern -> APattern
pattern APCons  p1 p2 = PCons_ () p1 p2 

-- | List pattern @[p1, .., pn]@.
pattern APList :: [APattern] -> APattern
pattern APList lp = PList_ () lp 
=======
type instance X_PVar TY = () 
type instance X_PWild TY = ()
type instance X_PUnit TY = ()
type instance X_PBool TY = ()
type instance X_PTup TY = ()   -- Type?
type instance X_PInj TY = ()
type instance X_PNat TY = ()
type instance X_PSucc TY = ()  -- Type?
type instance X_PCons TY = ()  -- Type?
type instance X_PList TY = ()  -- Type?

pattern AVar :: Name ATerm -> APattern
pattern AVar name = PVar_ name 

pattern PWild :: APattern
pattern PWild = PWild_ 

pattern PUnit :: APattern
pattern PUnit = PUnit_

pattern PBool :: Bool -> APattern
pattern PBool  b = PBool_ b 

pattern PTup  :: [APattern] -> APattern
pattern PTup lp = PTup_ lp 

-- | Injection pattern (@inl pat@ or @inr pat@).
pattern PInj  :: Side -> APattern -> APattern
pattern PInj s p = PInj_ s p 

-- | Literal natural number pattern.
pattern PNat  :: Integer -> APattern
pattern PNat n = PNat_ n 

-- | Successor pattern, @S p@.
pattern PSucc :: APattern -> APattern
pattern PSucc p = PSucc_ p 

-- | Cons pattern @p1 :: p2@.
pattern PCons :: APattern -> APattern -> APattern
pattern PCons  p1 p2 = PCons_ p1 p2 

-- | List pattern @[p1, .., pn]@.
pattern PList :: [APattern] -> APattern
pattern PList lp = PList_ lp 
>>>>>>> 9ba22e5ff3419d0e34d2f1760daa6794247a3b19

instance Alpha ATerm
instance Alpha ABinding
instance Alpha ALink
instance Alpha APattern
instance Alpha AGuard
instance Alpha AQual
instance Alpha APattern

------------------------------------------------------------
-- getType
------------------------------------------------------------

-- | Get the type at the root of an 'ATerm'.
getType :: ATerm -> Type
getType (ATVar ty _)      = ty
getType ATUnit            = TyUnit
getType (ATBool _)        = TyBool
getType (ATNat ty _)      = ty
getType (ATRat _)         = TyQP
getType (ATAbs ty _)      = ty
getType (ATApp ty _ _)    = ty
getType (ATTup ty _)      = ty
getType (ATInj ty _ _)    = ty
getType (ATUn ty _ _)     = ty
getType (ATBin ty _ _ _)  = ty
getType (ATTyOp ty _ _)   = ty
getType (ATChain ty _ _)  = ty
getType (ATList ty _ _)   = ty
getType (ATListComp ty _) = ty
getType (ATLet ty _)      = ty
getType (ATCase ty _)     = ty
getType (ATAscr _ ty)     = ty

{-# COMPLETE ATVar, ATUnit, ATBool, ATNat, ATRat, ATAbs,
    ATApp, ATTup, ATInj, ATUn, ATBin, ATTyOp, ATChain, ATList,
    ATListComp, ATLet, ATCase, ATAscr #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

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
         ATerm(..), getType

       , AProperty
         -- * Branches and guards
       , ABranch, AGuards(..), AGuard(..)
       )
       where

import           Unbound.LocallyNameless

import           Disco.AST.Surface
import           Disco.Types

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.
data ATerm where

  -- | A variable together with its type.
  ATVar   :: Type -> Name ATerm -> ATerm

  -- | The unit value.  We don't bother explicitly storing @TyUnit@
  --   here.
  ATUnit  :: ATerm

  -- | A boolean value. Again, we don't bother explicitly storing
  --   the type.
  ATBool  :: Bool -> ATerm

  -- | The empty list.  The type is inherently ambiguous so we store
  --   it here.
  ATList :: Type -> [ATerm] -> ATerm

  -- | A natural number.
  ATNat   :: Integer -> ATerm

  -- | A rational number.
  ATRat   :: Rational -> ATerm

  -- | Anonymous function, with its type.
  ATAbs   :: Type -> Bind (Name ATerm) ATerm -> ATerm

  -- | A function application, with its type.  Note that typechecking
  --   disambiguates juxtaposition, so at this stage we know for sure
  --   whether we have a function application or a multiplication.
  ATApp   :: Type -> ATerm -> ATerm -> ATerm

  -- | A pair.
  ATPair  :: Type -> ATerm -> ATerm -> ATerm

  -- | A sum type injection.
  ATInj   :: Type -> Side -> ATerm -> ATerm

  -- | A unary operator application.
  ATUn    :: Type -> UOp -> ATerm -> ATerm

  -- | A binary operator application.
  ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm

  -- | A type operator application.
  ATTyOp  :: Type -> TyOp -> Type -> ATerm

  -- | A (non-recursive) let expression.
  ATLet   :: Type -> Bind (Name ATerm, Embed ATerm) ATerm -> ATerm

  -- | A case expression.
  ATCase  :: Type -> [ABranch] -> ATerm

  -- | Type ascription.
  ATAscr  :: ATerm -> Type -> ATerm

  -- | @ATSub@ is used to record the fact that we made use of a
  --   subtyping judgment.  The term has the given type T because its
  --   type is a subtype of T.
  ATSub   :: Type -> ATerm -> ATerm
  deriving Show

  -- TODO: I don't think we are currently very consistent about using ATSub everywhere
  --   subtyping is invoked.  I am not sure how much it matters.

-- | Get the type at the root of an 'ATerm'.
getType :: ATerm -> Type
getType (ATVar ty _)     = ty
getType ATUnit           = TyUnit
getType (ATBool _)       = TyBool
getType (ATNat _)        = TyN
getType (ATRat _)        = TyQ
getType (ATAbs ty _)     = ty
getType (ATApp ty _ _)   = ty
getType (ATPair ty _ _)  = ty
getType (ATInj ty _ _)   = ty
getType (ATUn ty _ _)    = ty
getType (ATBin ty _ _ _) = ty
getType (ATTyOp ty _ _)  = ty
getType (ATList ty _)    = ty
getType (ATLet ty _)     = ty
getType (ATCase ty _)    = ty
getType (ATAscr _ ty)    = ty
getType (ATSub ty _)     = ty

-- | A branch of a case, consisting of a list of guards and a type-annotated term.
type ABranch = Bind AGuards ATerm

-- | A list of guards.
data AGuards where
  AGEmpty :: AGuards
  AGCons  :: Rebind AGuard AGuards -> AGuards
  deriving Show

-- | A single guard (@if@ or @when@) containing a type-annotated term.
data AGuard where

  -- | Boolean guard (@when <test>@)
  AGBool :: Embed ATerm -> AGuard

  -- | Pattern guard (@when term = pat@)
  AGPat  :: Embed ATerm -> Pattern -> AGuard

  deriving Show

type AProperty = Bind [(Name ATerm, Type)] ATerm

derive [''ATerm, ''AGuards, ''AGuard]

instance Alpha ATerm
instance Alpha AGuards
instance Alpha AGuard

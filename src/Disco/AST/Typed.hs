{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PatternSynonyms #-}

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
         ATerm(..), ALink(..), ABinding(..), AProperty
         ,pattern ATList, pattern ATListComp
         -- * Branches and guards
       , ABranch, AGuard(..), AQual(..)

         -- * Utilities
       , getType
       )
       where

import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.Syntax.Operators
import           Disco.Types

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.
data ATerm where

  -- | A variable together with its type.
  ATVar   :: Type -> Name ATerm -> ATerm

  -- | A (non-recursive) let expression.
  ATLet   :: Type -> Bind (Telescope ABinding) ATerm -> ATerm

  -- | The unit value.  We don't bother explicitly storing @TyUnit@
  --   here.
  ATUnit  :: ATerm

  -- | A boolean value. Again, we don't bother explicitly storing
  --   the type.
  ATBool  :: Bool -> ATerm

  -- | A natural number.
  ATNat   :: Type -> Integer -> ATerm

  -- | A nonnegative rational number (parsed and displayed as a
  --   decimal).
  ATRat   :: Rational -> ATerm

  -- | Anonymous function, with its type.
  ATAbs   :: Type -> Bind [(Name ATerm, Embed (Maybe Type))] ATerm -> ATerm

  -- | A function application, with its type.
  ATApp   :: Type -> ATerm -> ATerm -> ATerm

  -- | An n-tuple.
  ATTup   :: Type -> [ATerm] -> ATerm

  -- | A sum type injection.
  ATInj   :: Type -> Side -> ATerm -> ATerm

  -- | A case expression.
  ATCase  :: Type -> [ABranch] -> ATerm

  -- | A unary operator application.
  ATUn    :: Type -> UOp -> ATerm -> ATerm

  -- | A binary operator application.
  ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm

  -- | A chained comparison.
  ATChain :: Type -> ATerm -> [ALink] -> ATerm

  -- | A type operator application.
  ATTyOp  :: Type -> TyOp -> Type -> ATerm

  -- | A literal list.  The type would be ambiguous if the list was
  --   empty.
  ATContainer :: Type -> Container -> [ATerm] -> Maybe (Ellipsis ATerm) -> ATerm

  -- | A list comprehension.
  ATContainerComp :: Type -> Container -> Bind (Telescope AQual) ATerm -> ATerm

  -- | Type ascription.
  ATAscr  :: ATerm -> Type -> ATerm

  deriving (Show, Generic)

pattern ATList t xs e = ATContainer t CList xs e
pattern ATListComp t e = ATContainerComp t CList e

data ALink where
  ATLink :: BOp -> ATerm -> ALink
  deriving (Show, Generic)

data ABinding = ABinding (Maybe Type) (Name ATerm) (Embed ATerm)
  deriving (Show, Generic)

-- | A branch of a case, consisting of a list of guards and a type-annotated term.
type ABranch = Bind (Telescope AGuard) ATerm

-- | A single guard (@if@ or @when@) containing a type-annotated term.
data AGuard where

  -- | Boolean guard (@when <test>@)
  AGBool :: Embed ATerm -> AGuard

  -- | Pattern guard (@when term = pat@)
  AGPat  :: Embed ATerm -> Pattern -> AGuard

  deriving (Show, Generic)

-- Note: very similar to guards
--  maybe some generalization in the future?

-- | A single qualifier in a list comprehension.
data AQual where

  -- | A binding qualifier (i.e. @x <- t@)
  AQBind   :: Name ATerm -> Embed ATerm -> AQual

  -- | A boolean guard qualfier (i.e. @x + y > 4@)
  AQGuard  :: Embed ATerm -> AQual

  deriving (Show, Generic)

type AProperty = Bind [(Name ATerm, Type)] ATerm

instance Alpha ATerm
instance Alpha ABinding
instance Alpha ALink
instance Alpha AGuard
instance Alpha AQual

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
getType (ATContainer ty _ _ _)   = ty
getType (ATContainerComp ty _ _) = ty
getType (ATLet ty _)      = ty
getType (ATCase ty _)     = ty
getType (ATAscr _ ty)     = ty

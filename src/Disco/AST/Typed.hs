{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Disco.AST.Typed where

import           Unbound.LocallyNameless

import           Disco.AST.Surface
import           Disco.Types

-- TODO: Should probably really do this with a 2-level/open recursion
-- approach, with a cofree comonad or whatever

-- | An @ATerm@ is a typechecked term where every node in the tree has
--   been annotated with the type of the subterm rooted at that node.
data ATerm where
  ATVar   :: Type -> Name ATerm -> ATerm                -- ^ Variable with its type.
  ATUnit  :: ATerm                                      -- ^ Unit.  We don't bother
                                                        --   storing TyUnit here.
  ATBool  :: Bool -> ATerm                              -- ^ Bool.  Don't bother storing
                                                        --   the type.
  ATAbs   :: Type -> Bind (Name ATerm) ATerm -> ATerm   -- ^ Abstraction.
  ATApp   :: Type -> ATerm -> ATerm -> ATerm            -- ^ Application.
  ATPair  :: Type -> ATerm -> ATerm -> ATerm            -- ^ Pair.
  ATInj   :: Type -> Side -> ATerm -> ATerm             -- ^ Injection.
  ATNat   :: Integer -> ATerm                           -- ^ Natural number.
  ATUn    :: Type -> UOp -> ATerm -> ATerm              -- ^ Unary operator.
  ATBin   :: Type -> BOp -> ATerm -> ATerm -> ATerm     -- ^ Binary operator.
  ATLet   :: Type -> Bind (Name ATerm, Embed ATerm) ATerm -> ATerm  -- ^ (Non-recursive) let.
  ATCase  :: Type -> [ABranch] -> ATerm                 -- ^ Case expression.
  ATAscr  :: ATerm -> Type -> ATerm                     -- ^ Ascription.
  ATSub   :: Type -> ATerm -> ATerm                     -- ^ @ATSub@ is used to record
                                                        --   the fact that we made use of
                                                        --   a subtyping judgment.
                                                        --   The term has the given type T
                                                        --   because its type is a subtype
                                                        --   of T.
  deriving Show
  -- TODO: I don't think we are currently very consistent about using ATSub everywhere
  --   subtyping is invoked.  I am not sure how much it matters.

type ABranch = Bind AGuards ATerm

data AGuards where
  AGEmpty :: AGuards
  AGCons  :: Rebind AGuard AGuards -> AGuards
  deriving Show

data AGuard where
  AGIf   :: Embed ATerm -> AGuard             -- ^ Boolean guard (if <test>)
  AGWhen :: Embed ATerm -> Pattern -> AGuard  -- ^ Pattern guard (when term = pat)
  deriving Show

derive [''ATerm, ''AGuards, ''AGuard]

instance Alpha ATerm
instance Alpha AGuards
instance Alpha AGuard

-- | Get the type at the root of an 'ATerm'.
getType :: ATerm -> Type
getType (ATVar ty _)     = ty
getType ATUnit           = TyUnit
getType (ATBool _)       = TyBool
getType (ATAbs ty _)     = ty
getType (ATApp ty _ _)   = ty
getType (ATPair ty _ _)  = ty
getType (ATInj ty _ _)   = ty
getType (ATNat _)        = TyN
getType (ATUn ty _ _)    = ty
getType (ATBin ty _ _ _) = ty
getType (ATLet ty _)     = ty
getType (ATCase ty _)    = ty
getType (ATAscr _ ty)    = ty
getType (ATSub ty _)     = ty


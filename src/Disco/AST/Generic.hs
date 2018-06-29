{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Generic
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Abstract syntax trees representing the generic syntax of the Disco
-- language. Concrete AST instances may use this module as a template.
-----------------------------------------------------------------------------

module Disco.AST.Generic
       (
       Term_ (..)
      , X_TVar
      , X_TLet
      , X_TParens
      , X_TUnit
      , X_TBool
      , X_TNat
      , X_TRat
      , X_TAbs
      , X_TApp
      , X_TInj
      , X_TCase
      , X_TUn
      , X_TBin
      , X_TChain
      , X_TTyop
      , X_TContainer
      , X_TContainerComp
      , X_TAscr
      , X_Term
      , X_TTup
      , Telescope (..)
      , Container (..)
      , Link_ (..)
      , X_TLink
      , Qual_ (..)
      , X_QBind
      , X_QGuard
      , Binding_ (..)
      , Branch_
      , Guard_ (..)
      , X_GBool
      , X_GPat
      , Forall_t
      , Side (..)
      , Ellipsis (..)
      , Pattern_ (..)
      , X_PVar
      , X_PWild
      , X_PUnit
      , X_PBool
      , X_PTup
      , X_PInj
      , X_PNat
      , X_PSucc
      , X_PCons
      , X_PList
      , foldTelescope
      , mapTelescope
      , toTelescope
      , fromTelescope
      , Property_
       )
       where

import           GHC.Generics                     (Generic)

import           Unbound.Generics.LocallyNameless

import           Disco.Syntax.Operators
import           Disco.Types
import           GHC.Exts                         (Constraint)
------------------------------------------------------------
-- Telescopes
------------------------------------------------------------

-- | A telescope is essentially a list, except that each item can bind
--   names in the rest of the list.
data Telescope b where

  -- | The empty telescope.
  TelEmpty :: Telescope b

  -- | A binder of type @b@ followed by zero or more @b@'s.  This @b@
  --   can bind variables in the subsequent @b@'s.
  TelCons  :: Rebind b (Telescope b) -> Telescope b
  deriving (Show, Generic)

instance Alpha b => Alpha (Telescope b)

-- | Fold a telescope given a combining function and a value for the
--   empty telescope.
foldTelescope :: Alpha b => (b -> r -> r) -> r -> Telescope b -> r
foldTelescope _ z TelEmpty                       = z
foldTelescope f z (TelCons (unrebind -> (b,bs))) = f b (foldTelescope f z bs)

-- | Map over a telescope.
mapTelescope :: (Alpha a, Alpha b) => (a -> b) -> Telescope a -> Telescope b
mapTelescope f = toTelescope . map f . fromTelescope

-- | Convert a list to a telescope.
toTelescope :: Alpha b => [b] -> Telescope b
toTelescope []     = TelEmpty
toTelescope (b:bs) = TelCons (rebind b (toTelescope bs))

-- | Convert a telescope to a list.
fromTelescope :: Alpha b => Telescope b -> [b]
fromTelescope = foldTelescope (:) []

type Property_ e = Bind [(Name (Term_ e), Type)] (Term_ e)

------------------------------------------------------------
-- Terms
------------------------------------------------------------

-- | Injections into a sum type (@inl@ or @inr@) have a "side" (@L@ or @R@).
data Side = L | R
  deriving (Show, Eq, Enum, Generic)

instance Alpha Side

type family X_TVar e
type family X_TLet e
type family X_TParens e
type family X_TUnit e
type family X_TBool e
type family X_TNat e
type family X_TRat e
type family X_TAbs e
type family X_TApp e
type family X_TInj e
type family X_TCase e
type family X_TUn e
type family X_TBin e
type family X_TChain e
type family X_TTyop e
type family X_TContainer e
type family X_TContainerComp e
type family X_TAscr e
type family X_Term e
type family X_TTup e

data Term_ e where

  -- | A variable.
  TVar_   :: X_TVar e -> Name (Term_ e) -> Term_ e

  -- | A (non-recursive) let expression, @let x1 = t1, x2 = t2, ... in t@.
  TLet_   :: X_TLet e -> Bind (Telescope (Binding_ e)) (Term_ e) -> Term_ e

  -- | Explicit parentheses.  We need to keep track of these in order
  --   to syntactically distinguish multiplication and function
  --   application.
  TParens_ :: X_TParens e -> Term_ e -> Term_ e

  -- | The unit value, (), of type Unit.
  TUnit_  :: X_TUnit e -> Term_ e

  -- | True or false.
  TBool_  :: X_TBool e -> Bool -> Term_ e

  -- | A natural number.
  TNat_   :: X_TNat e -> Integer -> Term_ e

  -- | A nonnegative rational number, parsed as a decimal.
  TRat_   :: X_TRat e -> Rational -> Term_ e

  -- | An anonymous function.
  TAbs_   :: X_TAbs e -> Bind [(Name (Term_ e), Embed (Maybe Type))] (Term_ e) -> Term_ e

  -- | Function application.
  TApp_  :: X_TApp e -> Term_ e -> Term_ e -> Term_ e

  -- | An n-tuple, @(t1, ..., tn)@.
  TTup_   :: X_TTup e -> [Term_ e] -> Term_ e

  -- | An injection into a sum type.
  TInj_   :: X_TInj e -> Side -> Term_ e -> Term_ e

  -- | A case expression.
  TCase_  :: X_TCase e -> [Branch_ e] -> Term_ e

  -- | An application of a unary operator.
  TUn_    :: X_TUn e -> UOp -> Term_ e -> Term_ e

  -- | An application of a binary operator.
  TBin_   :: X_TBin e -> BOp -> Term_ e -> Term_ e -> Term_ e

  -- | A chained comparison.  Should contain only comparison
  --   operators.
  TChain_ :: X_TChain e -> Term_ e -> [Link_ e] -> Term_ e

  -- | An application of a type operator.
  TTyOp_  :: X_TTyop e -> TyOp -> Type -> Term_ e

  -- | A containter for sets and lsits.
  TContainer_ :: X_TContainer e -> Container -> [Term_ e] -> Maybe (Ellipsis (Term_ e)) -> Term_ e

  -- | A container for set and list comprehensions
  TContainerComp_ :: X_TContainerComp e -> Container -> Bind (Telescope (Qual_ e)) (Term_ e) -> Term_ e

  -- | Type ascription, @(Term_ e : type)@.
  TAscr_  :: X_TAscr e -> Term_ e -> Type -> Term_ e

  -- | A data constructor with an extension descriptor that a "concrete"
  --   implementation of a generic AST may use to carry extra information.
  XTerm_   :: X_Term e -> Term_ e
  deriving (Generic)

-- A type that abstracts over constraints for generic data constructors. 
-- This makes it easier to derive typeclass instances for generic types.
type Forall_t (a :: * -> Constraint) e
      = (a (X_TVar e), a (X_TLet e),
         a (X_TParens e), a (X_TUnit e),
         a (X_TBool e), a (X_TNat e),
         a (X_TRat e), a (X_TAbs e),
         a (X_TApp e), a (X_TInj e),
         a (X_TCase e), a (X_TUn e),
         a (X_TBin e), a (X_TChain e),
         a (X_TTyop e), a (X_TContainer e),
         a (X_TContainerComp e), a (X_TAscr e),
         a (X_Term e), a (X_TTup e),
         a (X_QBind e), a (X_QGuard e),
         a (X_GBool e), a (X_GPat e),
         a (X_TLink e), a (Binding_ e),
         a (X_PVar e), a (X_PWild e), a (X_PUnit e),
         a (X_PBool e), a (X_PTup e), a (X_PInj e),
         a (X_PNat e), a (X_PSucc e), a (X_PCons e),
         a (X_PList e))

deriving instance Forall_t Show e => Show (Term_ e)

type family X_TLink e

data Link_ e where
  TLink_ :: X_TLink e -> BOp -> Term_ e -> Link_ e
  deriving Generic

deriving instance (Show (X_TLink e), Show (Term_ e)) => Show (Link_ e)

-- | A container is a wrapper for sets and lists.
data Container where
  CList :: Container
  CSet :: Container
  deriving (Show, Eq, Enum, Generic)

instance Alpha Container

-- | An ellipsis is an "omitted" part of a literal container (such as a list or set), of the form
--   @..@ or @.. t@.
data Ellipsis t where
  Forever ::      Ellipsis t   -- @..@
  Until   :: t -> Ellipsis t   -- @.. t@
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance Alpha t => Alpha (Ellipsis t)


type family X_QBind e
type family X_QGuard e

data Qual_ e where

  -- | A binding qualifier (i.e. @x <- t@)
  QBind_   :: X_QBind e -> Name (Term_ e) -> Embed (Term_ e) -> Qual_ e

  -- | A boolean guard qualfier (i.e. @x + y > 4@)
  QGuard_  :: X_QGuard e -> Embed (Term_ e) -> Qual_ e

  deriving Generic

deriving instance (Show (X_QBind e), Show (X_QGuard e), Show (Term_ e)) => Show (Qual_ e)

-- | A binding is a name along with its definition.
data Binding_ e = Binding_ (Maybe Type) (Name (Term_ e)) (Embed (Term_ e))
  deriving (Generic)

deriving instance Forall_t Show  e => Show (Binding_ e)

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.

type Branch_ e = Bind (Telescope (Guard_ e)) (Term_ e)


type family X_GBool e
type family X_GPat e

data Guard_ e where

  -- | Boolean guard (@if <test>@)
  GBool_ :: X_GBool e -> Embed (Term_ e) -> Guard_ e

  -- | Pattern guard (@when term = pat@)
  GPat_  :: X_GPat e -> Embed (Term_ e) -> Pattern_ e -> Guard_ e

  deriving Generic

deriving instance Forall_t Show  e => Show (Guard_ e)

type family X_PVar e
type family X_PWild e
type family X_PUnit e
type family X_PBool e
type family X_PTup e
type family X_PInj e
type family X_PNat e
type family X_PSucc e
type family X_PCons e
type family X_PList e


-- | Patterns.
data Pattern_ e where

  -- | Variable pattern: matches anything and binds the variable.
  PVar_  :: X_PVar e -> Name (Term_ e) -> Pattern_ e

  -- | Wildcard pattern @_@: matches anything.
  PWild_ :: X_PWild e -> Pattern_ e

  -- | Unit pattern @()@: matches @()@.
  PUnit_ :: X_PUnit e -> Pattern_ e

  -- | Literal boolean pattern.
  PBool_ :: X_PBool e -> Bool -> Pattern_ e

  -- | Tuple pattern @(pat1, .. , patn)@.
  PTup_  :: X_PTup e -> [Pattern_ e] -> Pattern_ e

  -- | Injection pattern (@inl pat@ or @inr pat@).
  PInj_  :: X_PInj e -> Side -> Pattern_ e -> Pattern_ e

  -- | Literal natural number pattern.
  PNat_  :: X_PNat e -> Integer -> Pattern_ e

  -- | Successor pattern, @S p@.
  PSucc_ :: X_PSucc e -> Pattern_ e -> Pattern_ e

  -- | Cons pattern @p1 :: p2@.
  PCons_ :: X_PCons e -> Pattern_ e -> Pattern_ e -> Pattern_ e

  -- | List pattern @[p1, .., pn]@.
  PList_ :: X_PList e -> [Pattern_ e] -> Pattern_ e

  deriving (Generic)

deriving instance (Show (X_PVar e), Show (X_PWild e), Show (X_PUnit e),
                   Show (X_PTup e), Show (X_PInj e), Show (X_PNat e),
                   Show (X_PSucc e), Show (X_PCons e), Show (X_PList e),
                   Show (X_PBool e), Show (Term_ e)) => Show (Pattern_ e)


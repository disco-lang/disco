{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- Orphan Alpha Void instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Generic
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- Abstract syntax trees representing the generic syntax of the Disco
-- language. Concrete AST instances may use this module as a template.
--
-- For more detail on the approach used here, see
--
-- Najd and Peyton Jones, "Trees that Grow". Journal of Universal
-- Computer Science, vol. 23 no. 1 (2017), 42-62.
-- <https://arxiv.org/abs/1610.04799>
--
-- Essentially, we define a basic generic 'Term_' type, with a type
-- index to indicate what kind of term it is, i.e. what phase the term
-- belongs to.  Each constructor has a type family used to define any
-- extra data that should go in the constructor for a particular
-- phase; there is also one additional constructor which can be used
-- to store arbitrary additional information, again governed by a type
-- family.  Together with the use of pattern synonyms, the result is
-- that it looks like we have a different type for each phase, each
-- with its own set of constructors, but in fact all use the same
-- underlying type.  Particular instantiations of the generic
-- framework here can be found in "Disco.AST.Surface",
-- "Disco.AST.Typed", and "Disco.AST.Desugared".
-----------------------------------------------------------------------------

-- SPDX-License-Identifier: BSD-3-Clause

module Disco.AST.Generic
       ( -- * Telescopes

         Telescope (..), telCons
       , foldTelescope, mapTelescope
       , traverseTelescope
       , toTelescope, fromTelescope

         -- * Utility types

       , Side (..), selectSide
       , Container (..)
       , Ellipsis (..)

         -- * Term

       , Term_ (..)

       , X_TVar
       , X_TPrim
       , X_TLet
       , X_TParens
       , X_TUnit
       , X_TBool
       , X_TNat
       , X_TRat
       , X_TChar
       , X_TString
       , X_TAbs
       , X_TApp
       , X_TTup
       , X_TInj
       , X_TCase
       , X_TChain
       , X_TTyOp
       , X_TContainer
       , X_TContainerComp
       , X_TAscr
       , X_Term

       , ForallTerm

       -- * Link

       , Link_ (..)
       , X_TLink
       , ForallLink

       -- * Qual

       , Qual_ (..)
       , X_QBind
       , X_QGuard
       , ForallQual

       -- * Binding

       , Binding_ (..)

       -- * Branch
       , Branch_

       -- * Guard

       , Guard_ (..)
       , X_GBool
       , X_GPat
       , X_GLet
       , ForallGuard

       -- * Pattern

       , Pattern_ (..)
       , X_PVar
       , X_PWild
       , X_PAscr
       , X_PUnit
       , X_PBool
       , X_PTup
       , X_PInj
       , X_PNat
       , X_PChar
       , X_PString
       , X_PCons
       , X_PList
       , X_PAdd
       , X_PMul
       , X_PSub
       , X_PNeg
       , X_PFrac
       , X_Pattern
       , ForallPattern

       -- * Quantifiers

       , Quantifier(..)
       , Binder_
       , X_Binder

       -- * Property

       , Property_
       )
       where

import           Data.Typeable
import           GHC.Generics                     (Generic)

import           Data.Void
import           Unbound.Generics.LocallyNameless

import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
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
instance Subst t b => Subst t (Telescope b)

-- | Add a new item to the beginning of a 'Telescope'.
telCons :: Alpha b => b -> Telescope b -> Telescope b
telCons b tb = TelCons (rebind b tb)

-- | Fold a telescope given a combining function and a value to use
--   for the empty telescope.  Analogous to 'foldr' for lists.
foldTelescope :: Alpha b => (b -> r -> r) -> r -> Telescope b -> r
foldTelescope _ z TelEmpty                       = z
foldTelescope f z (TelCons (unrebind -> (b,bs))) = f b (foldTelescope f z bs)

-- | Apply a function to every item in a telescope.
mapTelescope :: (Alpha a, Alpha b) => (a -> b) -> Telescope a -> Telescope b
mapTelescope f = toTelescope . map f . fromTelescope

-- | Traverse over a telescope.
traverseTelescope
  :: (Applicative f, Alpha a, Alpha b)
  => (a -> f b) -> Telescope a -> f (Telescope b)
traverseTelescope f = foldTelescope (\a ftb -> telCons <$> f a <*> ftb) (pure TelEmpty)

-- | Convert a list to a telescope.
toTelescope :: Alpha b => [b] -> Telescope b
toTelescope = foldr telCons TelEmpty

-- | Convert a telescope to a list.
fromTelescope :: Alpha b => Telescope b -> [b]
fromTelescope = foldTelescope (:) []

------------------------------------------------------------
-- Utility types
------------------------------------------------------------

-- | Injections into a sum type (@inl@ or @inr@) have a "side" (@L@ or @R@).
data Side = L | R
  deriving (Show, Eq, Enum, Generic)

instance Alpha Side
instance Subst t Side

-- | Use a 'Side' to select one of two arguments (the first argument
--   for 'L', and the second for 'R').
selectSide :: Side -> a -> a -> a
selectSide L a _ = a
selectSide R _ b = b

-- | An enumeration of the different kinds of containers in disco:
--   lists, bags, and sets.
data Container where
  ListContainer :: Container
  BagContainer  :: Container
  SetContainer  :: Container
  deriving (Show, Eq, Enum, Generic)

instance Alpha Container
instance Subst t Container

-- | An ellipsis is an "omitted" part of a literal container (such as
--   a list or set), of the form @..@ or @.. t@.
data Ellipsis t where
  -- | 'Forever' represents an open-ended ellipsis, as in @[3 ..]@.
  Forever ::      Ellipsis t

  -- | 'Until' represents an ellipsis with a given endpoint, as in @[3 .. 20]@.
  Until   :: t -> Ellipsis t   -- @.. t@
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance Alpha t => Alpha (Ellipsis t)
instance Subst a t => Subst a (Ellipsis t)

------------------------------------------------------------
-- Terms
------------------------------------------------------------

type family X_TVar e
type family X_TPrim e
type family X_TLet e
type family X_TParens e
type family X_TUnit e
type family X_TBool e
type family X_TNat e
type family X_TRat e
type family X_TChar e
type family X_TString e
type family X_TAbs e
type family X_TApp e
type family X_TTup e
type family X_TInj e
type family X_TCase e
type family X_TChain e
type family X_TTyOp e
type family X_TContainer e
type family X_TContainerComp e
type family X_TAscr e
type family X_Term e

-- | The base generic AST representing terms in the disco language.
--   @e@ is a type index indicating the kind of term, i.e. the phase
--   (for example, surface, typed, or desugared).  Type families like
--   'X_TVar' and so on use the phase index to determine what extra
--   information (if any) should be stored in each constructor.  For
--   example, in the typed phase many constructors store an extra
--   type, giving the type of the term.
data Term_ e where

  -- | A term variable.
  TVar_   :: X_TVar e -> Name (Term_ e) -> Term_ e

  -- | A primitive, /i.e./ a constant  which is interpreted specially
  --   at runtime.  See "Disco.Syntax.Prims".
  TPrim_  :: X_TPrim e -> Prim -> Term_ e

  -- | A (non-recursive) let expression, @let x1 = t1, x2 = t2, ... in t@.
  TLet_   :: X_TLet e -> Bind (Telescope (Binding_ e)) (Term_ e) -> Term_ e

  -- | Explicit parentheses.  We need to keep track of these in the
  --   surface syntax in order to syntactically distinguish
  --   multiplication and function application.  However, note that
  --   these disappear after the surface syntax phase.
  TParens_ :: X_TParens e -> Term_ e -> Term_ e

  -- | The unit value, (), of type Unit.
  TUnit_  :: X_TUnit e -> Term_ e

  -- | A boolean value.
  TBool_  :: X_TBool e -> Bool -> Term_ e

  -- | A natural number.
  TNat_   :: X_TNat e -> Integer -> Term_ e

  -- | A nonnegative rational number, parsed as a decimal.  (Note
  --   syntax like @3/5@ does not parse as a rational, but rather as
  --   the application of a division operator to two natural numbers.)
  TRat_   :: X_TRat e -> Rational -> Term_ e

  -- | A literal unicode character, /e.g./ @'d'@.
  TChar_  :: X_TChar e -> Char -> Term_ e

  -- | A string literal, /e.g./ @"disco"@.
  TString_ :: X_TString e -> [Char] -> Term_ e

  -- | A binding abstraction, of the form @Q vars. expr@ where @Q@ is
  --   a quantifier and @vars@ is a list of bound variables and
  --   optional type annotations.  In particular, this could be a
  --   lambda abstraction, /i.e./ an anonymous function (/e.g./ @\x,
  --   (y:N). 2x + y@), a universal quantifier (@forall x, (y:N). x^2 +
  --   y > 0@), or an existential quantifier (@exists x, (y:N). x^2 + y
  --   == 0@).
  TAbs_   :: Quantifier -> X_TAbs e -> Binder_ e (Term_ e) -> Term_ e

  -- | Function application, @t1 t2@.
  TApp_   :: X_TApp e -> Term_ e -> Term_ e -> Term_ e

  -- | An n-tuple, @(t1, ..., tn)@.
  TTup_   :: X_TTup e -> [Term_ e] -> Term_ e

  -- | An injection into a sum type, @left x@ or @right x@.
  TInj_   :: X_TInj e -> Side -> Term_ e -> Term_ e

  -- | A case expression.
  TCase_  :: X_TCase e -> [Branch_ e] -> Term_ e

  -- | A chained comparison, consisting of a term followed by one or
  --   more "links", where each link is a comparison operator and
  --   another term.
  TChain_ :: X_TChain e -> Term_ e -> [Link_ e] -> Term_ e

  -- | An application of a type operator.
  TTyOp_  :: X_TTyOp e -> TyOp -> Type -> Term_ e

  -- | A containter literal (set, bag, or list).
  TContainer_ :: X_TContainer e -> Container -> [(Term_ e, Maybe (Term_ e))] -> Maybe (Ellipsis (Term_ e)) -> Term_ e

  -- | A container comprehension.
  TContainerComp_ :: X_TContainerComp e -> Container -> Bind (Telescope (Qual_ e)) (Term_ e) -> Term_ e

  -- | Type ascription, @(Term_ e : type)@.
  TAscr_  :: X_TAscr e -> Term_ e -> PolyType -> Term_ e

  -- | A data constructor with an extension descriptor that a "concrete"
  --   implementation of a generic AST may use to carry extra information.
  XTerm_   :: X_Term e -> Term_ e
  deriving (Generic)

-- A type that abstracts over constraints for generic data constructors.
-- This makes it easier to derive typeclass instances for generic types.
type ForallTerm (a :: * -> Constraint) e
  = ( a (X_TVar e)
    , a (X_TPrim e)
    , a (X_TLet e)
    , a (X_TParens e)
    , a (X_TUnit e)
    , a (X_TBool e)
    , a (X_TNat e)
    , a (X_TRat e)
    , a (X_TChar e)
    , a (X_TString e)
    , a (X_TAbs e)
    , a (X_TApp e)
    , a (X_TInj e)
    , a (X_TCase e)
    , a (X_TTup e)
    , a (X_TChain e)
    , a (X_TTyOp e)
    , a (X_TContainer e)
    , a (X_TContainerComp e)
    , a (X_TAscr e)
    , a (X_Term e)
    , a (Qual_ e)
    , a (Guard_ e)
    , a (Link_ e)
    , a (Binding_ e)
    , a (Pattern_ e)
    , a (Binder_ e (Term_ e))
    )

deriving instance ForallTerm Show e => Show (Term_ e)
instance
  ( Typeable e
  , ForallTerm (Subst Type) e
  , ForallTerm Alpha e
  )
  => Subst Type (Term_ e)
instance (Typeable e, ForallTerm Alpha e) => Alpha (Term_ e)

------------------------------------------------------------
-- Link
------------------------------------------------------------

type family X_TLink e

-- | A "link" is a comparison operator and a term; a single term
--   followed by a sequence of links makes up a comparison chain, such
--   as @2 < x < y < 10@.
data Link_ e where

  -- | Note that although the type of 'TLink_' says it can hold any
  --   'BOp', it should really only hold comparison operators.
  TLink_ :: X_TLink e -> BOp -> Term_ e -> Link_ e
  deriving Generic

type ForallLink (a :: * -> Constraint) e
  = ( a (X_TLink e)
    , a (Term_ e)
    )

deriving instance ForallLink Show e         => Show       (Link_ e)
instance          ForallLink (Subst Type) e => Subst Type (Link_ e)
instance (Typeable e, Show (Link_ e), ForallLink Alpha e) => Alpha (Link_ e)

------------------------------------------------------------
-- Qual
------------------------------------------------------------

type family X_QBind e
type family X_QGuard e

-- | A container comprehension consists of a head term and then a list
--   of qualifiers. Each qualifier either binds a variable to some
--   collection or consists of a boolean guard.
data Qual_ e where

  -- | A binding qualifier (i.e. @x in t@).
  QBind_   :: X_QBind e -> Name (Term_ e) -> Embed (Term_ e) -> Qual_ e

  -- | A boolean guard qualfier (i.e. @x + y > 4@).
  QGuard_  :: X_QGuard e -> Embed (Term_ e) -> Qual_ e

  deriving Generic

type ForallQual (a :: * -> Constraint) e
  = ( a (X_QBind e)
    , a (X_QGuard e)
    , a (Term_ e)
    )

deriving instance ForallQual Show         e => Show       (Qual_ e)
instance          ForallQual (Subst Type) e => Subst Type (Qual_ e)
instance (Typeable e, ForallQual Alpha e) => Alpha (Qual_ e)

------------------------------------------------------------
-- Binding
------------------------------------------------------------

-- | A binding is a name along with its definition, and optionally its
--   type.
data Binding_ e = Binding_ (Maybe (Embed PolyType)) (Name (Term_ e)) (Embed (Term_ e))
  deriving (Generic)

deriving instance ForallTerm Show  e => Show (Binding_ e)
instance Subst Type (Term_ e) => Subst Type (Binding_ e)
instance (Typeable e, Show (Binding_ e), Alpha (Term_ e)) => Alpha (Binding_ e)

------------------------------------------------------------
-- Branch
------------------------------------------------------------

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.

type Branch_ e = Bind (Telescope (Guard_ e)) (Term_ e)

------------------------------------------------------------
-- Guard
------------------------------------------------------------

type family X_GBool e
type family X_GPat e
type family X_GLet e

-- | Guards in case expressions.
data Guard_ e where

  -- | Boolean guard (@if <test>@)
  GBool_ :: X_GBool e -> Embed (Term_ e) -> Guard_ e

  -- | Pattern guard (@when term = pat@)
  GPat_  :: X_GPat e -> Embed (Term_ e) -> Pattern_ e -> Guard_ e

  -- | Let (@let x = term@)
  GLet_  :: X_GLet e -> Binding_ e -> Guard_ e

  deriving Generic

type ForallGuard (a :: * -> Constraint) e
  = ( a (X_GBool e)
    , a (X_GPat  e)
    , a (X_GLet  e)
    , a (Term_ e)
    , a (Pattern_ e)
    , a (Binding_ e)
    )

deriving instance ForallGuard Show         e => Show       (Guard_ e)
instance          ForallGuard (Subst Type) e => Subst Type (Guard_ e)
instance (Typeable e, Show (Guard_ e), ForallGuard Alpha e) => Alpha (Guard_ e)

------------------------------------------------------------
-- Pattern
------------------------------------------------------------

type family X_PVar e
type family X_PWild e
type family X_PAscr e
type family X_PUnit e
type family X_PBool e
type family X_PTup e
type family X_PInj e
type family X_PNat e
type family X_PChar e
type family X_PString e
type family X_PCons e
type family X_PList e
type family X_PAdd e
type family X_PMul e
type family X_PSub e
type family X_PNeg e
type family X_PFrac e
type family X_Pattern e

-- | Patterns.
data Pattern_ e where

  -- | Variable pattern: matches anything and binds the variable.
  PVar_  :: X_PVar e -> Name (Term_ e) -> Pattern_ e

  -- | Wildcard pattern @_@: matches anything.
  PWild_ :: X_PWild e -> Pattern_ e

  -- | Type ascription pattern @pat : ty@.
  PAscr_ :: X_PAscr e -> Pattern_ e -> Type -> Pattern_ e

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

  -- | Unicode character pattern
  PChar_ :: X_PChar e -> Char -> Pattern_ e

  -- | String pattern.
  PString_ :: X_PString e -> String -> Pattern_ e

  -- | Cons pattern @p1 :: p2@.
  PCons_ :: X_PCons e -> Pattern_ e -> Pattern_ e -> Pattern_ e

  -- | List pattern @[p1, .., pn]@.
  PList_ :: X_PList e -> [Pattern_ e] -> Pattern_ e

  -- | Addition pattern, @p + t@ or @t + p@
  PAdd_  :: X_PAdd e -> Side -> Pattern_ e -> Term_ e -> Pattern_ e

  -- | Multiplication pattern, @p * t@ or @t * p@
  PMul_  :: X_PMul e -> Side -> Pattern_ e -> Term_ e -> Pattern_ e

  -- | Subtraction pattern, @p - t@
  PSub_  :: X_PSub e -> Pattern_ e -> Term_ e -> Pattern_ e

  -- | Negation pattern, @-p@
  PNeg_  :: X_PNeg e -> Pattern_ e -> Pattern_ e

  -- | Fraction pattern, @p1/p2@
  PFrac_ :: X_PFrac e -> Pattern_ e -> Pattern_ e -> Pattern_ e

  -- | Expansion slot.
  XPattern_ :: X_Pattern e -> Pattern_ e

  deriving (Generic)

type ForallPattern (a :: * -> Constraint) e
      = ( a (X_PVar e)
        , a (X_PWild e)
        , a (X_PAscr e)
        , a (X_PUnit e)
        , a (X_PBool e)
        , a (X_PNat e)
        , a (X_PChar e)
        , a (X_PString e)
        , a (X_PTup e)
        , a (X_PInj e)
        , a (X_PCons e)
        , a (X_PList e)
        , a (X_PAdd e)
        , a (X_PMul e)
        , a (X_PSub e)
        , a (X_PNeg e)
        , a (X_PFrac e)
        , a (X_Pattern e)
        , a (Term_ e)
        )

deriving instance ForallPattern Show         e => Show       (Pattern_ e)
instance          ForallPattern (Subst Type) e => Subst Type (Pattern_ e)
instance (Typeable e, Show (Pattern_ e), ForallPattern Alpha e) => Alpha (Pattern_ e)

------------------------------------------------------------
-- Quantifiers and binders
------------------------------------------------------------

-- | A type family specifying what the binder in an abstraction can be.
--   Should have at least variables in it, but how many variables and
--   what other information is carried along may vary.
type family X_Binder e

-- | A binder represents the stuff between the quantifier and the body
--   of a lambda, ∀, or ∃ abstraction, as in @x : N, r : F@.
type Binder_ e a = Bind (X_Binder e) a

-- | A quantifier: λ, ∀, or ∃
data Quantifier = Lam | Ex | All
  deriving (Generic, Eq, Ord, Show)

instance Subst Type Quantifier
instance Alpha Quantifier

------------------------------------------------------------
-- Property
------------------------------------------------------------

-- | A property is just a term (of type Prop).
type Property_ e = Term_ e

------------------------------------------------------------
-- Orphan instances
------------------------------------------------------------

-- Need this if we want to put 'Void' as the type
-- of an extension slot (to kill a constructor)
instance Alpha Void

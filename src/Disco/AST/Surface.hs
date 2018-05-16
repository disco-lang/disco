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
-- Module      :  Disco.AST.Surface
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Abstract syntax trees representing the surface syntax of the Disco
-- language.
--
-----------------------------------------------------------------------------

module Disco.AST.Surface
       ( -- * Modules
         Module(..), TopLevel(..)
         -- ** Documentation
       , Docs, DocThing(..), Property
         -- ** Declarations
       , Decl(..), declName, isDefn

         -- * Terms

       , Term(..)

         -- ** Telescopes
       , Telescope(..), foldTelescope, mapTelescope, toTelescope, fromTelescope

         -- ** Expressions
       , Side(..), Link(..), Binding(..)

         -- ** Lists
       , Qual(..), Ellipsis(..)

         -- ** Case expressions and patterns
       , Branch, Guard(..), Pattern(..)
       )
       where

import           GHC.Generics                     (Generic)

import           Unbound.Generics.LocallyNameless

import           Disco.Context
import           Disco.Syntax.Operators
import           Disco.Types
import           GHC.Exts (Constraint)
-- | A module is a list of declarations together with a collection of
--   documentation for top-level names.
data Module = Module [Decl] (Ctx Term Docs)
  deriving Show

-- | A @TopLevel@ is either documentation (a 'DocThing') or a
--   declaration ('Decl').
data TopLevel = TLDoc DocThing | TLDecl Decl
  deriving Show

-- | Convenient synonym for a list of 'DocThing's.
type Docs = [DocThing]

-- | An item of documentation.
data DocThing
  = DocString   [String]    -- ^ A documentation string, i.e. a block
                            --   of @||| text@ items
  | DocProperty Property    -- ^ An example/doctest/property of the
                            --   form @!!! forall (x1:ty1) ... . property@
  deriving Show

-- | A property is a universally quantified term of the form
--   @forall v1 : T1, v2 : T2. term@.
type Property = Bind [(Name Term, Type)] Term

-- | A declaration is either a type declaration or a definition.
data Decl where

  -- | A type declaration, @name : type@.
  DType :: Name Term -> Type -> Decl

  -- | A group of definition clauses of the form @name pat1 .. patn = term@. The
  --   patterns bind variables in the term. For example, @f n (x,y) =
  --   n*x + y@.
  DDefn :: Name Term -> [Bind [Pattern] Term] -> Decl
  deriving Show

-- | Get the name that a declaration is about.
declName :: Decl -> Name Term
declName (DType x _) = x
declName (DDefn x _) = x

-- | Check whether a declaration is a definition.
isDefn :: Decl -> Bool
isDefn DDefn{} = True
isDefn _       = False

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
foldTelescope _ z TelEmpty = z
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

------------------------------------------------------------
-- Terms
------------------------------------------------------------

-- | Injections into a sum type (@inl@ or @inr@) have a "side" (@L@ or @R@).
data Side = L | R
  deriving (Show, Eq, Enum, Generic)


data Term_ e where

  -- | A variable.
  TVar_   :: T_var e -> Name (Term_ e) -> Term_ e

  -- | A (non-recursive) let expression, @let x1 = t1, x2 = t2, ... in t@.
  TLet_   :: T_let e -> Bind (Telescope Binding) (Term_ e) -> Term_ e

  -- | Explicit parentheses.  We need to keep track of these in order
  --   to syntactically distinguish multiplication and function
  --   application.
  TParens_ :: T_parens e -> Term_ e -> Term_ e

  -- | The unit value, (), of type Unit.
  TUnit_  :: T_unit e -> Term_ e

  -- | True or false.
  TBool_  :: T_bool e -> Bool -> Term_ e

  -- | A natural number.
  TNat_   :: T_nat e -> Integer -> Term_ e

  -- | A nonnegative rational number, parsed as a decimal.
  TRat_   :: T_rat e -> Rational -> Term_ e

  -- | An anonymous function.
  TAbs_   :: T_abs e -> Bind [(Name (Term_ e), Embed (Maybe Type))] (Term_ e) -> Term_ e

  -- | Function application.
  TApp_  :: T_app e -> Term_ e -> Term_ e -> Term_ e

  -- | An n-tuple, @(t1, ..., tn)@.
  TTup_   :: T_tup e -> [Term_ e] -> Term_ e

  -- | An injection into a sum type.
  TInj_   :: T_inj e -> Side -> Term_ e -> Term_ e

  -- | A case expression.
  TCase_  :: T_case e -> [Branch] -> Term_ e

  -- | An application of a unary operator.
  TUn_    :: T_un e -> UOp -> Term_ e -> Term_ e

  -- | An application of a binary operator.
  TBin_   :: T_bin e -> BOp -> Term_ e -> Term_ e -> Term_ e

  -- | A chained comparison.  Should contain only comparison
  --   operators.
  TChain_ :: T_chain e -> Term_ e -> [Link] -> Term_ e

  -- | An application of a type operator.
  TTyOp_  :: T_tyop e -> TyOp -> Type -> Term_ e

  -- | A literal list.
  TList_ :: T_list e -> [Term_ e] -> Maybe (Ellipsis (Term_ e)) -> Term_ e

  -- | List comprehension.
  TListComp_ :: T_listcomp e -> Bind (Telescope (Qual_ e)) (Term_ e) -> Term_ e

  -- | Type ascription, @(Term_ e : type)@.
  TAscr_  :: T_ascr e -> Term_ e -> Type -> Term_ e

  Term_   :: T_term e -> Term_ e 
  deriving (Generic)


-- type Forall_t (a :: * -> Constraint) e 
--       = (a (T_var e), a (T_let e),  
--          a (T_parens e), a (T_unit e), 
--          a (T_bool e), a (T_nat e), 
--          a (T_rat e), a (T_abs e), 
--          a (T_app e), a (T_inj e),
--          a (T_case e), a (T_un e),
--          a (T_bin e), a (T_chain e),
--          a (T_tyop e), a (T_list e),
--          a (T_listcomp e), a (T_ascr e),
--          a (T_term e), a (T_tup e))

-- deriving instance Forall_t Show e => Show (Term_ e)


type family T_var e 
type family T_let e 
type family T_parens e 
type family T_unit e 
type family T_bool e 
type family T_nat e 
type family T_rat e 
type family T_abs e 
type family T_app e 
type family T_inj e 
type family T_case e 
type family T_un e 
type family T_bin e 
type family T_chain e 
type family T_tyop e 
type family T_list e 
type family T_listcomp e 
type family T_ascr e
type family T_term e 
type family T_tup e 

  

-- | Terms.
data Term where

  -- | A variable.
  TVar   :: Name Term -> Term

  -- | A (non-recursive) let expression, @let x1 = t1, x2 = t2, ... in t@.
  TLet   :: Bind (Telescope Binding) Term -> Term

  -- | Explicit parentheses.  We need to keep track of these in order
  --   to syntactically distinguish multiplication and function
  --   application.
  TParens :: Term -> Term

  -- | The unit value, (), of type Unit.
  TUnit  :: Term

  -- | True or false.
  TBool  :: Bool -> Term

  -- | A natural number.
  TNat   :: Integer -> Term

  -- | A nonnegative rational number, parsed as a decimal.
  TRat   :: Rational -> Term

  -- | An anonymous function.
  TAbs   :: Bind [(Name Term, Embed (Maybe Type))] Term -> Term

  -- | Function application.
  TApp  :: Term -> Term -> Term

  -- | An n-tuple, @(t1, ..., tn)@.
  TTup   :: [Term] -> Term

  -- | An injection into a sum type.
  TInj   :: Side -> Term -> Term

  -- | A case expression.
  TCase  :: [Branch] -> Term

  -- | An application of a unary operator.
  TUn    :: UOp -> Term -> Term

  -- | An application of a binary operator.
  TBin   :: BOp -> Term -> Term -> Term

  -- | A chained comparison.  Should contain only comparison
  --   operators.
  TChain :: Term -> [Link] -> Term

  -- | An application of a type operator.
  TTyOp  :: TyOp -> Type -> Term

  -- | A literal list.
  TList :: [Term] -> Maybe (Ellipsis Term) -> Term

  -- | List comprehension.
  TListComp :: Bind (Telescope Qual) Term -> Term

  -- | Type ascription, @(term : type)@.
  TAscr  :: Term -> Type -> Term
  deriving (Show, Generic)

data Link_ e where
  TLink_ :: T_link e -> BOp -> Term_ e -> Link_ e
  deriving Generic

deriving instance (Show (T_link e), Show (Term_ e)) => Show (Link_ e)

type family T_link e

data Link where
  TLink :: BOp -> Term -> Link
  deriving (Show, Generic)

-- | An ellipsis is an "omitted" part of a literal list, of the form
--   @..@ or @.. t@.
data Ellipsis t where
  Forever ::      Ellipsis t   -- @..@
  Until   :: t -> Ellipsis t   -- @.. t@
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- Note: very similar to guards-
--  maybe some generalization in the future?

data Qual_ e where

  -- | A binding qualifier (i.e. @x <- t@)
  QBind_   :: Q_bind e -> Name (Term_ e) -> Embed (Term_ e) -> Qual_ e

  -- | A boolean guard qualfier (i.e. @x + y > 4@)
  QGuard_  :: Q_guard e -> Embed (Term_ e) -> Qual_ e

  deriving Generic

deriving instance (Show (Q_bind e), Show (Q_guard e), Show (Term_ e)) => Show (Qual_ e)

type family Q_bind e
type family Q_guard e

-- | A single qualifier in a list comprehension.
data Qual where

  -- | A binding qualifier (i.e. @x <- t@)
  QBind   :: Name Term -> Embed Term -> Qual

  -- | A boolean guard qualfier (i.e. @x + y > 4@)
  QGuard  :: Embed Term -> Qual

  deriving (Show, Generic)

-- | A binding is a name along with its definition.
data Binding = Binding (Maybe Type) (Name Term) (Embed Term)
  deriving (Show, Generic)

-- | A branch of a case is a list of guards with an accompanying term.
--   The guards scope over the term.  Additionally, each guard scopes
--   over subsequent guards.

type Branch_ e = Bind (Telescope (Guard_ e)) (Term_ e)

type Branch = Bind (Telescope Guard) Term

data Guard_ e where

  -- | Boolean guard (@if <test>@)
  GBool_ :: G_bool e -> Embed (Term_ e) -> Guard_ e

  -- | Pattern guard (@when term = pat@)
  GPat_  :: G_pat e -> Embed (Term_ e) -> Pattern -> Guard_ e

  deriving Generic

deriving instance (Show (G_bool e), Show (G_pat e), Show (Term_ e)) => Show (Guard_ e)

type family G_bool e
type family G_pat e

-- | A single guard in a branch: either an @if@ or a @when@.
data Guard where

  -- | Boolean guard (@if <test>@)
  GBool :: Embed Term -> Guard

  -- | Pattern guard (@when term = pat@)
  GPat  :: Embed Term -> Pattern -> Guard

  deriving (Show, Generic)

-- | Patterns.
data Pattern where

  -- | Variable pattern: matches anything and binds the variable.
  PVar  :: Name Term -> Pattern

  -- | Wildcard pattern @_@: matches anything.
  PWild :: Pattern

  -- | Unit pattern @()@: matches @()@.
  PUnit :: Pattern

  -- | Literal boolean pattern.
  PBool :: Bool -> Pattern

  -- | Tuple pattern @(pat1, .. , patn)@.
  PTup  :: [Pattern] -> Pattern

  -- | Injection pattern (@inl pat@ or @inr pat@).
  PInj  :: Side -> Pattern -> Pattern

  -- | Literal natural number pattern.
  PNat  :: Integer -> Pattern

  -- | Successor pattern, @S p@.
  PSucc :: Pattern -> Pattern

  -- | Cons pattern @p1 :: p2@.
  PCons :: Pattern -> Pattern -> Pattern

  -- | List pattern @[p1, .., pn]@.
  PList :: [Pattern] -> Pattern

  deriving (Show, Generic)
  -- TODO: figure out how to match on Z or Q!

instance Alpha Side
instance Alpha Link
instance Alpha Term
instance Alpha Binding
instance Alpha t => Alpha (Ellipsis t)
instance Alpha Guard
instance Alpha Pattern
instance Alpha Qual

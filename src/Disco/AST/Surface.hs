{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
       , UD
       , Term
       , pattern TVar
       , pattern TUn
       , pattern TLet
       , pattern TParens
       , pattern TUnit
       , pattern TBool
       , pattern TChar
       , pattern TString
       , pattern TNat
       , pattern TRat
       , pattern TAbs
       , pattern TApp
       , pattern TTup
       , pattern TInj
       , pattern TCase
       , pattern TBin
       , pattern TChain
       , pattern TTyOp
       , pattern TContainerComp
       , pattern TContainer
       , pattern TAscr
       , pattern TList
       , pattern TListComp

         -- ** Telescopes
       , Telescope(..), foldTelescope, mapTelescope, toTelescope, fromTelescope
         -- ** Expressions
       , Side(..)

       , Link
       , pattern TLink

       , Binding

         -- ** Lists
       , Qual
       , pattern QBind
       , pattern QGuard

       , Container(..)

       , Ellipsis(..)

         -- ** Case expressions and patterns
       , Branch

       , Guard
       , pattern GBool
       , pattern GPat

       , Pattern
       , pattern PVar
       , pattern PWild
       , pattern PUnit
       , pattern PBool
       , pattern PChar
       , pattern PString
       , pattern PTup
       , pattern PInj
       , pattern PNat
       , pattern PSucc
       , pattern PCons
       , pattern PList
       , pattern Binding
       )
       where

import           Disco.AST.Generic
import           Disco.Context
import           Disco.Syntax.Operators
import           Disco.Types
import           Unbound.Generics.LocallyNameless

-- | The extension descriptor for Surface specific AST types.

data UD

-- | A module is a list of declarations together with a collection of
--   documentation for top-level names.
data Module = Module [Decl] (Ctx Term Docs)
deriving instance ForallTerm Show  UD => Show Module

-- | A @TopLevel@ is either documentation (a 'DocThing') or a
--   declaration ('Decl').
data TopLevel = TLDoc DocThing | TLDecl Decl
deriving instance ForallTerm Show  UD => Show TopLevel

-- | Convenient synonym for a list of 'DocThing's.
type Docs = [DocThing]

-- | An item of documentation.
data DocThing
  = DocString   [String]    -- ^ A documentation string, i.e. a block
                            --   of @||| text@ items
  | DocProperty Property    -- ^ An example/doctest/property of the
                            --   form @!!! forall (x1:ty1) ... . property@
deriving instance ForallTerm Show  UD => Show DocThing

-- | A property is a universally quantified term of the form
--   @forall v1 : T1, v2 : T2. term@.
type Property = Property_ UD

-- | A declaration is either a type declaration or a definition.
data Decl where

  -- | A type declaration, @name : type@.
  DType :: Name Term -> Sigma -> Decl

  -- | A group of definition clauses of the form @name pat1 .. patn = term@. The
  --   patterns bind variables in the term. For example, @f n (x,y) =
  --   n*x + y@.
  DDefn :: Name Term -> [Bind [Pattern] Term] -> Decl

deriving instance ForallTerm Show  UD => Show Decl

-- | Get the name that a declaration is about.
declName :: Decl -> Name Term
declName (DType x _) = x
declName (DDefn x _) = x

-- | Check whether a declaration is a definition.
isDefn :: Decl -> Bool
isDefn DDefn{} = True
isDefn _       = False

------------------------------------------------------------
-- Terms
------------------------------------------------------------
type Term = Term_ UD

type instance X_TVar            UD = ()
type instance X_TLet            UD = ()
type instance X_TParens         UD = ()
type instance X_TUnit           UD = ()
type instance X_TBool           UD = ()
type instance X_TNat            UD = ()
type instance X_TRat            UD = ()
type instance X_TChar           UD = ()
type instance X_TString         UD = ()
type instance X_TAbs            UD = ()
type instance X_TApp            UD = ()
type instance X_TInj            UD = ()
type instance X_TCase           UD = ()
type instance X_TUn             UD = ()
type instance X_TBin            UD = ()
type instance X_TChain          UD = ()
type instance X_TTyOp           UD = ()
type instance X_TContainer      UD = ()
type instance X_TContainerComp  UD = ()
type instance X_TAscr           UD = ()
type instance X_Term            UD = ()
type instance X_TTup            UD = ()

pattern TVar :: Name Term -> Term
pattern TVar name = TVar_ () name

pattern TUn :: UOp -> Term -> Term
pattern TUn uop term = TUn_ () uop term

pattern TLet :: Bind (Telescope Binding) Term -> Term
pattern TLet bind = TLet_ () bind

pattern TParens :: Term -> Term
pattern TParens term  = TParens_ () term

pattern TUnit :: Term
pattern TUnit = TUnit_ ()

pattern TBool :: Bool -> Term
pattern TBool bool = TBool_ () bool

pattern TNat  :: Integer -> Term
pattern TNat int = TNat_ () int

pattern TRat :: Rational -> Term
pattern TRat rat = TRat_ () rat

pattern TChar :: Char -> Term
pattern TChar c = TChar_ () c

pattern TString :: String -> Term
pattern TString s = TString_ () s

pattern TAbs :: Bind [(Name Term, Embed (Maybe Type))] Term -> Term
pattern TAbs bind = TAbs_ () bind

pattern TApp  :: Term -> Term -> Term
pattern TApp term1 term2 = TApp_ () term1 term2

pattern TTup :: [Term] -> Term
pattern TTup termlist = TTup_ () termlist

pattern TInj :: Side -> Term -> Term
pattern TInj side term = TInj_ () side term

pattern TCase :: [Branch] -> Term
pattern TCase branch = TCase_ () branch

pattern TBin :: BOp -> Term -> Term -> Term
pattern TBin bop term1 term2 = TBin_ () bop term1 term2

pattern TChain :: Term -> [Link] -> Term
pattern TChain term linklist = TChain_ () term linklist

pattern TTyOp :: TyOp -> Type -> Term
pattern TTyOp tyop ty = TTyOp_ () tyop ty

pattern TContainer :: Container -> [Term] -> Maybe (Ellipsis Term) -> Term
pattern TContainer c tl mets = TContainer_ () c tl mets

pattern TContainerComp :: Container -> Bind (Telescope Qual) Term -> Term
pattern TContainerComp c b = TContainerComp_ () c b

pattern TAscr :: Term -> Sigma -> Term
pattern TAscr term ty = TAscr_ () term ty

{-# COMPLETE TVar, TUn, TLet, TParens, TUnit, TBool, TNat, TRat, TChar,
             TString, TAbs, TApp, TTup, TInj, TCase, TBin, TChain, TTyOp,
             TContainer, TContainerComp, TAscr #-}

pattern TList :: [Term] -> Maybe (Ellipsis Term) -> Term
pattern TList ts e = TContainer_ () ListContainer ts e

pattern TListComp :: Bind (Telescope Qual) Term -> Term
pattern TListComp x = TContainerComp_ () ListContainer x

type Link = Link_ UD

type instance X_TLink UD = ()

pattern TLink :: BOp -> Term -> Link
pattern TLink bop term = TLink_ () bop term

{-# COMPLETE TLink #-}

type Qual = Qual_ UD

type instance X_QBind UD = ()
type instance X_QGuard UD = ()

pattern QBind :: Name Term -> Embed Term -> Qual
pattern QBind namet embedt = QBind_ () namet embedt

pattern QGuard :: Embed Term -> Qual
pattern QGuard embedt = QGuard_ () embedt

{-# COMPLETE QBind, QGuard #-}

type Binding = Binding_ UD

pattern Binding :: Maybe (Embed Sigma) -> Name Term -> Embed Term -> Binding
pattern Binding m b n = Binding_ m b n

{-# COMPLETE Binding #-}

type Branch = Branch_ UD

type Guard = Guard_ UD

type instance X_GBool UD = ()
type instance X_GPat UD = ()

pattern GBool :: Embed Term -> Guard
pattern GBool embedt = GBool_ () embedt

pattern GPat :: Embed Term -> Pattern -> Guard
pattern GPat embedt pat = GPat_ () embedt pat

{-# COMPLETE GBool, GPat #-}

type Pattern = Pattern_ UD

type instance X_PVar UD    = ()
type instance X_PWild UD   = ()
type instance X_PUnit UD   = ()
type instance X_PBool UD   = ()
type instance X_PTup UD    = ()
type instance X_PInj UD    = ()
type instance X_PNat UD    = ()
type instance X_PChar UD   = ()
type instance X_PString UD = ()
type instance X_PSucc UD   = ()
type instance X_PCons UD   = ()
type instance X_PList UD   = ()
type instance X_Pattern UD = ()

pattern PVar :: Name Term -> Pattern
pattern PVar name = PVar_ () name

pattern PWild :: Pattern
pattern PWild = PWild_ ()

pattern PUnit :: Pattern
pattern PUnit = PUnit_ ()

pattern PBool :: Bool -> Pattern
pattern PBool  b = PBool_ () b

pattern PChar :: Char -> Pattern
pattern PChar c = PChar_ () c

pattern PString :: String -> Pattern
pattern PString s = PString_ () s

pattern PTup  :: [Pattern] -> Pattern
pattern PTup lp = PTup_ () lp

pattern PInj  :: Side -> Pattern -> Pattern
pattern PInj s p = PInj_ () s p

pattern PNat  :: Integer -> Pattern
pattern PNat n = PNat_ () n

pattern PSucc :: Pattern -> Pattern
pattern PSucc p = PSucc_ () p

pattern PCons :: Pattern -> Pattern -> Pattern
pattern PCons  p1 p2 = PCons_ () p1 p2

pattern PList :: [Pattern] -> Pattern
pattern PList lp = PList_ () lp

{-# COMPLETE PVar, PWild, PUnit, PBool, PTup, PInj, PNat,
             PChar, PSucc, PCons, PList #-}

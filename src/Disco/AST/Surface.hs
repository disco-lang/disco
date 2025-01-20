{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Disco.AST.Surface
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax trees representing the surface syntax of the Disco
-- language.
module Disco.AST.Surface (
  -- * Modules
  Module (..),
  emptyModule,
  TopLevel (..),

  -- ** Documentation
  Docs,
  DocThing (..),
  Property,

  -- ** Declarations
  TypeDecl (..),
  TermDefn (..),
  TypeDefn (..),
  Decl (..),
  partitionDecls,
  prettyTyDecl,

  -- * Terms
  UD,
  Term,
  pattern TVar,
  pattern TPrim,
  pattern TUn,
  pattern TBin,
  pattern TLet,
  pattern TParens,
  pattern TUnit,
  pattern TBool,
  pattern TChar,
  pattern TString,
  pattern TNat,
  pattern TRat,
  pattern TAbs,
  pattern TApp,
  pattern TTup,
  pattern TCase,
  pattern TChain,
  pattern TTyOp,
  pattern TContainerComp,
  pattern TContainer,
  pattern TAscr,
  pattern TWild,
  pattern TList,
  pattern TListComp,
  Quantifier (..),

  -- ** Telescopes
  Telescope (..),
  foldTelescope,
  mapTelescope,
  toTelescope,
  fromTelescope,

  -- ** Expressions
  Side (..),
  Link,
  pattern TLink,
  Binding,

  -- ** Lists
  Qual,
  pattern QBind,
  pattern QGuard,
  Container (..),
  Ellipsis (..),

  -- ** Case expressions and patterns
  Branch,
  Guard,
  pattern GBool,
  pattern GPat,
  pattern GLet,
  Pattern,
  pattern PVar,
  pattern PWild,
  pattern PAscr,
  pattern PUnit,
  pattern PBool,
  pattern PChar,
  pattern PString,
  pattern PTup,
  pattern PInj,
  pattern PNat,
  pattern PCons,
  pattern PList,
  pattern PAdd,
  pattern PMul,
  pattern PSub,
  pattern PNeg,
  pattern PFrac,
  pattern PNonlinear,
  pattern Binding,

  -- ** Pretty printing
  prettyPatternP,
)
where

import Control.Lens ((%~), _1, _2, _3)
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Disco.AST.Generic
import Disco.Effects.LFresh
import Disco.Extensions
import Disco.Pretty
import Disco.Syntax.Operators
import Disco.Syntax.Prims
import Disco.Types
import Polysemy hiding (Embed, embed)
import Polysemy.Reader
import Unbound.Generics.LocallyNameless hiding (LFresh (..), lunbind)
import Prelude hiding ((<>))

-- | The extension descriptor for Surface specific AST types.
data UD

-- | A module contains all the information from one disco source file.
data Module = Module
  { modExts :: Set Ext
  -- ^ Enabled extensions
  , modImports :: [String]
  -- ^ Module imports
  , modDecls :: [Decl]
  -- ^ Declarations
  , modDocs :: [(Name Term, Docs)]
  -- ^ Documentation
  , modTerms :: [Term]
  -- ^ Top-level (bare) terms
  }

deriving instance ForallTerm Show UD => Show Module

emptyModule :: Module
emptyModule =
  Module
    { modExts = S.empty
    , modImports = []
    , modDecls = []
    , modDocs = []
    , modTerms = []
    }

-- | A @TopLevel@ is either documentation (a 'DocThing') or a
--   declaration ('Decl').
data TopLevel = TLDoc DocThing | TLDecl Decl | TLExpr Term

deriving instance ForallTerm Show UD => Show TopLevel

-- | Convenient synonym for a list of 'DocThing's.
type Docs = [DocThing]

-- | An item of documentation.
data DocThing
  = -- | A documentation string, i.e. a block
    --   of @||| text@ items
    DocString [String]
  | -- | An example/doctest/property of the
    --   form @!!! forall (x1:ty1) ... . property@
    DocProperty Property

deriving instance ForallTerm Show UD => Show DocThing

-- | A property is a universally quantified term of the form
--   @forall v1 : T1, v2 : T2. term@.
type Property = Property_ UD

-- | A type declaration, @name : type@.
data TypeDecl = TypeDecl (Name Term) PolyType

-- | A group of definition clauses of the form @name pat1 .. patn = term@. The
--   patterns bind variables in the term. For example, @f n (x,y) =
--   n*x + y@.
data TermDefn = TermDefn (Name Term) (NonEmpty (Bind [Pattern] Term))

-- | A user-defined type (potentially recursive).
--
--   @type T arg1 arg2 ... = body
data TypeDefn = TypeDefn String [String] Type
  deriving (Show)

-- | A declaration is either a type declaration, a term definition, or
--   a type definition.
data Decl where
  DType :: TypeDecl -> Decl
  DDefn :: TermDefn -> Decl
  DTyDef :: TypeDefn -> Decl

deriving instance ForallTerm Show UD => Show TypeDecl
deriving instance ForallTerm Show UD => Show TermDefn
deriving instance ForallTerm Show UD => Show Decl

partitionDecls :: [Decl] -> ([TypeDecl], [TermDefn], [TypeDefn])
partitionDecls (DType tyDecl : ds) = (_1 %~ (tyDecl :)) (partitionDecls ds)
partitionDecls (DDefn def : ds) = (_2 %~ (def :)) (partitionDecls ds)
partitionDecls (DTyDef def : ds) = (_3 %~ (def :)) (partitionDecls ds)
partitionDecls [] = ([], [], [])

------------------------------------------------------------
-- Pretty-printing top-level declarations

-- prettyModule :: Module -> Doc
-- prettyModule = foldr ($+$) empty . map pretty

instance Pretty Decl where
  pretty = \case
    DType (TypeDecl x ty) -> pretty x <+> text ":" <+> pretty ty
    DTyDef (TypeDefn x args body) ->
      text "type" <+> text x <+> hsep (map text args) <+> text "=" <+> pretty body
    DDefn (TermDefn x bs) -> vcat $ map (pretty . (x,)) (NE.toList bs)

-- | Pretty-print a single clause in a definition.
instance Pretty (Name a, Bind [Pattern] Term) where
  pretty (x, b) = withPA funPA . lunbind b $ \(ps, t) ->
    pretty x <> hcat (map prettyPatternP ps) <+> text "=" <+> setPA initPA (pretty t)

-- | Pretty-print a type declaration.
prettyTyDecl :: Members '[Reader PA, LFresh] r => Name t -> Type -> Sem r (Doc ann)
prettyTyDecl x ty = hsep [pretty x, text ":", pretty ty]

------------------------------------------------------------
-- Terms
------------------------------------------------------------
type Term = Term_ UD

-- In the surface language, abstractions bind variables using a
-- (nonempty) list of patterns. Each pattern might contain any
-- number of variables, and might have type annotations on some
-- of its components.
type instance X_Binder UD = [Pattern]

type instance X_TVar UD = ()
type instance X_TPrim UD = ()
type instance X_TLet UD = ()
type instance X_TParens UD = ()
type instance X_TUnit UD = ()
type instance X_TBool UD = ()
type instance X_TNat UD = ()
type instance X_TRat UD = ()
type instance X_TChar UD = ()
type instance X_TString UD = ()
type instance X_TAbs UD = ()
type instance X_TApp UD = ()
type instance X_TTup UD = ()
type instance X_TCase UD = ()
type instance X_TChain UD = ()
type instance X_TTyOp UD = ()
type instance X_TContainer UD = ()
type instance X_TContainerComp UD = ()
type instance X_TAscr UD = ()
type instance X_Term UD = () -- TWild

pattern TVar :: Name Term -> Term
pattern TVar name = TVar_ () name

pattern TPrim :: Prim -> Term
pattern TPrim name = TPrim_ () name

pattern TUn :: UOp -> Term -> Term
pattern TUn uop term = TApp_ () (TPrim_ () (PrimUOp uop)) term

pattern TBin :: BOp -> Term -> Term -> Term
pattern TBin bop term1 term2 = TApp_ () (TPrim_ () (PrimBOp bop)) (TTup_ () [term1, term2])

pattern TLet :: Bind (Telescope Binding) Term -> Term
pattern TLet bind = TLet_ () bind

pattern TParens :: Term -> Term
pattern TParens term = TParens_ () term

pattern TUnit :: Term
pattern TUnit = TUnit_ ()

pattern TBool :: Bool -> Term
pattern TBool bool = TBool_ () bool

pattern TNat :: Integer -> Term
pattern TNat int = TNat_ () int

pattern TRat :: Rational -> Term
pattern TRat rat = TRat_ () rat

pattern TChar :: Char -> Term
pattern TChar c = TChar_ () c

pattern TString :: String -> Term
pattern TString s = TString_ () s

pattern TAbs :: Quantifier -> Bind [Pattern] Term -> Term
pattern TAbs q bind = TAbs_ q () bind

pattern TApp :: Term -> Term -> Term
pattern TApp term1 term2 = TApp_ () term1 term2

pattern TTup :: [Term] -> Term
pattern TTup termlist = TTup_ () termlist

pattern TCase :: [Branch] -> Term
pattern TCase branch = TCase_ () branch

pattern TChain :: Term -> [Link] -> Term
pattern TChain term linklist = TChain_ () term linklist

pattern TTyOp :: TyOp -> Type -> Term
pattern TTyOp tyop ty = TTyOp_ () tyop ty

pattern TContainer :: Container -> [(Term, Maybe Term)] -> Maybe (Ellipsis Term) -> Term
pattern TContainer c tl mets = TContainer_ () c tl mets

pattern TContainerComp :: Container -> Bind (Telescope Qual) Term -> Term
pattern TContainerComp c b = TContainerComp_ () c b

pattern TAscr :: Term -> PolyType -> Term
pattern TAscr term ty = TAscr_ () term ty

-- Since we parse patterns by first parsing a term and then ensuring
-- it is a valid pattern, we have to include wildcards in the syntax
-- of terms, although they will be rejected at a later phase.
pattern TWild :: Term
pattern TWild = XTerm_ ()

{-# COMPLETE
  TVar
  , TPrim
  , TLet
  , TParens
  , TUnit
  , TBool
  , TNat
  , TRat
  , TChar
  , TString
  , TAbs
  , TApp
  , TTup
  , TCase
  , TChain
  , TTyOp
  , TContainer
  , TContainerComp
  , TAscr
  , TWild
  #-}

pattern TList :: [Term] -> Maybe (Ellipsis Term) -> Term
pattern TList ts e <- TContainer_ () ListContainer (map fst -> ts) e
  where
    TList ts e = TContainer_ () ListContainer (map (,Nothing) ts) e

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

pattern Binding :: Maybe (Embed PolyType) -> Name Term -> Embed Term -> Binding
pattern Binding m b n = Binding_ m b n

{-# COMPLETE Binding #-}

type Branch = Branch_ UD

type Guard = Guard_ UD

type instance X_GBool UD = ()
type instance X_GPat UD = ()
type instance X_GLet UD = ()

pattern GBool :: Embed Term -> Guard
pattern GBool embedt = GBool_ () embedt

pattern GPat :: Embed Term -> Pattern -> Guard
pattern GPat embedt pat = GPat_ () embedt pat

pattern GLet :: Binding -> Guard
pattern GLet b = GLet_ () b

{-# COMPLETE GBool, GPat, GLet #-}

type Pattern = Pattern_ UD

type instance X_PVar UD = ()
type instance X_PWild UD = ()
type instance X_PAscr UD = ()
type instance X_PUnit UD = ()
type instance X_PBool UD = ()
type instance X_PTup UD = ()
type instance X_PInj UD = ()
type instance X_PNat UD = ()
type instance X_PChar UD = ()
type instance X_PString UD = ()
type instance X_PCons UD = ()
type instance X_PList UD = ()
type instance X_PAdd UD = ()
type instance X_PMul UD = ()
type instance X_PSub UD = ()
type instance X_PNeg UD = ()
type instance X_PFrac UD = ()
type instance X_Pattern UD = Void

pattern PVar :: Name Term -> Pattern
pattern PVar name = PVar_ () name

pattern PWild :: Pattern
pattern PWild = PWild_ ()

-- (?) TAscr uses a PolyType, but without higher rank types
-- I think we can't possibly need that here.
pattern PAscr :: Pattern -> Type -> Pattern
pattern PAscr p ty = PAscr_ () p ty

pattern PUnit :: Pattern
pattern PUnit = PUnit_ ()

pattern PBool :: Bool -> Pattern
pattern PBool b = PBool_ () b

pattern PChar :: Char -> Pattern
pattern PChar c = PChar_ () c

pattern PString :: String -> Pattern
pattern PString s = PString_ () s

pattern PTup :: [Pattern] -> Pattern
pattern PTup lp = PTup_ () lp

pattern PInj :: Side -> Pattern -> Pattern
pattern PInj s p = PInj_ () s p

pattern PNat :: Integer -> Pattern
pattern PNat n = PNat_ () n

pattern PCons :: Pattern -> Pattern -> Pattern
pattern PCons p1 p2 = PCons_ () p1 p2

pattern PList :: [Pattern] -> Pattern
pattern PList lp = PList_ () lp

pattern PAdd :: Side -> Pattern -> Term -> Pattern
pattern PAdd s p t = PAdd_ () s p t

pattern PMul :: Side -> Pattern -> Term -> Pattern
pattern PMul s p t = PMul_ () s p t

pattern PSub :: Pattern -> Term -> Pattern
pattern PSub p t = PSub_ () p t

pattern PNeg :: Pattern -> Pattern
pattern PNeg p = PNeg_ () p

pattern PFrac :: Pattern -> Pattern -> Pattern
pattern PFrac p1 p2 = PFrac_ () p1 p2

pattern PNonlinear :: Pattern -> Name Term -> Pattern
pattern PNonlinear p x <- PNonlinear_ (unembed -> p) x
  where
    PNonlinear p x = PNonlinear_ (embed p) x

{-# COMPLETE
  PVar
  , PWild
  , PAscr
  , PUnit
  , PBool
  , PTup
  , PInj
  , PNat
  , PChar
  , PString
  , PCons
  , PList
  , PAdd
  , PMul
  , PSub
  , PNeg
  , PFrac
  #-}

------------------------------------------------------------
-- Pretty-printing for surface-syntax terms
--
-- The instances in this section are used to pretty-print surface
-- syntax, for example, when printing the source code definition of a
-- term (e.g. via the :doc REPL command).

-- | Pretty-print a term with guaranteed parentheses.
prettyTermP :: Members '[LFresh, Reader PA] r => Term -> Sem r (Doc ann)
prettyTermP t@TTup {} = setPA initPA $ pretty t
-- prettyTermP t@TContainer{} = setPA initPA $ "" <+> prettyTerm t
prettyTermP t = withPA initPA $ pretty t

instance Pretty Term where
  pretty = \case
    TVar x -> pretty x
    TPrim (PrimUOp uop) ->
      case M.lookup uop uopMap of
        Just (OpInfo (UOpF Pre _) (syn : _) _) -> text syn <> text "~"
        Just (OpInfo (UOpF Post _) (syn : _) _) -> text "~" <> text syn
        _ -> error $ "pretty @Term: " ++ show uop ++ " is not in the uopMap!"
    TPrim (PrimBOp bop) -> text "~" <> pretty bop <> text "~"
    TPrim p ->
      case M.lookup p primMap of
        Just (PrimInfo _ nm True) -> text nm
        Just (PrimInfo _ nm False) -> text "$" <> text nm
        Nothing -> error $ "pretty @Term: Prim " ++ show p ++ " is not in the primMap!"
    TParens t -> pretty t
    TUnit -> text "■"
    (TBool b) -> text (take 1 $ show b)
    TChar c -> text (show c)
    TString cs -> doubleQuotes $ text cs
    TAbs q bnd -> withPA initPA $
      lunbind bnd $ \(args, body) ->
        prettyQ q
          <> (hsep =<< punctuate (text ",") (map pretty args))
          <> text "."
          <+> lt (pretty body)
     where
      prettyQ Lam = text "λ"
      prettyQ All = text "∀"
      prettyQ Ex = text "∃"

    -- special case for fully applied unary operators
    TApp (TPrim (PrimUOp uop)) t ->
      case M.lookup uop uopMap of
        Just (OpInfo (UOpF Post _) _ _) ->
          withPA (ugetPA uop) $
            lt (pretty t) <> pretty uop
        Just (OpInfo (UOpF Pre _) _ _) ->
          withPA (ugetPA uop) $
            pretty uop <> rt (pretty t)
        _ -> error $ "pretty @Term: uopMap doesn't contain " ++ show uop
    -- special case for fully applied binary operators
    TApp (TPrim (PrimBOp bop)) (TTup [t1, t2]) ->
      withPA (getPA bop) $
        hsep
          [ lt (pretty t1)
          , pretty bop
          , rt (pretty t2)
          ]
    -- Always pretty-print function applications with parentheses
    TApp t1 t2 ->
      withPA funPA $
        lt (pretty t1) <> prettyTermP t2
    TTup ts -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ts)
      parens (hsep ds)
    TContainer c ts e -> setPA initPA $ do
      ds <- punctuate (text ",") (map prettyCount ts)
      let pe = case e of
            Nothing -> []
            Just (Until t) -> [text "..", pretty t]
      containerDelims c (hsep (ds ++ pe))
     where
      prettyCount (t, Nothing) = pretty t
      prettyCount (t, Just n) = lt (pretty t) <+> text "#" <+> rt (pretty n)
    TContainerComp c bqst ->
      lunbind bqst $ \(qs, t) ->
        setPA initPA $ containerDelims c (hsep [pretty t, text "|", pretty qs])
    TNat n -> integer n
    TChain t lks ->
      withPA (getPA Eq) . hsep $
        lt (pretty t)
          : concatMap prettyLink lks
     where
      prettyLink (TLink op t2) =
        [ pretty op
        , setPA (getPA op) . rt $ pretty t2
        ]
    TLet bnd -> withPA initPA $
      lunbind bnd $ \(bs, t2) -> do
        ds <- punctuate (text ",") (map pretty (fromTelescope bs))
        hsep
          [ text "let"
          , hsep ds
          , text "in"
          , pretty t2
          ]
    TCase b ->
      withPA initPA $
        (text "{?" <+> prettyBranches b) $+$ text "?}"
    TAscr t ty ->
      withPA ascrPA $
        lt (pretty t) <+> text ":" <+> rt (pretty ty)
    TRat r -> text (prettyDecimal r)
    TTyOp op ty ->
      withPA funPA $
        pretty op <+> pretty ty
    TWild -> text "_"

-- | Print appropriate delimiters for a container literal.
containerDelims :: Member (Reader PA) r => Container -> (Sem r (Doc ann) -> Sem r (Doc ann))
containerDelims ListContainer = brackets
containerDelims BagContainer = bag
containerDelims SetContainer = braces

prettyBranches :: Members '[Reader PA, LFresh] r => [Branch] -> Sem r (Doc ann)
prettyBranches = \case
  [] -> text ""
  b : bs ->
    pretty b
      $+$ foldr (($+$) . (text "," <+>) . pretty) empty bs

-- | Pretty-print a single branch in a case expression.
instance Pretty Branch where
  pretty br = lunbind br $ \(gs, t) ->
    pretty t <+> pretty gs

-- | Pretty-print the guards in a single branch of a case expression.
instance Pretty (Telescope Guard) where
  pretty = \case
    TelEmpty -> text "otherwise"
    gs -> foldr (\g r -> pretty g <+> r) (text "") (fromTelescope gs)

instance Pretty Guard where
  pretty = \case
    GBool et -> text "if" <+> pretty (unembed et)
    GPat et p -> text "when" <+> pretty (unembed et) <+> text "is" <+> pretty p
    GLet b -> text "let" <+> pretty b

-- | Pretty-print a binding, i.e. a pairing of a name (with optional
--   type annotation) and term.
instance Pretty Binding where
  pretty = \case
    Binding Nothing x (unembed -> t) ->
      hsep [pretty x, text "=", pretty t]
    Binding (Just (unembed -> ty)) x (unembed -> t) ->
      hsep [pretty x, text ":", pretty ty, text "=", pretty t]

-- | Pretty-print the qualifiers in a comprehension.
instance Pretty (Telescope Qual) where
  pretty (fromTelescope -> qs) = do
    ds <- punctuate (text ",") (map pretty qs)
    hsep ds

-- | Pretty-print a single qualifier in a comprehension.
instance Pretty Qual where
  pretty = \case
    QBind x (unembed -> t) -> hsep [pretty x, text "in", pretty t]
    QGuard (unembed -> t) -> pretty t

-- | Pretty-print a pattern with guaranteed parentheses.
prettyPatternP :: Members '[LFresh, Reader PA] r => Pattern -> Sem r (Doc ann)
prettyPatternP p@PTup {} = setPA initPA $ pretty p
prettyPatternP p = withPA initPA $ pretty p

-- We could probably alternatively write a function to turn a pattern
-- back into a term, and pretty-print that instead of the below.
-- Unsure whether it would be worth it.

instance Pretty Pattern where
  pretty = \case
    PVar x -> pretty x
    PWild -> text "_"
    PAscr p ty ->
      withPA ascrPA $
        lt (pretty p) <+> text ":" <+> rt (pretty ty)
    PUnit -> text "■"
    PBool b -> text $ map toLower $ show b
    PChar c -> text (show c)
    PString s -> text (show s)
    PTup ts -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ts)
      parens (hsep ds)
    PInj s p ->
      withPA funPA $
        pretty s <> prettyPatternP p
    PNat n -> integer n
    PCons p1 p2 ->
      withPA (getPA Cons) $
        lt (pretty p1) <+> text "::" <+> rt (pretty p2)
    PList ps -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ps)
      brackets (hsep ds)
    PAdd L p t ->
      withPA (getPA Add) $
        lt (pretty p) <+> text "+" <+> rt (pretty t)
    PAdd R p t ->
      withPA (getPA Add) $
        lt (pretty t) <+> text "+" <+> rt (pretty p)
    PMul L p t ->
      withPA (getPA Mul) $
        lt (pretty p) <+> text "*" <+> rt (pretty t)
    PMul R p t ->
      withPA (getPA Mul) $
        lt (pretty t) <+> text "*" <+> rt (pretty p)
    PSub p t ->
      withPA (getPA Sub) $
        lt (pretty p) <+> text "-" <+> rt (pretty t)
    PNeg p ->
      withPA (ugetPA Neg) $
        text "-" <> rt (pretty p)
    PFrac p1 p2 ->
      withPA (getPA Div) $
        lt (pretty p1) <+> text "/" <+> rt (pretty p2)
    PNonlinear p _ -> pretty p

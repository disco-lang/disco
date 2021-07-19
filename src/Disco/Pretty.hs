{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Pretty
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Various pretty-printing facilities for disco.
--
-----------------------------------------------------------------------------

-- TODO: the calls to 'error' should be replaced with logging/error capabilities.

module Disco.Pretty
  ( module Disco.Pretty.DSL
  , module Disco.Pretty
  )
  where

import           Prelude                          hiding ((<>))

import           Control.Lens                     (view)
import           Control.Monad                    ((>=>))
import           Data.Bifunctor
import           Data.Char                        (chr, isAlpha, toLower)
import qualified Data.Map                         as M
import           Data.Ratio

import           Disco.Effects.LFresh
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Reader

import           Text.PrettyPrint                 (Doc)
import           Unbound.Generics.LocallyNameless (Bind, Name, string2Name,
                                                   unembed)

import           Disco.AST.Core
import           Disco.AST.Generic                (selectSide)
import           Disco.AST.Surface
import           Disco.Eval                       (TopInfo, topTyDefs)
import           Disco.Interpret.Core             (mapToSet, rnfV, whnfList,
                                                   whnfV)
import           Disco.Module
import           Disco.Pretty.DSL
import           Disco.Pretty.Prec
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Typecheck.Erase            (eraseClause)
import           Disco.Types
import           Disco.Value

------------------------------------------------------------
-- Utilities for handling precedence and associativity

-- | Convenience function combining 'setPA' and 'mparens', since we
--   often want to simultaneously indicate what the precedence and
--   associativity of a term is, and optionally surround it with
--   parentheses depending on the precedence and associativity of its
--   parent.
withPA :: Member (Reader PA) r => PA -> Sem r Doc -> Sem r Doc
withPA pa = mparens pa . setPA pa

-- | Locally set the precedence and associativity within a
--   subcomputation.
setPA :: Member (Reader PA) r => PA -> Sem r a -> Sem r a
setPA = local . const

-- | Mark a subcomputation as pretty-printing a term on the left of an
--   operator (so parentheses can be inserted appropriately, depending
--   on the associativity).
lt :: Member (Reader PA) r => Sem r Doc -> Sem r Doc
lt = local (\(PA p _) -> PA p InL)

-- | Mark a subcomputation as pretty-printing a term on the right of
--   an operator (so parentheses can be inserted appropriately,
--   depending on the associativity).
rt :: Member (Reader PA) r => Sem r Doc -> Sem r Doc
rt = local (\(PA p _) -> PA p InR)

-- | Optionally surround a pretty-printed term with parentheses,
--   depending on its precedence and associativity (given as the 'PA'
--   argument) and that of its context (given by the ambient 'Reader
--   PA' effect).
mparens :: Member (Reader PA) r => PA -> Sem r Doc -> Sem r Doc
mparens pa doc = do
  parentPA <- ask
  (if pa < parentPA then parens else id) doc

------------------------------------------------------------
-- Pretty-printing for types

-- | Pretty-print a type.
prettyTy :: Member (Reader PA) r => Type -> Sem r Doc
prettyTy (TyAtom a)      = prettyTyAtom a
prettyTy (ty1 :->: ty2)   = withPA tarrPA $
  lt (prettyTy ty1) <+> text "→" <+> rt (prettyTy ty2)
prettyTy (ty1 :*: ty2)    = withPA tmulPA $
  lt (prettyTy ty1) <+> text "×" <+> rt (prettyTy ty2)
prettyTy (ty1 :+: ty2)    = withPA taddPA $
  lt (prettyTy ty1) <+> text "+" <+> rt (prettyTy ty2)
prettyTy (TyCon c [])  = prettyTyCon c
prettyTy (TyCon c tys) = do
  ds <- setPA initPA $ punctuate (text ",") (map prettyTy tys)
  prettyTyCon c <> parens (hsep ds)

-- | Pretty-print an atomic type, i.e. a type variable or base type.
prettyTyAtom :: Member (Reader PA) r => Atom -> Sem r Doc
prettyTyAtom (AVar (U v)) = prettyName v
prettyTyAtom (AVar (S v)) = text "$" <> prettyName v
prettyTyAtom (ABase b)    = prettyBaseTy b

-- | Pretty-print a base type.
prettyBaseTy :: BaseTy -> Sem r Doc
prettyBaseTy Void = text "Void"
prettyBaseTy Unit = text "Unit"
prettyBaseTy B    = text "Bool"
prettyBaseTy P    = text "Prop"
prettyBaseTy N    = text "ℕ"
prettyBaseTy Z    = text "ℤ"
prettyBaseTy Q    = text "ℚ"
prettyBaseTy F    = text "𝔽"
prettyBaseTy C    = text "Char"
prettyBaseTy b    = error $ "Impossible: got " ++ show b ++ " in prettyBaseTy"

-- | Pretty-print a type constructor.
prettyTyCon :: Con -> Sem r Doc
prettyTyCon CMap      = text "Map"
prettyTyCon CGraph    = text "Graph"
prettyTyCon (CUser s) = text s
prettyTyCon CList     = text "List"
prettyTyCon CBag      = text "Bag"
prettyTyCon CSet      = text "Set"
prettyTyCon c         = error $ "Impossible: got Con " ++ show c ++ " in prettyTyCon"

-- | Pretty-print a polytype.  Note that we never explicitly print
--   @forall@; quantification is implicit, as in Haskell.
prettyPolyTy :: Members '[LFresh, Reader PA] r => PolyType -> Sem r Doc
prettyPolyTy (Forall bnd) = lunbind bnd $
  \(_, body) -> prettyTy body

-- | Pretty-print a type definition.
prettyTyDef :: Member (Reader PA) r => String -> TyDefBody -> Sem r Doc
prettyTyDef tyName (TyDefBody ps body)
  = text tyName <> prettyArgs ps <+> text "=" <+> prettyTy (body (map (TyVar . string2Name) ps))
  where
    prettyArgs [] = empty
    prettyArgs _  = do
        ds <- punctuate (text ",") (map text ps)
        parens (hsep ds)

------------------------------------------------------------
-- Pretty-printing for surface-syntax terms
--
-- The functions in this section are used to pretty-print surface
-- syntax, for example, when printing the source code definition of a
-- term (e.g. via the :doc REPL command).

-- | Pretty-print a variable name.
prettyName :: Member (Reader PA) r => Name a -> Sem r Doc
prettyName = text . show

-- | Pretty-print a term with guaranteed parentheses.
prettyTermP :: Members '[LFresh, Reader PA] r => Term -> Sem r Doc
prettyTermP t@TTup{} = setPA initPA $ prettyTerm t
prettyTermP t        = withPA initPA $ prettyTerm t

-- | Pretty-print a term.
prettyTerm :: Members '[LFresh, Reader PA] r => Term -> Sem r Doc
prettyTerm (TVar x)      = prettyName x
prettyTerm (TPrim (PrimUOp uop)) = case M.lookup uop uopMap of
  Just (OpInfo (UOpF Pre _) (syn:_) _)  -> text syn <> text "~"
  Just (OpInfo (UOpF Post _) (syn:_) _) -> text "~" <> text syn
  _ -> error $ "prettyTerm: " ++ show uop ++ " is not in the uopMap!"
prettyTerm (TPrim (PrimBOp bop)) = text "~" <> prettyBOp bop <> text "~"
prettyTerm (TPrim p)     =
  case M.lookup p primMap of
    Just (PrimInfo _ nm True)  -> text nm
    Just (PrimInfo _ nm False) -> text "$" <> text nm
    Nothing -> error $ "prettyTerm: Prim " ++ show p ++ " is not in the primMap!"
prettyTerm (TParens t)   = prettyTerm t
prettyTerm TUnit         = text "■"
prettyTerm (TBool b)     = text (map toLower $ show b)
prettyTerm (TChar c)     = text (show c)
prettyTerm (TString cs)  = doubleQuotes $ text cs
prettyTerm (TAbs q bnd)  = withPA initPA $
  lunbind bnd $ \(args, body) ->
  prettyQ q
    <> (hsep =<< punctuate (text ",") (map prettyPattern args))
    <> text "."
    <+> lt (prettyTerm body)
  where
    prettyQ Lam = text "λ"
    prettyQ All = text "∀"
    prettyQ Ex  = text "∃"

-- special case for fully applied unary operators
prettyTerm (TApp (TPrim (PrimUOp uop)) t) =
  case M.lookup uop uopMap of
    Just (OpInfo (UOpF Post _) _ _) -> withPA (ugetPA uop) $
      lt (prettyTerm t) <> prettyUOp uop
    Just (OpInfo (UOpF Pre  _) _ _) -> withPA (ugetPA uop) $
      prettyUOp uop <> rt (prettyTerm t)
    _ -> error $ "prettyTerm: uopMap doesn't contain " ++ show uop

-- special case for fully applied binary operators
prettyTerm (TApp (TPrim (PrimBOp bop)) (TTup [t1, t2])) = withPA (getPA bop) $
  hsep
  [ lt (prettyTerm t1)
  , prettyBOp bop
  , rt (prettyTerm t2)
  ]

-- Always pretty-print function applications with parentheses
prettyTerm (TApp t1 t2)  = withPA funPA $
  lt (prettyTerm t1) <> prettyTermP t2

prettyTerm (TTup ts)     = setPA initPA $ do
  ds <- punctuate (text ",") (map prettyTerm ts)
  parens (hsep ds)
prettyTerm (TContainer c ts e)  = setPA initPA $ do
  ds <- punctuate (text ",") (map prettyCount ts)
  let pe = case e of
             Nothing        -> []
             Just Forever   -> [text ".."]
             Just (Until t) -> [text "..", prettyTerm t]
  containerDelims c (hsep (ds ++ pe))
  where
    prettyCount (t, Nothing) = prettyTerm t
    prettyCount (t, Just n)  = lt (prettyTerm t) <+> text "#" <+> rt (prettyTerm n)
prettyTerm (TContainerComp c bqst) =
  lunbind bqst $ \(qs,t) ->
  setPA initPA $ containerDelims c (hsep [prettyTerm t, text "|", prettyQuals qs])
prettyTerm (TNat n)       = integer n
prettyTerm (TChain t lks) = withPA (getPA Eq) . hsep $
    lt (prettyTerm t)
    : concatMap prettyLink lks
  where
    prettyLink (TLink op t2) =
      [ prettyBOp op
      , setPA (getPA op) . rt $ prettyTerm t2
      ]
prettyTerm (TLet bnd) = withPA initPA $
  lunbind bnd $ \(bs, t2) -> do
    ds <- punctuate (text ",") (map prettyBinding (fromTelescope bs))
    hsep
      [ text "let"
      , hsep ds
      , text "in"
      , prettyTerm t2
      ]

prettyTerm (TCase b)    = withPA initPA $
  (text "{?" <+> prettyBranches b) $+$ text "?}"
prettyTerm (TAscr t ty) = withPA ascrPA $
  lt (prettyTerm t) <+> text ":" <+> rt (prettyPolyTy ty)
prettyTerm (TRat  r)    = text (prettyDecimal r)
prettyTerm (TTyOp op ty)  = withPA funPA $
  prettyTyOp op <+> prettyTy ty
prettyTerm TWild = text "_"

-- | Pretty-print a side, i.e. @left@ or @right@ injection.
prettySide :: Member (Reader PA) r => Side -> Sem r Doc
prettySide L = text "left"
prettySide R = text "right"

-- | Print appropriate delimiters for a container literal.
containerDelims :: Member (Reader PA) r => Container -> (Sem r Doc -> Sem r Doc)
containerDelims ListContainer = brackets
containerDelims BagContainer  = bag
containerDelims SetContainer  = braces

prettyTyOp :: Member (Reader PA) r => TyOp -> Sem r Doc
prettyTyOp Enumerate = text "enumerate"
prettyTyOp Count     = text "count"

-- | Pretty-print a unary operator, by looking up its concrete syntax
--   in the 'uopMap'.
prettyUOp :: Member (Reader PA) r => UOp -> Sem r Doc
prettyUOp op =
  case M.lookup op uopMap of
    Just (OpInfo _ (syn:_) _) ->
      text $ syn ++ (if all isAlpha syn then " " else "")
    _ -> error $ "UOp " ++ show op ++ " not in uopMap!"

-- | Pretty-print a binary operator, by looking up its concrete syntax
--   in the 'bopMap'.
prettyBOp :: Member (Reader PA) r => BOp -> Sem r Doc
prettyBOp op =
  case M.lookup op bopMap of
    Just (OpInfo _ (syn:_) _) -> text syn
    _                         -> error $ "BOp " ++ show op ++ " not in bopMap!"

-- | Pretty-print the branches of a case expression.
prettyBranches :: Members '[LFresh, Reader PA] r => [Branch] -> Sem r Doc
prettyBranches []     = error "Empty branches are disallowed."
prettyBranches (b:bs) =
  prettyBranch False b
  $+$
  foldr (($+$) . prettyBranch True) empty bs

-- | Pretty-print a single branch in a case expression.
prettyBranch :: Members '[LFresh, Reader PA] r => Bool -> Branch -> Sem r Doc
prettyBranch com br = lunbind br $ \(gs,t) ->
  (if com then (text "," <+>) else id) (prettyTerm t <+> prettyGuards gs)

-- | Pretty-print the guards in a single branch of a case expression.
prettyGuards :: Members '[LFresh, Reader PA] r => Telescope Guard -> Sem r Doc
prettyGuards TelEmpty                     = text "otherwise"
prettyGuards (fromTelescope -> gs)
  = foldr (\g r -> prettyGuard g <+> r) (text "") gs

-- | Pretty-print one guard in a branch of a case expression.
prettyGuard :: Members '[LFresh, Reader PA] r => Guard -> Sem r Doc
prettyGuard (GBool et)  = text "if" <+> prettyTerm (unembed et)
prettyGuard (GPat et p) = text "when" <+> prettyTerm (unembed et) <+> text "is" <+> prettyPattern p
prettyGuard (GLet b)    = text "let" <+> prettyBinding b

-- | Pretty-print a binding, i.e. a pairing of a name (with optional
--   type annotation) and term.
prettyBinding :: Members '[LFresh, Reader PA] r => Binding -> Sem r Doc
prettyBinding (Binding Nothing x (unembed -> t))
  = hsep [prettyName x, text "=", prettyTerm t]
prettyBinding (Binding (Just (unembed -> ty)) x (unembed -> t))
  = hsep [prettyName x, text ":", prettyPolyTy ty, text "=", prettyTerm t]

-- | Pretty-print the qualifiers in a comprehension.
prettyQuals :: Members '[LFresh, Reader PA] r => Telescope Qual -> Sem r Doc
prettyQuals (fromTelescope -> qs) = do
  ds <- punctuate (text ",") (map prettyQual qs)
  hsep ds

-- | Pretty-print a single qualifier in a comprehension.
prettyQual :: Members '[LFresh, Reader PA] r => Qual -> Sem r Doc
prettyQual (QBind x (unembed -> t))
  = hsep [prettyName x, text "in", prettyTerm t]
prettyQual (QGuard (unembed -> t))
  = prettyTerm t

-- | Pretty-print a pattern with guaranteed parentheses.
prettyPatternP :: Members '[LFresh, Reader PA] r => Pattern -> Sem r Doc
prettyPatternP p@PTup{} = setPA initPA $ prettyPattern p
prettyPatternP p        = withPA initPA $ prettyPattern p

-- We could probably alternatively write a function to turn a pattern
-- back into a term, and pretty-print that instead of the below.
-- Unsure whether it would be worth it.

-- | Pretty-print a pattern.
prettyPattern :: Members '[LFresh, Reader PA] r => Pattern -> Sem r Doc
prettyPattern (PVar x)          = prettyName x
prettyPattern PWild             = text "_"
prettyPattern (PAscr p ty)      = withPA ascrPA $
  lt (prettyPattern p) <+> text ":" <+> rt (prettyTy ty)
prettyPattern PUnit             = text "■"
prettyPattern (PBool b)         = text $ map toLower $ show b
prettyPattern (PChar c)         = text (show c)
prettyPattern (PString s)       = text (show s)
prettyPattern (PTup ts)         = setPA initPA $ do
  ds <- punctuate (text ",") (map prettyPattern ts)
  parens (hsep ds)
prettyPattern (PInj s p)        = withPA funPA $
  prettySide s <> prettyPatternP p
prettyPattern (PNat n)          = integer n
prettyPattern (PCons p1 p2)     = withPA (getPA Cons) $
  lt (prettyPattern p1) <+> text "::" <+> rt (prettyPattern p2)
prettyPattern (PList ps)        = setPA initPA $ do
  ds <- punctuate (text ",") (map prettyPattern ps)
  brackets (hsep ds)
prettyPattern (PAdd L p t)      = withPA (getPA Add) $
  lt (prettyPattern p) <+> text "+" <+> rt (prettyTerm t)
prettyPattern (PAdd R p t)      = withPA (getPA Add) $
  lt (prettyTerm t) <+> text "+" <+> rt (prettyPattern p)
prettyPattern (PMul L p t)      = withPA (getPA Mul) $
  lt (prettyPattern p) <+> text "*" <+> rt (prettyTerm t)
prettyPattern (PMul R p t)      = withPA (getPA Mul) $
  lt (prettyTerm t) <+> text "*" <+> rt (prettyPattern p)
prettyPattern (PSub p t)        = withPA (getPA Sub) $
  lt (prettyPattern p) <+> text "-" <+> rt (prettyTerm t)
prettyPattern (PNeg p)          = withPA (ugetPA Neg) $
  text "-" <> rt (prettyPattern p)
prettyPattern (PFrac p1 p2)     = withPA (getPA Div) $
  lt (prettyPattern p1) <+> text "/" <+> rt (prettyPattern p2)

------------------------------------------------------------
-- Pretty-printing top-level declarations

-- prettyModule :: Module -> Doc
-- prettyModule = foldr ($+$) empty . map prettyDecl

-- | Pretty-print a declaration.
prettyDecl :: Members '[LFresh, Reader PA] r => Decl -> Sem r Doc
prettyDecl (DType  (TypeDecl x ty)) = prettyName x <+> text ":" <+> prettyPolyTy ty
prettyDecl (DTyDef (TypeDefn x args body))
  = text "type" <+> text x <+> hsep (map text args) <+> text "=" <+> prettyTy body
prettyDecl (DDefn  (TermDefn x bs)) = vcat $ map (prettyClause x) bs

-- | Pretty-print a definition.
prettyDefn :: Members '[LFresh, Reader PA] r => Defn -> Sem r Doc
prettyDefn (Defn x patTys ty clauses) = vcat $
  prettyTyDecl x (foldr (:->:) ty patTys)
  :
  map (prettyClause x . eraseClause) clauses

-- | Pretty-print a single clause in a definition.
prettyClause :: Members '[LFresh, Reader PA] r => Name a -> Bind [Pattern] Term -> Sem r Doc
prettyClause x b
  = withPA funPA . lunbind b $ \(ps, t) ->
      prettyName x <> hcat (map prettyPatternP ps) <+> text "=" <+> setPA initPA (prettyTerm t)

-- | Pretty-print a property.
prettyProperty :: Members '[LFresh, Reader PA] r => Property -> Sem r Doc
prettyProperty = prettyTerm

-- | Pretty-print a type declaration.
prettyTyDecl :: Member (Reader PA) r => Name t -> Type -> Sem r Doc
prettyTyDecl x ty = hsep [prettyName x, text ":", prettyTy ty]

------------------------------------------------------------
-- Pretty-printing values
------------------------------------------------------------

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.
prettyValue :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyValue ty v = prettyV ty v >> output "\n"

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.  Takes a continuation that specifies how the output
--   should be processed (which will be called many times as the
--   output is produced incrementally).
prettyV :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyV ty = whnfV >=> prettyWHNF ty

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyVP :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyVP ty@(_ :*: _) v = prettyV ty v
prettyVP ty           v = output "(" >> prettyV ty v >> output ")"

-- | Pretty-print a value which is already guaranteed to be in weak
--   head normal form.
prettyWHNF :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyWHNF (TyUser nm args) v = do
  tymap <- inputs (view topTyDefs)
  case M.lookup nm tymap of
    Just (TyDefBody _ body) -> prettyWHNF (body args) v
    Nothing                 -> error "Impossible! TyDef name does not exist in TyMap"
prettyWHNF TyUnit          VUnit        = output "■"
prettyWHNF TyProp          _            = prettyPlaceholder TyProp
prettyWHNF TyBool          (VInj s _)   = output $ map toLower (show (selectSide s False True))
prettyWHNF TyC             (VNum _ c)   = output (show $ chr (fromIntegral (numerator c)))
prettyWHNF (TyList TyC)    v            = prettyString v
prettyWHNF (TyList ty)     v            = prettyList ty v
prettyWHNF ty@(_ :*: _)    v            = output "(" >> prettyTuple ty v >> output ")"
prettyWHNF (ty1 :+: ty2) (VInj s v)
  = case s of
      L -> output "left"  >> prettyVP ty1 v
      R -> output "right" >> prettyVP ty2 v
prettyWHNF _ (VNum d r)
  | denominator r == 1 = output $ show (numerator r)
  | otherwise          = case d of
      Fraction -> output $ show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> output $ prettyDecimal r

prettyWHNF ty@(_ :->: _) _ = prettyPlaceholder ty

prettyWHNF (TySet t) (VBag xs) =
  output "{" >> prettySequence t (map fst xs) ", " >> output "}"
prettyWHNF (TyBag t) (VBag xs) = prettyBag t xs

prettyWHNF (TyGraph a) (VGraph _ adj) = prettyWHNF (TyMap a (TySet a)) =<< rnfV adj
prettyWHNF (TyMap k v) (VMap m)
  | M.null m = output "emptyMap"
  | otherwise = do
      output "map("
      prettyWHNF (TySet (k :*: v)) =<< mapToSet k v (VMap m)
      output ")"

prettyWHNF ty v = error $
  "Impossible! No matching case in prettyWHNF for " ++ show v ++ ": " ++ show ty

prettyPlaceholder :: Member (Output String) r => Type -> Sem r ()
prettyPlaceholder ty = do
  output "<"
  tyStr <- renderDoc (prettyTy ty)
  output tyStr
  output ">"

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> [Value] -> String -> Sem r ()
prettySequence _ []     _   = output ""
prettySequence t [x]    _   = prettyV t x
prettySequence t (x:xs) del = prettyV t x >> output del >> prettySequence t xs del

prettyBag :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> [(Value, Integer)] -> Sem r ()
prettyBag _ []         = output "⟅⟆"
prettyBag t vs
  | all ((==1) . snd) vs   = output "⟅" >> prettySequence t (map fst vs) ", " >> output "⟆"
  | otherwise              = output "⟅" >> prettyCounts vs >> output "⟆"

  where
    prettyCounts []      = error "Impossible! prettyCounts []"
    prettyCounts [v]     = prettyCount v
    prettyCounts (v:vs') = prettyCount v >> output ", " >> prettyCounts vs'

    prettyCount (v,1) = prettyV t v
    prettyCount (v,n) = prettyV t v >> output (" # " ++ show n)

prettyString :: Members (Input TopInfo ': Output String ': EvalEffects) r => Value -> Sem r ()
prettyString str = output "\"" >> go str >> output "\""
  where
    toChar :: Value -> String
    toChar (VNum _ c) = drop 1 . reverse . drop 1 . reverse . show $ [chr (fromIntegral (numerator c))]
    toChar v' = error $ "Impossible! Value that's not a char in prettyString.toChar: " ++ show v'

    go :: Members (Output String ': EvalEffects) r => Value -> Sem r ()
    go v = do
      whnfList v (return ()) $ \hd tl -> do
        hd' <- whnfV hd
        output (toChar hd')
        go tl

-- | Pretty-print a list with elements of a given type, assuming the
--   list has already been reduced to WHNF.
prettyList :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyList ty v = output "[" >> go v
  where
    go w = whnfList w (output "]") $ \hd tl -> do
      prettyV ty hd
      tlWHNF <- whnfV tl
      case tlWHNF of
        VInj R _ -> output ", "
        _        -> return ()
      go tlWHNF

prettyTuple :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyTuple (ty1 :*: ty2) (VPair v1 v2) = do
  prettyV ty1 v1
  output ", "
  whnfV v2 >>= prettyTuple ty2
prettyTuple ty v = prettyV ty v

--------------------------------------------------
-- Pretty-printing decimals

-- | Pretty-print a rational number using its decimal expansion, in
--   the format @nnn.prefix[rep]...@, with any repeating digits enclosed
--   in square brackets.
prettyDecimal :: Rational -> String
prettyDecimal r = printedDecimal
   where
     (n,d) = properFraction r :: (Integer, Rational)
     (expan, len) = digitalExpansion 10 (numerator d) (denominator d)
     printedDecimal
       | length first102 > 101 || length first102 == 101 && last first102 /= 0
         = show n ++ "." ++ concatMap show (take 100 expan) ++ "..."
       | rep == [0]
         = show n ++ "." ++ (if null pre then "0" else concatMap show pre)
       | otherwise
         = show n ++ "." ++ concatMap show pre ++ "[" ++ concatMap show rep ++ "]"
       where
         (pre, rep) = splitAt len expan
         first102   = take 102 expan

-- Given a list, find the indices of the list giving the first and
-- second occurrence of the first element to repeat, or Nothing if
-- there are no repeats.
findRep :: Ord a => [a] -> ([a], Int)
findRep = findRep' M.empty 0

findRep' :: Ord a => M.Map a Int -> Int -> [a] -> ([a], Int)
findRep' _ _ [] = error "Impossible. Empty list in findRep'"
findRep' prevs ix (x:xs)
  | x `M.member` prevs = ([], prevs M.! x)
  | otherwise          = first (x:) $ findRep' (M.insert x ix prevs) (ix+1) xs

-- | @digitalExpansion b n d@ takes the numerator and denominator of a
--   fraction n/d between 0 and 1, and returns a pair of (1) a list of
--   digits @ds@, and (2) a nonnegative integer k such that @splitAt k
--   ds = (prefix, rep)@, where the infinite base-b expansion of
--   n/d is 0.@(prefix ++ cycle rep)@.  For example,
--
--   > digitalExpansion 10 1 4  = ([2,5,0], 2)
--   > digitalExpansion 10 1 7  = ([1,4,2,8,5,7], 0)
--   > digitalExpansion 10 3 28 = ([1,0,7,1,4,2,8,5], 2)
--   > digitalExpansion 2  1 5  = ([0,0,1,1], 0)
--
--   It works by performing the standard long division algorithm, and
--   looking for the first time that the remainder repeats.
digitalExpansion :: Integer -> Integer -> Integer -> ([Integer], Int)
digitalExpansion b n d = digits
  where
    longDivStep (_, r) = (b*r) `divMod` d
    res       = tail $ iterate longDivStep (0,n)
    digits    = first (map fst) (findRep res)

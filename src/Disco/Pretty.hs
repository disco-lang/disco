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
  ( module Disco.Pretty.Monadic
  , module Disco.Pretty
  )
  where

import           Prelude                          hiding ((<>))
import           System.IO                        (hFlush, stdout)

import           Control.Lens                     (view)
import           Control.Monad                    ((>=>))
import           Data.Bifunctor
import           Data.Char                        (chr, isAlpha, toLower)
import qualified Data.Map                         as M
import           Data.Ratio

import           Capability.Reader
import           Capability.State
import           Text.PrettyPrint                 (Doc)
import           Unbound.Generics.LocallyNameless (Bind, LFresh, Name, lunbind,
                                                   string2Name, unembed)

import           Disco.AST.Core
import           Disco.AST.Generic                (selectSide)
import           Disco.AST.Surface
import           Disco.Capability
import           Disco.Eval                       (topTyDefs)
import           Disco.Interpret.Core             (mapToSet, rnfV, whnfList,
                                                   whnfV)
import           Disco.Module
import           Disco.Pretty.Monadic
import           Disco.Pretty.Prec
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Typecheck.Erase            (eraseClause)
import           Disco.Types
import           Disco.Util
import           Disco.Value

withPA :: HasReader' "pa" m => PA -> m Doc -> m Doc
withPA pa = mparens pa . setPA pa

setPA :: HasReader' "pa" m => PA -> m Doc -> m Doc
setPA = local @"pa" . const

lt :: HasReader' "pa" m => m Doc -> m Doc
lt = local @"pa" (\(PA p _) -> PA p InL)

rt :: HasReader' "pa" m => m Doc -> m Doc
rt = local @"pa" (\(PA p _) -> PA p InR)

mparens :: HasReader' "pa" m => PA -> m Doc -> m Doc
mparens pa doc = do
  parentPA <- ask @"pa"
  (if pa < parentPA then parens else id) doc

--------------------------------------------------

prettyTy :: HasReader' "pa" m => Type -> m Doc
prettyTy (TyVar v)        = text (show v)
prettyTy TyVoid           = text "Void"
prettyTy TyUnit           = text "Unit"
prettyTy TyBool           = text "Bool"
prettyTy TyProp           = text "Prop"
prettyTy TyC              = text "Char"
prettyTy (ty1 :->: ty2)   = withPA tarrPA $
  lt (prettyTy ty1) <+> text "→" <+> rt (prettyTy ty2)
prettyTy (ty1 :*: ty2)    = withPA tmulPA $
  lt (prettyTy ty1) <+> text "×" <+> rt (prettyTy ty2)
prettyTy (ty1 :+: ty2)    = withPA taddPA $
  lt (prettyTy ty1) <+> text "+" <+> rt (prettyTy ty2)
prettyTy TyN              = text "ℕ"
prettyTy TyZ              = text "ℤ"
prettyTy TyQ              = text "ℚ"
prettyTy TyF              = text "𝔽"
-- prettyTy (TyFin n)        = text "ℤ" <> (integer n)
prettyTy (TyList ty)      = withPA tfunPA $
  text "List" <+> rt (prettyTy ty)
prettyTy (TyBag ty)       = withPA tfunPA $
  text "Bag" <+> rt (prettyTy ty)
prettyTy (TySet ty)       = withPA tfunPA $
  text "Set" <+> rt (prettyTy ty)
prettyTy (TyContainer (AVar (U c)) ty) = withPA tfunPA $
  text (show c) <+> rt (prettyTy ty)
prettyTy (TyUser name [])   = text name
prettyTy (TyUser name args) = withPA tfunPA $
  hsep (text name : map (rt . prettyTy) args)
prettyTy (TySkolem n)     = text "%" <> prettyName n
prettyTy (TyGraph ty)     = withPA tfunPA $
  text "Graph" <+> rt (prettyTy ty)
prettyTy (TyMap k v)      =  withPA tfunPA $
  hsep [text "Map", rt (prettyTy k), rt (prettyTy v)]

prettyPolyTy :: (LFresh m, HasReader' "pa" m) => PolyType -> m Doc
prettyPolyTy (Forall bnd) = lunbind bnd $
  \(_, body) -> prettyTy body

prettyTyDef :: HasReader' "pa" m => String -> TyDefBody -> m Doc
prettyTyDef tyName (TyDefBody ps body)
  = text tyName <+> hsep (map text ps) <+> text "=" <+> prettyTy (body (map (TyVar . string2Name) ps))

--------------------------------------------------

prettyName :: HasReader' "pa" m => Name a -> m Doc
prettyName = text . show

-- Pretty-print a term with guaranteed parentheses.
prettyTermP :: (LFresh m, HasReader' "pa" m) => Term -> m Doc
prettyTermP t@TTup{} = setPA initPA $ prettyTerm t
prettyTermP t        = withPA initPA $ prettyTerm t

prettyTerm :: (LFresh m, HasReader' "pa" m) => Term -> m Doc
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

prettySide :: HasReader' "pa" m => Side -> m Doc
prettySide L = text "left"
prettySide R = text "right"

containerDelims :: HasReader' "pa" m => Container -> (m Doc -> m Doc)
containerDelims ListContainer = brackets
containerDelims BagContainer  = bag
containerDelims SetContainer  = braces

prettyTyOp :: HasReader' "pa" m => TyOp -> m Doc
prettyTyOp Enumerate = text "enumerate"
prettyTyOp Count     = text "count"

prettyUOp :: HasReader' "pa" m => UOp -> m Doc
prettyUOp op =
  case M.lookup op uopMap of
    Just (OpInfo _ (syn:_) _) ->
      text $ syn ++ (if all isAlpha syn then " " else "")
    _ -> error $ "UOp " ++ show op ++ " not in uopMap!"

prettyBOp :: HasReader' "pa" m => BOp -> m Doc
prettyBOp op =
  case M.lookup op bopMap of
    Just (OpInfo _ (syn:_) _) -> text syn
    _                         -> error $ "BOp " ++ show op ++ " not in bopMap!"

prettyBranches :: (LFresh m, HasReader' "pa" m) => [Branch] -> m Doc
prettyBranches []     = error "Empty branches are disallowed."
prettyBranches (b:bs) =
  prettyBranch False b
  $+$
  foldr (($+$) . prettyBranch True) empty bs

prettyBranch :: (LFresh m, HasReader' "pa" m) => Bool -> Branch -> m Doc
prettyBranch com br = lunbind br $ \(gs,t) ->
  (if com then (text "," <+>) else id) (prettyTerm t <+> prettyGuards gs)

prettyGuards :: (LFresh m, HasReader' "pa" m) => Telescope Guard -> m Doc
prettyGuards TelEmpty                     = text "otherwise"
prettyGuards (fromTelescope -> gs)
  = foldr (\g r -> prettyGuard g <+> r) (text "") gs

prettyGuard :: (LFresh m, HasReader' "pa" m) => Guard -> m Doc
prettyGuard (GBool et)  = text "if" <+> prettyTerm (unembed et)
prettyGuard (GPat et p) = text "when" <+> prettyTerm (unembed et) <+> text "is" <+> prettyPattern p
prettyGuard (GLet b)    = text "let" <+> prettyBinding b

prettyBinding :: (LFresh m, HasReader' "pa" m) => Binding -> m Doc
prettyBinding (Binding Nothing x (unembed -> t))
  = hsep [prettyName x, text "=", prettyTerm t]
prettyBinding (Binding (Just (unembed -> ty)) x (unembed -> t))
  = hsep [prettyName x, text ":", prettyPolyTy ty, text "=", prettyTerm t]

prettyQuals :: (LFresh m, HasReader' "pa" m) => Telescope Qual -> m Doc
prettyQuals (fromTelescope -> qs) = do
  ds <- punctuate (text ",") (map prettyQual qs)
  hsep ds

prettyQual :: (LFresh m, HasReader' "pa" m) => Qual -> m Doc
prettyQual (QBind x (unembed -> t))
  = hsep [prettyName x, text "in", prettyTerm t]
prettyQual (QGuard (unembed -> t))
  = prettyTerm t

-- Print out a pattern with guaranteed parentheses.
prettyPatternP :: (LFresh m, HasReader' "pa" m) => Pattern -> m Doc
prettyPatternP p@PTup{} = setPA initPA $ prettyPattern p
prettyPatternP p        = withPA initPA $ prettyPattern p

prettyPattern :: (LFresh m, HasReader' "pa" m) => Pattern -> m Doc
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

-- prettyModule :: Module -> Doc
-- prettyModule = foldr ($+$) empty . map prettyDecl

prettyDecl :: (LFresh m, HasReader' "pa" m) => Decl -> m Doc
prettyDecl (DType  (TypeDecl x ty)) = prettyName x <+> text ":" <+> prettyPolyTy ty
prettyDecl (DTyDef (TypeDefn x args body))
  = text "type" <+> text x <+> hsep (map text args) <+> text "=" <+> prettyTy body
prettyDecl (DDefn  (TermDefn x bs)) = vcat $ map (prettyClause x) bs

prettyDefn :: (LFresh m, HasReader' "pa" m) => Defn -> m Doc
prettyDefn (Defn x patTys ty clauses) = vcat $
  prettyTyDecl x (foldr (:->:) ty patTys)
  :
  map (prettyClause x . eraseClause) clauses

prettyClause :: (LFresh m, HasReader' "pa" m) => Name a -> Bind [Pattern] Term -> m Doc
prettyClause x b
  = withPA funPA . lunbind b $ \(ps, t) ->
      prettyName x <> hcat (map prettyPatternP ps) <+> text "=" <+> setPA initPA (prettyTerm t)

prettyProperty :: (LFresh m, HasReader' "pa" m) => Property -> m Doc
prettyProperty = prettyTerm

prettyTyDecl :: HasReader' "pa" m => Name t -> Type -> m Doc
prettyTyDecl x ty = hsep [prettyName x, text ":", prettyTy ty]

------------------------------------------------------------

------------------------------------------------------------
-- Pretty-printing values
------------------------------------------------------------

-- XXX This needs to be refactored so (1) we don't have to plumb the
-- output callback around everywhere, and (2) to properly take
-- associativity/precedence etc. into account.

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.  This version actually prints the values on the console, followed
--   by a newline.  For a more general version, see 'prettyValueWith'.
prettyValue :: (Has '[St "top"] m, MonadDisco m) => Type -> Value -> m ()
prettyValue ty v = do
  prettyValueWith (\s -> iputStr s >> io (hFlush stdout)) ty v
  iputStrLn ""

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.  Takes a continuation that specifies how the output
--   should be processed (which will be called many times as the
--   output is produced incrementally).
prettyValueWith :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> Value -> m ()
prettyValueWith k ty = whnfV >=> prettyWHNF k ty

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyValueWithP :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> Value -> m ()
prettyValueWithP k ty@(_ :*: _) v = prettyValueWith k ty v
prettyValueWithP k ty           v = k "(" >> prettyValueWith k ty v >> k ")"

-- | Pretty-print a value which is already guaranteed to be in weak
--   head normal form.
prettyWHNF :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> Value -> m ()
prettyWHNF out (TyUser nm args) v = do
  tymap <- gets @"top" (view topTyDefs)
  case M.lookup nm tymap of
    Just (TyDefBody _ body) -> prettyWHNF out (body args) v
    Nothing                 -> error "Impossible! TyDef name does not exist in TyMap"
prettyWHNF out TyUnit          VUnit        = out "■"
prettyWHNF out TyProp          _            = prettyPlaceholder out TyProp
prettyWHNF out TyBool          (VInj s _)   = out $ map toLower (show (selectSide s False True))
prettyWHNF out TyC             (VNum _ c)   = out (show $ chr (fromIntegral (numerator c)))
prettyWHNF out (TyList TyC)    v            = prettyString out v
prettyWHNF out (TyList ty)     v            = prettyList out ty v
prettyWHNF out ty@(_ :*: _)    v            = out "(" >> prettyTuple out ty v >> out ")"
prettyWHNF out (ty1 :+: ty2) (VInj s v)
  = case s of
      L -> out "left"  >> prettyValueWithP out ty1 v
      R -> out "right" >> prettyValueWithP out ty2 v
prettyWHNF out _ (VNum d r)
  | denominator r == 1 = out $ show (numerator r)
  | otherwise          = case d of
      Fraction -> out $ show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> out $ prettyDecimal r

prettyWHNF out ty@(_ :->: _) _ = prettyPlaceholder out ty

prettyWHNF out (TySet t) (VBag xs) =
  out "{" >> prettySequence out t (map fst xs) ", " >> out "}"
prettyWHNF out (TyBag t) (VBag xs) = prettyBag out t xs

prettyWHNF out (TyGraph a) (VGraph _ adj) = prettyWHNF out (TyMap a (TySet a)) =<< rnfV adj
prettyWHNF out (TyMap k v) (VMap m)
  | M.null m = out "emptyMap"
  | otherwise = do
      out "map("
      prettyWHNF out (TySet (k :*: v)) =<< mapToSet k v (VMap m)
      out ")"

prettyWHNF _ ty v = error $
  "Impossible! No matching case in prettyWHNF for " ++ show v ++ ": " ++ show ty

prettyPlaceholder :: MonadDisco m => (String -> m ()) -> Type -> m ()
prettyPlaceholder out ty = do
  out "<"
  tyStr <- renderDoc (prettyTy ty)
  out tyStr
  out ">"

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> [Value] -> String -> m ()
prettySequence out _ []     _   = out ""
prettySequence out t [x]    _   = prettyValueWith out t x
prettySequence out t (x:xs) del = prettyValueWith out t x >> out del >> prettySequence out t xs del

prettyBag :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> [(Value, Integer)] -> m ()
prettyBag out _ []         = out "⟅⟆"
prettyBag out t vs
  | all ((==1) . snd) vs   = out "⟅" >> prettySequence out t (map fst vs) ", " >> out "⟆"
  | otherwise              = out "⟅" >> prettyCounts vs >> out "⟆"

  where
    prettyCounts []      = error "Impossible! prettyCounts []"
    prettyCounts [v]     = prettyCount v
    prettyCounts (v:vs') = prettyCount v >> out ", " >> prettyCounts vs'

    prettyCount (v,1) = prettyValueWith out t v
    prettyCount (v,n) = prettyValueWith out t v >> out (" # " ++ show n)

prettyString :: forall m. MonadDisco m => (String -> m ()) -> Value -> m ()
prettyString out str = out "\"" >> go str >> out "\""
  where
    toChar :: Value -> String
    toChar (VNum _ c) = drop 1 . reverse . drop 1 . reverse . show $ [chr (fromIntegral (numerator c))]
    toChar v' = error $ "Impossible! Value that's not a char in prettyString.toChar: " ++ show v'

    go :: MonadDisco m => Value -> m ()
    go v = do
      whnfList v (return ()) $ \hd tl -> do
        hd' <- whnfV hd
        out (toChar hd')
        go tl

-- | Pretty-print a list with elements of a given type, assuming the
--   list has already been reduced to WHNF.
prettyList :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> Value -> m ()
prettyList out ty v = out "[" >> go v
  where
    go w = whnfList w (out "]") $ \hd tl -> do
      prettyValueWith out ty hd
      tlWHNF <- whnfV tl
      case tlWHNF of
        VInj R _ -> out ", "
        _        -> return ()
      go tlWHNF

prettyTuple :: (Has '[St "top"] m, MonadDisco m) => (String -> m ()) -> Type -> Value -> m ()
prettyTuple out (ty1 :*: ty2) (VPair v1 v2) = do
  prettyValueWith out ty1 v1
  out ", "
  whnfV v2 >>= prettyTuple out ty2
prettyTuple out ty v = prettyValueWith out ty v

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

{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

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

import           Data.Bifunctor
import           Data.Char                        (isAlpha, toLower)
import qualified Data.Map                         as M
import           Data.Ratio

import           Algebra.Graph                    (foldg)

import           Disco.Effects.LFresh
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Reader

import           Text.PrettyPrint                 (Doc)
import           Unbound.Generics.LocallyNameless (Bind, Name, string2Name,
                                                   unembed)

import           Disco.AST.Core                   (Op (OEmptyGraph),
                                                   RationalDisplay (..))
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Messages
import           Disco.Module
import           Disco.Pretty.DSL
import           Disco.Pretty.Prec
import           Disco.Property
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Typecheck.Erase            (eraseClause, eraseProperty)
import           Disco.Typecheck.Solve            (SimpleConstraint (..))
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
-- Pretty type class

class Pretty t where
  pretty :: Members '[Reader PA, LFresh] r => t -> Sem r Doc

prettyStr :: Pretty t => t -> Sem r String
prettyStr = renderDoc . runLFresh . pretty

------------------------------------------------------------
-- Pretty-printing for types

instance Pretty Type where
  pretty (TyAtom a)     = pretty a
  pretty (ty1 :->: ty2) = withPA tarrPA $
    lt (pretty ty1) <+> text "→" <+> rt (pretty ty2)
  pretty (ty1 :*: ty2)  = withPA tmulPA $
    lt (pretty ty1) <+> text "×" <+> rt (pretty ty2)
  pretty (ty1 :+: ty2)  = withPA taddPA $
    lt (pretty ty1) <+> text "+" <+> rt (pretty ty2)
  pretty (TyCon c [])   = pretty c
  pretty (TyCon c tys)  = do
    ds <- setPA initPA $ punctuate (text ",") (map pretty tys)
    pretty c <> parens (hsep ds)

instance Pretty Atom where
  pretty = \case
    AVar (U v) -> pretty v
    AVar (S v) -> text "$" <> pretty v
    ABase b    -> pretty b

instance Pretty BaseTy where
  pretty = \case
    Void -> text "Void"
    Unit -> text "Unit"
    B    -> text "Bool"
    P    -> text "Prop"
    N    -> text "ℕ"
    Z    -> text "ℤ"
    Q    -> text "ℚ"
    F    -> text "𝔽"
    C    -> text "Char"
    b    -> error $ "Impossible: got " ++ show b ++ " in pretty @BaseTy"

instance Pretty Con where
  pretty = \case
    CMap    -> text "Map"
    CGraph  -> text "Graph"
    CUser s -> text s
    CList   -> text "List"
    CBag    -> text "Bag"
    CSet    -> text "Set"
    c       -> error $ "Impossible: got Con " ++ show c ++ " in pretty @Con"

-- | Pretty-print a polytype.  Note that we never explicitly print
--   @forall@; quantification is implicit, as in Haskell.
instance Pretty PolyType where
  pretty (Forall bnd) = lunbind bnd $
    \(_, body) -> pretty body

-- | Pretty-print a type definition.
instance Pretty (String, TyDefBody) where

  pretty (tyName, TyDefBody ps body)
    = text tyName <> prettyArgs ps <+> text "=" <+> pretty (body (map (TyVar . string2Name) ps))
    where
      prettyArgs [] = empty
      prettyArgs _  = do
          ds <- punctuate (text ",") (map text ps)
          parens (hsep ds)

------------------------------------------------------------
-- Pretty-printing for surface-syntax terms
--
-- The instances in this section are used to pretty-print surface
-- syntax, for example, when printing the source code definition of a
-- term (e.g. via the :doc REPL command).

instance Pretty (Name a) where
  pretty = text . show

-- | Pretty-print a term with guaranteed parentheses.
prettyTermP :: Members '[LFresh, Reader PA] r => Term -> Sem r Doc
prettyTermP t@TTup{} = setPA initPA $ pretty t
-- prettyTermP t@TContainer{} = setPA initPA $ "" <+> prettyTerm t
prettyTermP t        = withPA initPA $ pretty t

instance Pretty Term where
  pretty = \case
    TVar x      -> pretty x
    TPrim (PrimUOp uop) ->
      case M.lookup uop uopMap of
        Just (OpInfo (UOpF Pre _) (syn:_) _)  -> text syn <> text "~"
        Just (OpInfo (UOpF Post _) (syn:_) _) -> text "~" <> text syn
        _ -> error $ "pretty @Term: " ++ show uop ++ " is not in the uopMap!"
    TPrim (PrimBOp bop) -> text "~" <> pretty bop <> text "~"
    TPrim p ->
      case M.lookup p primMap of
        Just (PrimInfo _ nm True)  -> text nm
        Just (PrimInfo _ nm False) -> text "$" <> text nm
        Nothing -> error $ "pretty @Term: Prim " ++ show p ++ " is not in the primMap!"
    TParens t   -> pretty t
    TUnit       -> text "■"
    (TBool b)     -> text (map toLower $ show b)
    TChar c     -> text (show c)
    TString cs  -> doubleQuotes $ text cs
    TAbs q bnd  -> withPA initPA $
      lunbind bnd $ \(args, body) ->
      prettyQ q
        <> (hsep =<< punctuate (text ",") (map pretty args))
        <> text "."
        <+> lt (pretty body)
      where
        prettyQ Lam = text "λ"
        prettyQ All = text "∀"
        prettyQ Ex  = text "∃"

    -- special case for fully applied unary operators
    TApp (TPrim (PrimUOp uop)) t ->
      case M.lookup uop uopMap of
        Just (OpInfo (UOpF Post _) _ _) -> withPA (ugetPA uop) $
          lt (pretty t) <> pretty uop
        Just (OpInfo (UOpF Pre  _) _ _) -> withPA (ugetPA uop) $
          pretty uop <> rt (pretty t)
        _ -> error $ "pretty @Term: uopMap doesn't contain " ++ show uop

    -- special case for fully applied binary operators
    TApp (TPrim (PrimBOp bop)) (TTup [t1, t2]) -> withPA (getPA bop) $
      hsep
      [ lt (pretty t1)
      , pretty bop
      , rt (pretty t2)
      ]

    -- Always pretty-print function applications with parentheses
    TApp t1 t2  -> withPA funPA $
      lt (pretty t1) <> prettyTermP t2

    TTup ts     -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ts)
      parens (hsep ds)
    TContainer c ts e  -> setPA initPA $ do
      ds <- punctuate (text ",") (map prettyCount ts)
      let pe = case e of
                 Nothing        -> []
                 Just (Until t) -> [text "..", pretty t]
      containerDelims c (hsep (ds ++ pe))
      where
        prettyCount (t, Nothing) = pretty t
        prettyCount (t, Just n)  = lt (pretty t) <+> text "#" <+> rt (pretty n)
    TContainerComp c bqst ->
      lunbind bqst $ \(qs,t) ->
      setPA initPA $ containerDelims c (hsep [pretty t, text "|", pretty qs])
    TNat n       -> integer n
    TChain t lks -> withPA (getPA Eq) . hsep $
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

    TCase b    -> withPA initPA $
      (text "{?" <+> pretty b) $+$ text "?}"
    TAscr t ty -> withPA ascrPA $
      lt (pretty t) <+> text ":" <+> rt (pretty ty)
    TRat  r    -> text (prettyDecimal r)
    TTyOp op ty  -> withPA funPA $
      pretty op <+> pretty ty
    TWild -> text "_"

instance Pretty Side where
  pretty = \case
    L -> text "left"
    R -> text "right"

-- | Print appropriate delimiters for a container literal.
containerDelims :: Member (Reader PA) r => Container -> (Sem r Doc -> Sem r Doc)
containerDelims ListContainer = brackets
containerDelims BagContainer  = bag
containerDelims SetContainer  = braces

instance Pretty TyOp where
  pretty = \case
    Enumerate -> text "enumerate"
    Count     -> text "count"

-- | Pretty-print a unary operator, by looking up its concrete syntax
--   in the 'uopMap'.
instance Pretty UOp where
  pretty op = case M.lookup op uopMap of
    Just (OpInfo _ (syn:_) _) ->
      text $ syn ++ (if all isAlpha syn then " " else "")
    _ -> error $ "UOp " ++ show op ++ " not in uopMap!"

-- | Pretty-print a binary operator, by looking up its concrete syntax
--   in the 'bopMap'.
instance Pretty BOp where
  pretty op = case M.lookup op bopMap of
    Just (OpInfo _ (syn:_) _) -> text syn
    _                         -> error $ "BOp " ++ show op ++ " not in bopMap!"

instance Pretty [Branch] where
  pretty = \case
    [] -> error "Empty branches are disallowed."
    b:bs ->
      pretty b
      $+$
      foldr (($+$) . (text "," <+>) . pretty) empty bs

-- | Pretty-print a single branch in a case expression.
instance Pretty Branch where
  pretty br = lunbind br $ \(gs,t) ->
    pretty t <+> pretty gs

-- | Pretty-print the guards in a single branch of a case expression.
instance Pretty (Telescope Guard) where
  pretty = \case
    TelEmpty -> text "otherwise"
    gs       -> foldr (\g r -> pretty g <+> r) (text "") (fromTelescope gs)

instance Pretty Guard where
  pretty = \case
    GBool et  -> text "if" <+> pretty (unembed et)
    GPat et p -> text "when" <+> pretty (unembed et) <+> text "is" <+> pretty p
    GLet b    -> text "let" <+> pretty b

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
    QGuard (unembed -> t)  -> pretty t

-- | Pretty-print a pattern with guaranteed parentheses.
prettyPatternP :: Members '[LFresh, Reader PA] r => Pattern -> Sem r Doc
prettyPatternP p@PTup{} = setPA initPA $ pretty p
prettyPatternP p        = withPA initPA $ pretty p

-- We could probably alternatively write a function to turn a pattern
-- back into a term, and pretty-print that instead of the below.
-- Unsure whether it would be worth it.

instance Pretty Pattern where
  pretty = \case
    PVar x      -> pretty x
    PWild       -> text "_"
    PAscr p ty  -> withPA ascrPA $
      lt (pretty p) <+> text ":" <+> rt (pretty ty)
    PUnit       -> text "■"
    PBool b     -> text $ map toLower $ show b
    PChar c     -> text (show c)
    PString s   -> text (show s)
    PTup ts     -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ts)
      parens (hsep ds)
    PInj s p    -> withPA funPA $
      pretty s <> prettyPatternP p
    PNat n      -> integer n
    PCons p1 p2 -> withPA (getPA Cons) $
      lt (pretty p1) <+> text "::" <+> rt (pretty p2)
    PList ps    -> setPA initPA $ do
      ds <- punctuate (text ",") (map pretty ps)
      brackets (hsep ds)
    PAdd L p t  -> withPA (getPA Add) $
      lt (pretty p) <+> text "+" <+> rt (pretty t)
    PAdd R p t  -> withPA (getPA Add) $
      lt (pretty t) <+> text "+" <+> rt (pretty p)
    PMul L p t  -> withPA (getPA Mul) $
      lt (pretty p) <+> text "*" <+> rt (pretty t)
    PMul R p t  -> withPA (getPA Mul) $
      lt (pretty t) <+> text "*" <+> rt (pretty p)
    PSub p t    -> withPA (getPA Sub) $
      lt (pretty p) <+> text "-" <+> rt (pretty t)
    PNeg p      -> withPA (ugetPA Neg) $
      text "-" <> rt (pretty p)
    PFrac p1 p2 -> withPA (getPA Div) $
      lt (pretty p1) <+> text "/" <+> rt (pretty p2)

------------------------------------------------------------
-- Pretty-printing top-level declarations

-- prettyModule :: Module -> Doc
-- prettyModule = foldr ($+$) empty . map pretty

instance Pretty Decl where
  pretty = \case
    DType  (TypeDecl x ty) -> pretty x <+> text ":" <+> pretty ty
    DTyDef (TypeDefn x args body) ->
      text "type" <+> text x <+> hsep (map text args) <+> text "=" <+> pretty body
    DDefn  (TermDefn x bs) -> vcat $ map (pretty . (x,)) bs

instance Pretty Defn where
  pretty (Defn x patTys ty clauses) = vcat $
    pretty (x, foldr (:->:) ty patTys)
    :
    map (pretty . (x,) . eraseClause) clauses

-- | Pretty-print a single clause in a definition.
instance Pretty (Name a, Bind [Pattern] Term) where
  pretty (x, b) = withPA funPA . lunbind b $ \(ps, t) ->
    pretty x <> hcat (map prettyPatternP ps) <+> text "=" <+> setPA initPA (pretty t)

-- | Pretty-print a type declaration.
instance Pretty (Name t, Type) where
  pretty (x, ty) = hsep [pretty x, text ":", pretty ty]

------------------------------------------------------------
-- Pretty-printing values
------------------------------------------------------------

prettyValue :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc

-- Lazily expand any user-defined types
prettyValue (TyUser x args) v = do
  tydefs <- input
  let (TyDefBody _ body) = tydefs M.! x   -- This can't fail if typechecking succeeded
  prettyValue (body args) v

prettyValue _      VUnit                     = "■"
prettyValue TyProp _                         = prettyPlaceholder TyProp
prettyValue TyBool (VInj s _)                = text $ map toLower (show (s == R))
prettyValue TyC (vchar -> c)                 = text (show c)
prettyValue (TyList TyC) (vlist vchar -> cs) = doubleQuotes . text . concatMap prettyChar $ cs
  where
    prettyChar = drop 1 . reverse . drop 1 . reverse . show . (:[])
prettyValue (TyList ty) (vlist id -> xs)     = do
  ds <- punctuate (text ",") (map (prettyValue ty) xs)
  brackets (hsep ds)
prettyValue ty@(_ :*: _) v                   = parens (prettyTuple ty v)
prettyValue (ty1 :+: _) (VInj L v)           = "left"  <> prettyVP ty1 v
prettyValue (_ :+: ty2) (VInj R v)           = "right" <> prettyVP ty2 v
prettyValue _ (VNum d r)
  | denominator r == 1                       = text $ show (numerator r)
  | otherwise                                = text $ case d of
      Fraction -> show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> prettyDecimal r

prettyValue ty@(_ :->: _) _                  = prettyPlaceholder ty

prettyValue (TySet ty) (VBag xs)             = braces $ prettySequence ty "," (map fst xs)
prettyValue (TyBag ty) (VBag xs)             = prettyBag ty xs
prettyValue (TyMap tyK tyV) (VMap m)         =
  "map" <> parens (braces (prettySequence (tyK :*: tyV) "," (assocsToValues m)))
  where
    assocsToValues = map (\(k,v) -> VPair (fromSimpleValue k) v) . M.assocs

  -- XXX can we get rid of OEmptyGraph?
prettyValue (TyGraph _ ) (VConst OEmptyGraph) = "emptyGraph"
prettyValue (TyGraph ty) (VGraph g)          =
  foldg
    "emptyGraph"
    (("vertex" <>) . prettyVP ty . fromSimpleValue)
    (\l r -> withPA (getPA Add) $ lt l <+> "+" <+> rt r)
    (\l r -> withPA (getPA Mul) $ lt l <+> "*" <+> rt r)
    g

prettyValue ty v = error $ "unimplemented: prettyValue " ++ show ty ++ " " ++ show v

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyVP :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc
prettyVP ty@(_ :*: _) = prettyValue ty
prettyVP ty           = parens . prettyValue ty

prettyPlaceholder :: Members '[Reader PA, LFresh] r => Type -> Sem r Doc
prettyPlaceholder ty = "<" <> pretty ty <> ">"

prettyTuple :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Value -> Sem r Doc
prettyTuple (ty1 :*: ty2) (VPair v1 v2) = prettyValue ty1 v1 <> "," <+> prettyTuple ty2 v2
prettyTuple ty v                        = prettyValue ty v

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> Doc -> [Value] -> Sem r Doc
prettySequence ty del vs = hsep =<< punctuate (return del) (map (prettyValue ty) vs)

-- | Pretty-print a literal bag value.
prettyBag :: Members '[Input TyDefCtx, LFresh, Reader PA] r => Type -> [(Value,Integer)] -> Sem r Doc
prettyBag _ [] = bag empty
prettyBag ty vs
  | all ((==1) . snd) vs = bag $ prettySequence ty "," (map fst vs)
  | otherwise            = bag $ hsep =<< punctuate (return ",") (map prettyCount vs)
  where
    prettyCount (v,1) = prettyValue ty v
    prettyCount (v,n) = prettyValue ty v <+> "#" <+> text (show n)

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

------------------------------------------------------------
-- Pretty-printing for test results
------------------------------------------------------------

-- XXX redo with message framework, with proper support for indentation etc.

prettyTestFailure
  :: Members '[Output Message, Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r ()
prettyTestFailure _    (TestResult True _ _)    = return ()
prettyTestFailure prop (TestResult False r env) = do
  prettyFailureReason prop r
  prettyTestEnv "    Counterexample:" env

prettyTestResult
  :: Members '[Output Message, Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestResult -> Sem r ()
prettyTestResult prop r | not (testIsOk r) = prettyTestFailure prop r
prettyTestResult prop (TestResult _ r _)   = do
  dp <- renderDoc $ pretty (eraseProperty prop)
  info'       "  - Test passed: " >> info dp
  prettySuccessReason r

prettySuccessReason
  :: Members '[Output Message, Input TyDefCtx, LFresh, Reader PA] r
  => TestReason -> Sem r ()
prettySuccessReason (TestFound (TestResult _ _ vs)) = do
  prettyTestEnv "    Found example:" vs
prettySuccessReason (TestNotFound Exhaustive) = do
  info     "    No counterexamples exist."
prettySuccessReason (TestNotFound (Randomized n m)) = do
  info'       "    Checked "
  info' (show (n + m))
  info " possibilities without finding a counterexample."
prettySuccessReason _ = return ()

prettyFailureReason
  :: Members '[Output Message, Input TyDefCtx, LFresh, Reader PA] r
  => AProperty -> TestReason -> Sem r ()
prettyFailureReason prop TestBool = do
  dp <- renderDoc $ pretty (eraseProperty prop)
  info'     "  - Test is false: " >> info dp
prettyFailureReason prop (TestEqual ty v1 v2) = do
  info'     "  - Test result mismatch for: "
  info =<< renderDoc (pretty (eraseProperty prop))
  info'     "    - Left side:  "
  info =<< renderDoc (prettyValue ty v2)
  info'     "    - Right side: "
  info =<< renderDoc (prettyValue ty v1)
prettyFailureReason prop (TestRuntimeError e) = do
  info'     "  - Test failed: "
  dp <- renderDoc $ pretty (eraseProperty prop)
  info dp
  info'     "    " >> info (show e)
prettyFailureReason prop (TestFound (TestResult _ r _)) = do
  prettyFailureReason prop r
prettyFailureReason prop (TestNotFound Exhaustive) = do
  info'     "  - No example exists: "
  dp <- renderDoc $ pretty (eraseProperty prop)
  info dp
  info   "    All possible values were checked."
prettyFailureReason prop (TestNotFound (Randomized n m)) = do
  info'     "  - No example was found: "
  dp <- renderDoc $ pretty (eraseProperty prop)
  info dp
  info'     "    Checked " >> info' (show (n + m)) >> info " possibilities."

prettyTestEnv
  :: Members '[Output Message, Input TyDefCtx, LFresh, Reader PA] r
  => String -> TestEnv -> Sem r ()
prettyTestEnv _ (TestEnv []) = return ()
prettyTestEnv s (TestEnv vs) = do
  info s
  mapM_ prettyBind vs
  where
    maxNameLen = maximum . map (\(n, _, _) -> length n) $ vs
    prettyBind (x, ty, v) = do
      info' "      "
      info' x
      info' (replicate (maxNameLen - length x) ' ')
      info' " = "
      info =<< renderDoc (prettyValue ty v)

------------------------------------------------------------
-- Pretty-printing for constraints
------------------------------------------------------------

instance Pretty SimpleConstraint where
  pretty = \case
    ty1 :<: ty2 -> pretty ty1 <+> "<:" <+> pretty ty2
    ty1 :=: ty2 -> pretty ty1 <+> "=" <+> pretty ty2









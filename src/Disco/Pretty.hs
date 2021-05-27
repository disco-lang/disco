{-# LANGUAGE NoMonomorphismRestriction #-}
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

module Disco.Pretty where

import           Prelude                          hiding ((<>))
import           System.IO                        (hFlush, stdout)

import           Control.Applicative              hiding (empty)
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Char                        (chr, isAlpha, toLower)
import qualified Data.Map                         as M
import           Data.Ratio

import           Control.Lens                     (use)

import qualified Text.PrettyPrint                 as PP
import           Unbound.Generics.LocallyNameless (Bind, Name, lunbind,
                                                   string2Name, unembed)

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.Eval                       (Disco, IErr, Value (..), io,
                                                   iputStr, iputStrLn,
                                                   topTyDefns)
import           Disco.Interpret.Core             (mapToSet, rnfV, whnfV)
import           Disco.Module
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Typecheck.Erase            (eraseClause)
import           Disco.Types

--------------------------------------------------
-- Monadic pretty-printing

vcat :: Monad f => [f PP.Doc] -> f PP.Doc
vcat ds  = PP.vcat <$> sequence ds

hcat :: Monad f => [f PP.Doc] -> f PP.Doc
hcat ds  = PP.hcat <$> sequence ds

hsep :: Monad f => [f PP.Doc] -> f PP.Doc
hsep ds  = PP.hsep <$> sequence ds

parens :: Functor f => f PP.Doc -> f PP.Doc
parens   = fmap PP.parens

brackets :: Functor f => f PP.Doc -> f PP.Doc
brackets = fmap PP.brackets

braces :: Functor f => f PP.Doc -> f PP.Doc
braces = fmap PP.braces

bag :: Monad f => f PP.Doc -> f PP.Doc
bag p = text "⟅" <> p <> text "⟆"

quotes :: Functor f => f PP.Doc -> f PP.Doc
quotes = fmap PP.quotes

doubleQuotes :: Functor f => f PP.Doc -> f PP.Doc
doubleQuotes = fmap PP.doubleQuotes

text :: Monad m => String -> m PP.Doc
text     = return . PP.text

integer :: Monad m => Integer -> m PP.Doc
integer  = return . PP.integer

nest :: Functor f => Int -> f PP.Doc -> f PP.Doc
nest n d = PP.nest n <$> d

empty :: Monad m => m PP.Doc
empty    = return PP.empty

(<+>) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
(<+>) = liftA2 (PP.<+>)

(<>) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
(<>)  = liftA2 (PP.<>)

($+$) :: Applicative f => f PP.Doc -> f PP.Doc -> f PP.Doc
($+$) = liftA2 (PP.$+$)

punctuate :: Monad f => f PP.Doc -> [f PP.Doc] -> f [f PP.Doc]
punctuate p ds = do
  p' <- p
  ds' <- sequence ds
  return . map return $ PP.punctuate p' ds'

--------------------------------------------------
-- Precedence and associativity

type Prec = Int

ugetPA :: UOp -> PA
ugetPA op = PA (uPrec op) In

getPA :: BOp -> PA
getPA op = PA (bPrec op) (assoc op)

data PA = PA Prec BFixity
  deriving (Show, Eq)

instance Ord PA where
  compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)

initPA :: PA
initPA = PA 0 InL

ascrPA :: PA
ascrPA = PA 1 InL

funPA :: PA
funPA = PA funPrec InL

rPA :: Int -> PA
rPA n = PA n InR

tarrPA, taddPA, tmulPA, tfunPA :: PA
tarrPA = rPA 1
taddPA = rPA 6
tmulPA = rPA 7
tfunPA = PA 9 InL

type Doc = ReaderT PA (Disco IErr) PP.Doc

renderDoc :: Doc -> Disco IErr String
renderDoc = fmap PP.render . flip runReaderT initPA

withPA :: PA -> Doc -> Doc
withPA pa = mparens pa . setPA pa

setPA :: PA -> Doc -> Doc
setPA pa = local (const pa)

lt :: Doc -> Doc
lt = local (\(PA p _) -> PA p InL)

rt :: Doc -> Doc
rt = local (\(PA p _) -> PA p InR)

mparens :: PA -> Doc -> Doc
mparens pa doc = do
  parentPA <- ask
  (if pa < parentPA then parens else id) doc

--------------------------------------------------

prettyTy :: Type -> Doc
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

prettyPolyTy :: PolyType -> Doc
prettyPolyTy (Forall bnd) = lunbind bnd $
  \(_, body) -> prettyTy body

prettyTyDef :: String -> TyDefBody -> Doc
prettyTyDef tyName (TyDefBody ps body)
  = text tyName <+> hsep (map text ps) <+> text "=" <+> prettyTy (body (map (TyVar . string2Name) ps))

--------------------------------------------------

prettyName :: Name a -> Doc
prettyName = text . show

-- Pretty-print a term with guaranteed parentheses.
prettyTermP :: Term -> Doc
prettyTermP t@TTup{} = setPA initPA $ prettyTerm t
prettyTermP t        = withPA initPA $ prettyTerm t

prettyTerm :: Term -> Doc
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
prettyTerm TUnit         = text "()"
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
prettyTerm (TInj side t)  = withPA funPA $
  prettySide side <> prettyTermP t
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

prettySide :: Side -> Doc
prettySide L = text "left"
prettySide R = text "right"

containerDelims :: Container -> (Doc -> Doc)
containerDelims ListContainer = brackets
containerDelims BagContainer  = bag
containerDelims SetContainer  = braces

prettyTyOp :: TyOp -> Doc
prettyTyOp Enumerate = text "enumerate"
prettyTyOp Count     = text "count"

prettyUOp :: UOp -> Doc
prettyUOp op =
  case M.lookup op uopMap of
    Just (OpInfo _ (syn:_) _) ->
      text $ syn ++ (if all isAlpha syn then " " else "")
    _ -> error $ "UOp " ++ show op ++ " not in uopMap!"

prettyBOp :: BOp -> Doc
prettyBOp op =
  case M.lookup op bopMap of
    Just (OpInfo _ (syn:_) _) -> text syn
    _                         -> error $ "BOp " ++ show op ++ " not in bopMap!"

prettyBranches :: [Branch] -> Doc
prettyBranches []     = error "Empty branches are disallowed."
prettyBranches (b:bs) =
  prettyBranch False b
  $+$
  foldr (($+$) . prettyBranch True) empty bs

prettyBranch :: Bool -> Branch -> Doc
prettyBranch com br = lunbind br $ \(gs,t) ->
  (if com then (text "," <+>) else id) (prettyTerm t <+> prettyGuards gs)

prettyGuards :: Telescope Guard -> Doc
prettyGuards TelEmpty                     = text "otherwise"
prettyGuards (fromTelescope -> gs)
  = foldr (\g r -> prettyGuard g <+> r) (text "") gs

prettyGuard :: Guard -> Doc
prettyGuard (GBool et)  = text "if" <+> prettyTerm (unembed et)
prettyGuard (GPat et p) = text "when" <+> prettyTerm (unembed et) <+> text "is" <+> prettyPattern p
prettyGuard (GLet b)    = text "let" <+> prettyBinding b

prettyBinding :: Binding -> Doc
prettyBinding (Binding Nothing x (unembed -> t))
  = hsep [prettyName x, text "=", prettyTerm t]
prettyBinding (Binding (Just (unembed -> ty)) x (unembed -> t))
  = hsep [prettyName x, text ":", prettyPolyTy ty, text "=", prettyTerm t]

prettyQuals :: Telescope Qual -> Doc
prettyQuals (fromTelescope -> qs) = do
  ds <- punctuate (text ",") (map prettyQual qs)
  hsep ds

prettyQual :: Qual -> Doc
prettyQual (QBind x (unembed -> t))
  = hsep [prettyName x, text "in", prettyTerm t]
prettyQual (QGuard (unembed -> t))
  = prettyTerm t

-- Print out a pattern with guaranteed parentheses.
prettyPatternP :: Pattern -> Doc
prettyPatternP p@PTup{} = setPA initPA $ prettyPattern p
prettyPatternP p        = withPA initPA $ prettyPattern p

prettyPattern :: Pattern -> Doc
prettyPattern (PVar x)          = prettyName x
prettyPattern PWild             = text "_"
prettyPattern (PAscr p ty)      = withPA ascrPA $
  lt (prettyPattern p) <+> text ":" <+> rt (prettyTy ty)
prettyPattern PUnit             = text "()"
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

prettyDecl :: Decl -> Doc
prettyDecl (DType  (TypeDecl x ty)) = prettyName x <+> text ":" <+> prettyPolyTy ty
prettyDecl (DTyDef (TypeDefn x args body))
  = text "type" <+> text x <+> hsep (map text args) <+> text "=" <+> prettyTy body
prettyDecl (DDefn  (TermDefn x bs)) = vcat $ map (prettyClause x) bs

prettyDefn :: Defn -> Doc
prettyDefn (Defn x patTys ty clauses) = vcat $
  prettyTyDecl x (foldr (:->:) ty patTys)
  :
  map (prettyClause x . eraseClause) clauses

prettyClause :: Name a -> Bind [Pattern] Term -> Doc
prettyClause x b
  = withPA funPA . lunbind b $ \(ps, t) ->
      prettyName x <> hcat (map prettyPatternP ps) <+> text "=" <+> setPA initPA (prettyTerm t)

prettyProperty :: Property -> Doc
prettyProperty = prettyTerm

prettyTyDecl :: Name t -> Type -> Doc
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
prettyValue :: Type -> Value -> Disco IErr ()
prettyValue ty v = do
  prettyValueWith (\s -> iputStr s >> io (hFlush stdout)) ty v
  iputStrLn ""

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.  Takes a continuation that specifies how the output
--   should be processed (which will be called many times as the
--   output is produced incrementally).
prettyValueWith :: (String -> Disco IErr ()) -> Type -> Value -> Disco IErr ()
prettyValueWith k ty = whnfV >=> prettyWHNF k ty

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyValueWithP :: (String -> Disco IErr ()) -> Type -> Value -> Disco IErr ()
prettyValueWithP k ty@(_ :*: _) v = prettyValueWith k ty v
prettyValueWithP k ty           v = k "(" >> prettyValueWith k ty v >> k ")"

-- | Pretty-print a value which is already guaranteed to be in weak
--   head normal form.
prettyWHNF :: (String -> Disco IErr ()) -> Type -> Value -> Disco IErr ()
prettyWHNF out (TyUser nm args) v = do
  tymap <- use topTyDefns
  case M.lookup nm tymap of
    Just (TyDefBody _ body) -> prettyWHNF out (body args) v
    Nothing                 -> error "Impossible! TyDef name does not exist in TyMap"
prettyWHNF out TyUnit          (VCons 0 []) = out "()"
prettyWHNF out TyProp          _            = prettyPlaceholder out TyProp
prettyWHNF out TyBool          (VCons i []) = out $ map toLower (show (toEnum i :: Bool))
prettyWHNF out TyC             (VNum _ c)   = out (show $ chr (fromIntegral (numerator c)))
prettyWHNF out (TyList TyC)    v            = prettyString out v
prettyWHNF out (TyList ty)     v            = prettyList out ty v
prettyWHNF out ty@(_ :*: _)    v            = out "(" >> prettyTuple out ty v >> out ")"
prettyWHNF out (ty1 :+: ty2) (VCons i [v])
  = case i of
      0 -> out "left"  >> prettyValueWithP out ty1 v
      1 -> out "right" >> prettyValueWithP out ty2 v
      _ -> error "Impossible! Constructor for sum is neither 0 nor 1 in prettyWHNF"
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

prettyPlaceholder :: (String -> Disco IErr ()) -> Type -> Disco IErr ()
prettyPlaceholder out ty = do
  out "<"
  tyStr <- renderDoc (prettyTy ty)
  out tyStr
  out ">"

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: (String -> Disco IErr ()) -> Type -> [Value] -> String -> Disco IErr ()
prettySequence out _ []     _   = out ""
prettySequence out t [x]    _   = prettyValueWith out t x
prettySequence out t (x:xs) del = prettyValueWith out t x >> out del >> prettySequence out t xs del

prettyBag :: (String -> Disco IErr ()) -> Type -> [(Value, Integer)] -> Disco IErr ()
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

prettyString :: (String -> Disco IErr ()) -> Value -> Disco IErr ()
prettyString out str = out "\"" >> go str >> out "\""
  where
    toChar :: Value -> String
    toChar (VNum _ c) = drop 1 . reverse . drop 1 . reverse . show $ [chr (fromIntegral (numerator c))]
    toChar v' = error $ "Impossible! Value that's not a char in prettyString.toChar: " ++ show v'

    go :: Value -> Disco IErr ()
    go v = do
      v' <- whnfV v
      case v' of
        (VCons 0 []) -> return ()
        (VCons 1 [hd, tl]) -> do
          hd' <- whnfV hd
          out (toChar hd')
          go tl
        v'' -> error $ "Impossible! Value that's not a string in prettyString: " ++ show v''

-- | Pretty-print a list with elements of a given type, assuming the
--   list has already been reduced to WHNF.
prettyList :: (String -> Disco IErr ()) -> Type -> Value -> Disco IErr ()
prettyList out ty v = out "[" >> go v
  where
    go (VCons 0 []) = out "]"
    go (VCons 1 [hd, tl]) = do
      prettyValueWith out ty hd
      tlWHNF <- whnfV tl
      case tlWHNF of
        VCons 1 _ -> out ", "
        _         -> return ()
      go tlWHNF

    go v' = error $ "Impossible! Value that's not a list (or not in WHNF) in prettyList: " ++ show v'

prettyTuple :: (String -> Disco IErr ()) -> Type -> Value -> Disco IErr ()
prettyTuple out (ty1 :*: ty2) (VCons 0 [v1, v2]) = do
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

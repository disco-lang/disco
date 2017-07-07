{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns              #-}

module Disco.Pretty where

import           Control.Applicative     hiding (empty)
import           Control.Monad.Reader
import           Data.Char               (toLower, isAlpha)
import           Data.Ratio
import qualified Data.Map                as M

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, lunbind, runLFreshM,
                                          unembed, unrebind)

import           Disco.AST.Core
import           Disco.AST.Surface
import           Disco.Interpret.Core    (Value(..))
import           Disco.Types


--------------------------------------------------
-- Monadic pretty-printing

vcat :: Monad f => [f PP.Doc] -> f PP.Doc
vcat ds  = PP.vcat <$> sequence ds

hsep :: Monad f => [f PP.Doc] -> f PP.Doc
hsep ds  = PP.hsep <$> sequence ds

parens :: Functor f => f PP.Doc -> f PP.Doc
parens   = fmap PP.parens

brackets :: Functor f => f PP.Doc -> f PP.Doc
brackets = fmap PP.brackets

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

prec :: BOp -> Prec
prec op =
  case M.lookup op bopMap of
    Just (OpInfo _ _ p) -> p
    _                   -> error $ "BOp " ++ show op ++ " not in bopMap!"

assoc :: BOp -> BFixity
assoc op =
  case M.lookup op bopMap of
    Just (OpInfo (BOpF fx _) _ _) -> fx
    _                             -> error $ "BOp " ++ show op ++ " not in bopMap!"

getPA :: BOp -> PA
getPA op = PA (prec op) (assoc op)

data PA = PA Prec BFixity
  deriving (Show, Eq)

instance Ord PA where
  compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)

initPA :: PA
initPA = PA 0 InL

funPrec :: Prec
funPrec = length opTable

funPA :: PA
funPA = PA funPrec InL

arrPA :: PA
arrPA = PA 1 InR

type Doc = ReaderT PA LFreshM PP.Doc

--------------------------------------------------

prettyTy :: Type -> Doc
prettyTy (TyVar v)        = text (show v)
prettyTy TyVoid           = text "Void"
prettyTy TyUnit           = text "Unit"
prettyTy TyBool           = text "Bool"
prettyTy (TyArr ty1 ty2)  = mparens arrPA $
  prettyTy' 1 InL ty1 <+> text "→" <+> prettyTy' 1 InR ty2
prettyTy (TyPair ty1 ty2) = mparens (PA 7 InR) $
  prettyTy' 7 InL ty1 <+> text "×" <+> prettyTy' 7 InR ty2
prettyTy (TySum  ty1 ty2) = mparens (PA 6 InR) $
  prettyTy' 6 InL ty1 <+> text "+" <+> prettyTy' 6 InR ty2
prettyTy TyN              = text "ℕ"
prettyTy TyZ              = text "ℤ"
prettyTy TyQ              = text "ℚ"
prettyTy TyQP             = text "ℚ⁺"
prettyTy (TyList ty)      = mparens (PA 9 InR) $
  text "List" <+> prettyTy' 9 InR ty

prettyTy' :: Prec -> BFixity -> Type -> Doc
prettyTy' p a t = local (const (PA p a)) (prettyTy t)

--------------------------------------------------

mparens :: PA -> Doc -> Doc
mparens pa doc = do
  parentPA <- ask
  (if (pa < parentPA) then parens else id) doc

prettyName :: Name Term -> Doc
prettyName = text . show

prettyTerm :: Term -> Doc
prettyTerm (TVar x)      = prettyName x
prettyTerm TUnit         = text "()"
prettyTerm (TBool b)     = text (map toLower $ show b)
prettyTerm (TAbs bnd)    = mparens initPA $
  lunbind bnd $ \(x,body) ->
  hsep [prettyName x, text "↦", prettyTerm' 0 InL body]
prettyTerm (TJuxt t1 t2) = mparens funPA $
  prettyTerm' funPrec InL t1 <+> prettyTerm' funPrec InR t2
prettyTerm (TTup ts)     = do
  ds <- punctuate (text ",") (map (prettyTerm' 0 InL) ts)
  parens (hsep ds)
prettyTerm (TList ts)    = do
  ds <- punctuate (text ",") (map (prettyTerm' 0 InL) ts)
  brackets (hsep ds)
prettyTerm (TInj side t) = mparens funPA $
  prettySide side <+> prettyTerm' funPrec InR t
prettyTerm (TNat n)      = integer n
prettyTerm (TUn Fact t)  = prettyTerm' (1 + funPrec) InL t <> text "!"
prettyTerm (TUn op t)    = prettyUOp op <> prettyTerm' (1 + funPrec) InR t
prettyTerm (TBin op t1 t2) = mparens (getPA op) $
  hsep
  [ prettyTerm' (prec op) InL t1
  , prettyBOp op
  , prettyTerm' (prec op) InR t2
  ]
prettyTerm (TChain t lks) = mparens (getPA Eq) . hsep $
    prettyTerm' (prec Eq) InL t
    : concatMap prettyLink lks
  where
    prettyLink (TLink op t2) =
      [ prettyBOp op
      , prettyTerm' (prec op) InR t2
      ]
prettyTerm (TLet bnd) = mparens initPA $
  lunbind bnd $ \((x, unembed -> t1), t2) ->
  hsep
    [ text "let"
    , prettyName x
    , text "="
    , prettyTerm' 0 InL t1
    , text "in"
    , prettyTerm' 0 InL t2
    ]
prettyTerm (TCase b)    = nest 2 (prettyBranches b)
  -- XXX FIX ME: what is the precedence of ascription?
prettyTerm (TAscr t ty) = parens (prettyTerm t <+> text ":" <+> prettyTy ty)
prettyTerm (TRat  r)    = text (prettyDecimal r)
prettyTerm (TTyOp op ty)  = mparens funPA $
    prettyTyOp op <+> prettyTy' funPrec InR ty

prettyTerm' :: Prec -> BFixity -> Term -> Doc
prettyTerm' p a t = local (const (PA p a)) (prettyTerm t)

prettySide :: Side -> Doc
prettySide L = text "left"
prettySide R = text "right"

prettyTyOp :: TyOp -> Doc
prettyTyOp Enumerate  = text "enumerate"
prettyTyOp Count      = text "count"

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
    _ -> error $ "BOp " ++ show op ++ " not in bopMap!"

prettyBranches :: [Branch] -> Doc
prettyBranches [] = error "Empty branches are disallowed."
prettyBranches bs = foldr ($+$) empty (map prettyBranch bs)

prettyBranch :: Branch -> Doc
prettyBranch br = lunbind br $ (\(gs,t) -> text "{" <+> prettyTerm t <+> prettyGuards gs)

guardList :: Guards -> [Guard]
guardList GEmpty = []
guardList (GCons (unrebind -> (g,gs))) = g : guardList gs

prettyGuards :: Guards -> Doc
prettyGuards GEmpty                     = text "otherwise"
prettyGuards (guardList -> gs)
  = foldr (\g r -> prettyGuard g <+> r) (text "") gs

prettyGuard :: Guard -> Doc
prettyGuard (GBool et)  = text "if" <+> (prettyTerm (unembed et))
prettyGuard (GPat et p) = text "when" <+> prettyTerm (unembed et) <+> text "is" <+> prettyPattern p

prettyPattern :: Pattern -> Doc
prettyPattern (PVar x) = prettyName x
prettyPattern PWild = text "_"
prettyPattern PUnit = text "()"
prettyPattern (PBool b) = text $ map toLower $ show b
prettyPattern (PTup ts) = do
  ds <- punctuate (text ",") (map prettyPattern ts)
  parens (hsep ds)
prettyPattern (PInj s p) = prettySide s <+> prettyPattern p
prettyPattern (PNat n) = integer n
prettyPattern (PSucc p) = text "S" <+> prettyPattern p
prettyPattern (PCons {}) = error "prettyPattern PCons unimplemented"
prettyPattern (PList {}) = error "prettyPattern PCons unimplemented"

------------------------------------------------------------

-- prettyModule :: Module -> Doc
-- prettyModule = foldr ($+$) empty . map prettyDecl

prettyDecl :: Decl -> Doc
prettyDecl (DType x ty) = prettyName x <+> text ":" <+> prettyTy ty
prettyDecl (DDefn x bs) = vcat $ map prettyClause bs
  where
    prettyClause b
      = lunbind b $ \(ps, t) ->
        (prettyName x <+> (hsep $ map prettyPattern ps) <+> text "=" <+> prettyTerm t) $+$ text " "

------------------------------------------------------------

renderDoc :: Doc -> String
renderDoc = PP.render . runLFreshM . flip runReaderT initPA

------------------------------------------------------------
-- Pretty-printing values
------------------------------------------------------------

-- | Basic pretty-printing of values.
prettyValue :: Type -> Value -> String
prettyValue TyUnit (VCons 0 []) = "()"
prettyValue TyBool (VCons i []) = map toLower (show (toEnum i :: Bool))
prettyValue (TyList ty) v = prettyList ty v
prettyValue _ (VClos _ _)       = "<function>"
prettyValue _ (VThunk _ _)      = "<thunk>"
prettyValue _ (VFun _)          = "<function>"
prettyValue ty@(TyPair _ _) v   = "(" ++ prettyTuple ty v ++ ")"
prettyValue (TySum ty1 ty2) (VCons i [v])
  = case i of
      0 -> "left " ++ prettyValue ty1 v
      1 -> "right " ++ prettyValue ty2 v
      _ -> error "Impossible! Constructor for sum is neither 0 nor 1 in prettyValue"
prettyValue _ (VNum d r)
  | denominator r == 1 = show (numerator r)
  | otherwise          = case d of
      Fraction -> show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> prettyDecimal r

prettyValue _ _ = error "Impossible! No matching case in prettyValue"

prettyList :: Type -> Value -> String
prettyList ty v = "[" ++ go v
  where
    go (VCons 0 []) = "]"
    go (VCons 1 [hd, VCons 0 []]) = prettyValue ty hd ++ "]"
    go (VCons 1 [hd, tl])         = prettyValue ty hd ++ ", " ++ go tl
    go v' = error $ "Impossible! Value that's not a list in prettyList: " ++ show v'

prettyTuple :: Type -> Value -> String
prettyTuple (TyPair ty1 ty2) (VCons 0 [v1, v2]) = prettyValue ty1 v1 ++ ", " ++ prettyTuple ty2 v2
prettyTuple ty v = prettyValue ty v

--------------------------------------------------
-- Pretty-printing decimals

-- | Pretty-print a rational number using its decimal expansion, in
--   the format @nnn.prefix[rep]...@, with any repeating digits enclosed
--   in square brackets.
prettyDecimal :: Rational -> String
prettyDecimal r = show n ++ "." ++ fractionalDigits
  where
    (n,d) = properFraction r :: (Integer, Rational)
    (prefix,rep) = digitalExpansion 10 (numerator d) (denominator d)
    fractionalDigits = concatMap show prefix ++ repetend
    repetend = case rep of
      []  -> ""
      [0] -> ""
      _   -> "[" ++ concatMap show rep ++ "]"

-- Given a list, find the indices of the list giving the first and
-- second occurrence of the first element to repeat, or Nothing if
-- there are no repeats.
findRep :: Ord a => [a] -> Maybe (Int,Int)
findRep = findRep' M.empty 0

findRep' :: Ord a => M.Map a Int -> Int -> [a] -> Maybe (Int,Int)
findRep' _ _ [] = Nothing
findRep' prevs ix (x:xs)
  | x `M.member` prevs = Just (prevs M.! x, ix)
  | otherwise          = findRep' (M.insert x ix prevs) (ix+1) xs

slice :: (Int,Int) -> [a] -> [a]
slice (s,f) = drop s . take f

-- | @digitalExpansion b n d@ takes the numerator and denominator of a
--   fraction n/d between 0 and 1, and returns two lists of digits
--   @(prefix, rep)@, such that the infinite base-b expansion of n/d is
--   0.@(prefix ++ cycle rep)@.  For example,
--
--   > digitalExpansion 10 1 4  = ([2,5],[0])
--   > digitalExpansion 10 1 7  = ([], [1,4,2,8,5,7])
--   > digitalExpansion 10 3 28 = ([1,0], [7,1,4,2,8,5])
--   > digitalExpansion 2  1 5  = ([], [0,0,1,1])
--
--   It works by performing the standard long division algorithm, and
--   looking for the first time that the remainder repeats.
digitalExpansion :: Integer -> Integer -> Integer -> ([Integer],[Integer])
digitalExpansion b n d = (prefix,rep)
  where
    longDivStep (_, r) = ((b*r) `divMod` d)
    res       = tail $ iterate longDivStep (0,n)
    digits    = map fst res
    Just lims = findRep res
    rep       = slice lims digits
    prefix    = take (fst lims) digits

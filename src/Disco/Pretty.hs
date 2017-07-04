{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns              #-}

module Disco.Pretty where

import           Control.Applicative     hiding (empty)
import           Control.Monad.Reader
import           Data.Char               (toLower)
import           Data.List               (findIndex)
import           Data.Maybe              (fromJust)
import           Data.Ratio

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, lunbind, runLFreshM,
                                          unembed, unrebind)

import           Disco.AST.Surface
import           Disco.Types


--------------------------------------------------
-- Monadic pretty-printing

vcat :: Monad f => [f PP.Doc] -> f PP.Doc
vcat ds  = PP.vcat <$> sequence ds

hsep :: Monad f => [f PP.Doc] -> f PP.Doc
hsep ds  = PP.hsep <$> sequence ds

parens :: Functor f => f PP.Doc -> f PP.Doc
parens   = fmap PP.parens

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

--------------------------------------------------
-- Precedence and associativity

type Prec = Int
data Assoc = AL | AR | AN
  deriving (Show, Eq)

prec :: BOp -> Prec
prec op = fromJust . findIndex (op `elem`) $
  [ []
  , []
  , [ Or ]
  , [ And ]
  , [ Eq, Neq, Lt, Gt, Leq, Geq, Divides, RelPm ]
  , []
  , [ Add, Sub ]
  , [ Mul, Div, Mod ]
  , [ Exp ]
  ]

assoc :: BOp -> Assoc
assoc op
  | op `elem` [Add, Sub, Mul, Div, Mod] = AL
  | op `elem` [And, Or, Exp]            = AR
  | otherwise                           = AN

getPA :: BOp -> PA
getPA op = PA (prec op) (assoc op)

data PA = PA Prec Assoc
  deriving (Show, Eq)

instance Ord PA where
  compare (PA p1 a1) (PA p2 a2) = compare p1 p2 `mappend` (if a1 == a2 then EQ else LT)

initPA :: PA
initPA = PA 0 AL

funPA :: PA
funPA = PA 10 AL

arrPA :: PA
arrPA = PA 1 AR

type Doc = ReaderT PA LFreshM PP.Doc

--------------------------------------------------

prettyTy :: Type -> Doc
prettyTy (TyVar v)        = text (show v)
prettyTy TyVoid           = text "Void"
prettyTy TyUnit           = text "Unit"
prettyTy TyBool           = text "Bool"
prettyTy (TyArr ty1 ty2)  = mparens arrPA $
  prettyTy' 1 AL ty1 <+> text "→" <+> prettyTy' 1 AR ty2
prettyTy (TyPair ty1 ty2) = mparens (PA 7 AR) $
  prettyTy' 7 AL ty1 <+> text "×" <+> prettyTy' 7 AR ty2
prettyTy (TySum  ty1 ty2) = mparens (PA 6 AR) $
  prettyTy' 6 AL ty1 <+> text "+" <+> prettyTy' 6 AR ty2
prettyTy TyN              = text "ℕ"
prettyTy TyZ              = text "ℤ"
prettyTy TyQ              = text "ℚ"
prettyTy TyQP             = text "ℚ⁺"
prettyTy (TyList ty)      = mparens (PA 9 AR) $
  text "List" <+> prettyTy' 9 AR ty

prettyTy' :: Prec -> Assoc -> Type -> Doc
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
  hsep [prettyName x, text "↦", prettyTerm' 0 AL body]
prettyTerm (TJuxt t1 t2) = mparens funPA $
  prettyTerm' 10 AL t1 <+> prettyTerm' 10 AR t2
prettyTerm (TPair t1 t2) =
  parens (prettyTerm' 0 AL t1 <> text "," <+> prettyTerm' 0 AL t2)
prettyTerm (TInj side t) = mparens funPA $
  prettySide side <+> prettyTerm' 10 AR t
prettyTerm (TNat n)      = integer n
prettyTerm (TUn Fact t)  = prettyTerm' 11 AL t <> text "!"
prettyTerm (TUn op t)    = prettyUOp op <> prettyTerm' 11 AR t
prettyTerm (TBin op t1 t2) = mparens (getPA op) $
  hsep
  [ prettyTerm' (prec op) AL t1
  , prettyBOp op
  , prettyTerm' (prec op) AR t2
  ]
prettyTerm (TChain t lks) = mparens (getPA Eq) . hsep $
    prettyTerm' (prec Eq) AL t
    : concatMap prettyLink lks
  where
    prettyLink (TLink op t2) =
      [ prettyBOp op
      , prettyTerm' (prec op) AR t2
      ]
prettyTerm (TLet bnd) = mparens initPA $
  lunbind bnd $ \((x, unembed -> t1), t2) ->
  hsep
    [ text "let"
    , prettyName x
    , text "="
    , prettyTerm' 0 AL t1
    , text "in"
    , prettyTerm' 0 AL t2
    ]
prettyTerm (TCase b)    = nest 2 (prettyBranches b)
  -- XXX FIX ME: what is the precedence of ascription?
prettyTerm (TAscr t ty) = parens (prettyTerm t <+> text ":" <+> prettyTy ty)
prettyTerm (TList {})   = error "prettyTerm TList unimplemented"
prettyTerm (TRat  r)    =
     text (show (numerator r `div` denominator r)) <> text "."
  <> text (decimalize (10 * (numerator r `mod` denominator r)) (denominator r))
  where
    decimalize 0 _ = ""
    decimalize n d = show (n `div` d) ++ decimalize (10 * (n `mod` d)) d

prettyTerm' :: Prec -> Assoc -> Term -> Doc
prettyTerm' p a t = local (const (PA p a)) (prettyTerm t)

prettySide :: Side -> Doc
prettySide L = text "inl"
prettySide R = text "inr"

prettyUOp :: UOp -> Doc
prettyUOp Neg  = text "-"
prettyUOp Not  = text "not "
prettyUOp Sqrt = text "sqrt "
prettyUOp Lg   = text "lg "
prettyUOp Fact = error "Impossible! prettyUOp Fact"

prettyBOp :: BOp -> Doc
prettyBOp Add     = text "+"
prettyBOp Sub     = text "-"
prettyBOp Mul     = text "*"
prettyBOp Div     = text "/"
prettyBOp Exp     = text "^"
prettyBOp Eq      = text "="
prettyBOp Neq     = text "/="
prettyBOp Lt      = text "<"
prettyBOp Gt      = text ">"
prettyBOp Leq     = text "<="
prettyBOp Geq     = text ">="
prettyBOp And     = text "and"
prettyBOp Or      = text "or"
prettyBOp Mod     = text "mod"
prettyBOp Divides = text "|"
prettyBOp RelPm   = text "#"
prettyBOp Binom   = text "choose"
prettyBOp Cons    = text "::"

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
prettyPattern (PPair p1 p2) = parens $ prettyPattern p1 <> text "," <+> prettyPattern p2
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

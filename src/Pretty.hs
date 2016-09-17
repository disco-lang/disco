module Pretty where

import           Control.Applicative     hiding (empty)
import           Data.Char               (toLower)

import qualified Parser                  as PR
import           Types

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, bind, embed, lunbind,
                                          rec, runLFreshM, string2Name, unembed,
                                          unrec)

--------------------------------------------------
-- Monadic pretty-printing

type Doc = LFreshM PP.Doc

hsep ds  = PP.hsep <$> sequence ds
parens   = fmap PP.parens
text     = return . PP.text
integer  = return . PP.integer
nest n d = PP.nest n <$> d
empty    = return PP.empty

(<+>) = liftA2 (PP.<+>)
(<>)  = liftA2 (PP.<>)
($+$) = liftA2 (PP.$+$)

--------------------------------------------------
prettyTy :: Type -> Doc
prettyTy TyVoid = text "Void"
prettyTy TyUnit = text "Unit"
prettyTy TyBool = text "Bool"
prettyTy (TyArr ty1 ty2) = parens (prettyTy ty1) <+> text "->" <+> parens (prettyTy ty2)
prettyTy (TyPair ty1 ty2) = parens (prettyTy ty1) <+> text "*" <+> parens (prettyTy ty2)
prettyTy (TySum  ty1 ty2) = parens (prettyTy ty1) <+> text "+" <+> parens (prettyTy ty2)
prettyTy TyN = text "N"
prettyTy TyZ = text "Z"
prettyTy TyQ = text "Q"

--------------------------------------------------

prettyName :: Name Term -> Doc
prettyName = text . show

prettyTerm :: Term -> Doc
prettyTerm (TVar x)      = prettyName x
prettyTerm TUnit         = text "()"
prettyTerm (TBool b)     = text (map toLower $ show b)
prettyTerm (TAbs bnd)    =
  lunbind bnd $ \(x,body) ->
  hsep [prettyName x, text "|->", prettyTerm body]
prettyTerm (TJuxt t1 t2)  = parens (prettyTerm t1) <+> parens (prettyTerm t2) -- XXX
prettyTerm (TPair t1 t2) =
  parens (prettyTerm t1 <> text "," <+> prettyTerm t2)
prettyTerm (TInj side t) = prettySide side <+> parens (prettyTerm t)  -- XXX
prettyTerm (TNat n)      = integer n
prettyTerm (TUn op t)    = prettyUOp op <> parens (prettyTerm t)   -- XXX
prettyTerm (TBin op t1 t2) = hsep [parens $ prettyTerm t1, prettyBOp op, parens $ prettyTerm t2]  -- XXX
prettyTerm (TLet bnd) = lunbind bnd $ \(def, t2) ->
  let (x, em) = unrec def
      t1 = unembed em
   in hsep [text "let", prettyName x, text "=", prettyTerm t1, text "in", prettyTerm t2]
prettyTerm (TCase b) = text "case" $+$ nest 2 (prettyBranches b)
prettyTerm (TAscr t ty) = parens (prettyTerm t <+> text ":" <+> prettyTy ty)
prettyTerm TWrong = text "WRONG"
-- To do:
--   add precedence & associativity handling to omit unnecessary parens

prettySide :: Side -> Doc
prettySide L = text "inl"
prettySide R = text "inr"

prettyUOp :: UOp -> Doc
prettyUOp Neg = text "-"

prettyBOp :: BOp -> Doc
prettyBOp Add    = text "+"
prettyBOp Sub    = text "-"
prettyBOp Mul    = text "*"
prettyBOp Div    = text "/"
prettyBOp Equals = text "=="
prettyBOp Less   = text "<"
prettyBOp And    = text "&&"
prettyBOp Or     = text "||"

prettyBranches :: [Branch] -> Doc
  -- TODO: Why should empty branches be disallowed?  A case with no
  -- branches is exactly the right thing to eliminate a value of type
  -- Void.  I think we ought to allow it, and perhaps also come up
  -- with some nicer syntax sugar (e.g. maybe you can write 'absurd'
  -- instead of writing a case with no branches).
prettyBranches [] = error "Empty branches are disallowed."
prettyBranches bs = foldr ($+$) empty (map prettyBranch bs)

prettyBranch :: Branch -> Doc
prettyBranch br = lunbind br $ (\(gs,t) -> text "{" <+> prettyTerm t <+> prettyGuards gs)

prettyGuards :: [Guard] -> Doc
prettyGuards [] = text "otherwise"
prettyGuards gs = foldr (\g r -> prettyGuard g <+> r) (text "") gs

prettyGuard :: Guard -> Doc
prettyGuard (GIf et) = text "if" <+> (prettyTerm (unembed et))
prettyGuard (GWhere et p) = text "where" <+> prettyTerm (unembed et) <+> text "=" <+> prettyPattern p

prettyPattern :: Pattern -> Doc
prettyPattern (PVar x) = prettyName x
prettyPattern PWild = text "_"
prettyPattern PUnit = text "()"
prettyPattern (PBool b) = text $ map toLower $ show b
prettyPattern (PPair p1 p2) = parens $ prettyPattern p1 <> text "," <+> prettyPattern p2
prettyPattern (PInj s p) = prettySide s <+> prettyPattern p
prettyPattern (PNat n) = integer n
prettyPattern (PSucc p) = text "S" <+> prettyPattern p

prettyTermStr :: Term -> String
prettyTermStr = PP.render . runLFreshM . prettyTerm

echoTerm :: String -> String
echoTerm = prettyTermStr . PR.parseTermStr

echoTermP :: String -> IO ()
echoTermP = putStrLn . echoTerm

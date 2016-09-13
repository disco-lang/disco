module Pretty where

import           Control.Applicative
import           Data.Char               (toLower)

import           Types
import qualified Parser                  as PR

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, bind, embed, unembed, lunbind,
                                          rec, unrec, string2Name, runLFreshM)

--------------------------------------------------
-- Monadic pretty-printing

type Doc = LFreshM PP.Doc

hsep ds = PP.hsep <$> sequence ds
parens = fmap PP.parens
text = return . PP.text
integer = return . PP.integer

(<+>) = liftA2 (PP.<+>)
(<>) = liftA2 (PP.<>)

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
prettyTerm (TApp t1 t2)  = parens (prettyTerm t1) <+> parens (prettyTerm t2) -- XXX
prettyTerm (TPair t1 t2) =
  parens (prettyTerm t1 <> text "," <+> prettyTerm t2)
prettyTerm (TInj side t) = prettySide side <+> parens (prettyTerm t)  -- XXX
prettyTerm (TInt n)      = integer n
prettyTerm (TUn op t)    = prettyUOp op <> parens (prettyTerm t)   -- XXX
prettyTerm (TBin op t1 t2) = hsep [parens $ prettyTerm t1, prettyBOp op, parens $ prettyTerm t2]  -- XXX
prettyTerm (TLet bnd) = lunbind bnd $ \(def, t2) ->
  let (x, em) = unrec def
      t1 = unembed em
   in hsep [text "let", prettyName x, text "=", prettyTerm t1, text "in", prettyTerm t2]      
prettyTerm (TCase b) = text "case" <+> prettyBranches b
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
prettyBranches [] = error "Empty branches are disallowed." -- Need a better error message.
prettyBranches [b] = prettyBranch b
prettyBranches (b:bs) = prettyBranch b <+> prettyBranches bs
                   
prettyBranch :: Branch -> Doc
prettyBranch br = lunbind br $ (\(gs,t) -> text "{" <+> prettyTerm t <+> prettyGuards gs)

prettyGuards :: [Guard] -> Doc
-- HDE: What do we want our concrete syntax to be for the trivial guard?                
prettyGuards = foldr (\g r -> prettyGuard g <+> r) (text "")
                      
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
prettyPattern (PInt n) = integer n
prettyPattern (PSucc p) = text "S" <+> prettyPattern p
       
prettyTermStr :: Term -> String
prettyTermStr = PP.render.runLFreshM.prettyTerm

echoTerm :: String -> String
echoTerm = prettyTermStr.PR.parseTermStr
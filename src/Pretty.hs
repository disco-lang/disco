module Pretty where

import           Control.Applicative
import           Data.Char               (toLower)

import           Types

import qualified Text.PrettyPrint        as PP
import           Unbound.LocallyNameless (LFreshM, Name, bind, embed, lunbind,
                                          rec, string2Name)

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
prettyTerm TWrong = text "WRONG"
-- To do:
--   TLet
--   TCase
--
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

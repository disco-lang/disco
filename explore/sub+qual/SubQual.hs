{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

-- 26 May 2018.  Study for disco type system.

-- STLC extended with arithmetic, subtyping, and type qualifiers; a
-- bidirectional checker with constraint generation.

module SubQual where

import           Data.Coerce
import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless

import           Data.Map     (Map, (!))
import qualified Data.Map     as M
import           Data.Set     (Set)
import qualified Data.Set     as S

import Parsing2

------------------------------------------------------------
-- Atomic types
------------------------------------------------------------

data Atom where
  AVar :: Name Type -> Atom
  ANat :: Atom
  AInt :: Atom
  deriving (Show, Eq, Ord, Generic)

instance Alpha Atom

instance Subst Atom Atom where
  isvar (AVar x) = Just (SubstName (coerce x))
  isvar _        = Nothing

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

isSub :: Atom -> Atom -> Bool
isSub a1 a2 | a1 == a2 = True
isSub ANat AInt = True
isSub _ _ = False

------------------------------------------------------------
-- Type structure
------------------------------------------------------------

------------------------------------------------------------
-- Type structure
------------------------------------------------------------

data Cons where
  CArr  :: Cons
--  CPair :: Cons
  deriving (Show, Eq, Ord, Generic)

instance Alpha Cons

data Variance = Co | Contra  -- could add 'In' as well

arity :: Cons -> [Variance]
arity CArr  = [Contra, Co]
-- arity CPair = [Co, Co]

------------------------------------------------------------
-- Monotypes
------------------------------------------------------------

data Type where
  TyAtom :: Atom -> Type
  TyCons :: Cons -> [Type] -> Type
  deriving (Show, Eq, Ord, Generic)

instance Alpha Type

instance Subst Type Atom
instance Subst Type Cons
instance Subst Type Type where
  isvar (TyAtom (AVar x)) = Just (SubstName x)
  isvar _                 = Nothing

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s  = S.map (substs s)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s  = M.map (substs s)

-- type S = S' Type

-- atomToTypeSubst :: S' Atom -> S' Type
-- atomToTypeSubst = map (coerce *** TyAtom)

var :: String -> Type
var x = TyVar (string2Name x)

pattern TyVar :: Name Type -> Type
pattern TyVar v = TyAtom (AVar v)

pattern TyNat :: Type
pattern TyNat   = TyAtom ANat

pattern TyInt :: Type
pattern TyInt   = TyAtom AInt

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

-- pattern TyPair :: Type -> Type -> Type
-- pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

data Qual = QNum | QSub
  deriving (Show, Eq, Ord, Generic)

data QType = QType Qual Type
  deriving (Show, Eq, Ord, Generic)

type Delta = [QType]

data Constraint where
  CSub  :: Type -> Type -> Constraint
  CEq   :: Type -> Type -> Constraint
  CQual :: QType        -> Constraint
  CAnd  :: Constraint -> Constraint -> Constraint
  CTrue :: Constraint
  CImpl :: Delta -> Constraint -> Constraint
  CAll  :: Bind [Name Type] Constraint -> Constraint

  deriving (Show, Generic)

instance Alpha Qual
instance Alpha QType
instance Alpha Constraint

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

newtype Sigma = Forall (Bind [Name Type] (Delta, Type))
  deriving (Show, Generic)

instance Alpha Sigma

------------------------------------------------------------
-- Terms
------------------------------------------------------------

data Const = Nat Integer | Plus | Minus
  deriving (Show, Eq, Generic)

data Expr where
  EVar   :: Name Expr -> Expr
  EConst :: Const -> Expr
  ELam   :: Bind (Name Expr) Expr -> Expr
  EApp   :: Expr -> Expr -> Expr
  EAnnot :: Expr -> Sigma -> Expr
  ELet   :: Expr -> Bind (Name Expr) Expr -> Expr

  deriving (Show, Generic)

instance Alpha Const
instance Alpha Expr

instance Subst Expr Const where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Expr where
  isvar (EVar x) = Just (SubstName x)
  isvar _        = Nothing
instance Subst Expr Type where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Sigma where
  subst  _ _ = id
  substs _   = id


------------------------------------------------------------
-- Parsing
------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser emptyDef
  { reservedNames = ["let", "in", "N", "Nat", "Z", "Int", "num", "sub", "forall"]
  , opStart       = oneOf "+-"
  , opLetter      = oneOf "+-"
  }

parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser (Name Expr)
identifier = string2Name <$> getIdentifier lexer

tyvar :: Parser (Name Type)
tyvar = string2Name <$> getIdentifier lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

natural :: Parser Integer
natural = getNatural lexer

parseAtom :: Parser Expr
parseAtom
  =   EVar  <$> identifier
  <|> (EConst . Nat) <$> natural
  <|> eLam  <$> (symbol "\\" *> identifier)
            <*> (symbol "." *> parseExpr)
  <|> eLet  <$> (reserved "let" *> identifier)
            <*> (symbol "=" *> parseExpr)
            <*> (reserved "in" *> parseExpr)
  <|> parens parseExpr
  where
    eLam x e = ELam (bind x e)
    eLet x e1 e2 = ELet e1 (bind x e2)

parseExpr :: Parser Expr
parseExpr = ascribe <$> parseExpr' <*> optionMaybe (symbol ":" *> parseSigma)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = EAnnot t ty


parseExpr' :: Parser Expr
parseExpr' = buildExpressionParser table parseAtom
  where
    table = [ [ Infix  (EApp <$ reservedOp "")   AssocLeft ]
            , [ Infix  (ePlus  <$ reservedOp "+") AssocLeft
              , Infix  (eMinus <$ reservedOp "-") AssocLeft
              ]
            ]
    ePlus  e1 e2 = EApp (EApp (EConst Plus) e1) e2
    eMinus e1 e2 = EApp (EApp (EConst Minus) e1) e2

parseTypeAtom :: Parser Type
parseTypeAtom =
      (TyNat <$ (reserved "N" <|> reserved "Nat"))
  <|> (TyInt <$ (reserved "Z" <|> reserved "Int"))
  <|> (TyVar <$> tyvar)
  <|> parens parseType

parseType :: Parser Type
parseType = buildExpressionParser table parseTypeAtom
  where
    table = [ [ Infix (TyFun <$ reservedOp "->") AssocRight ] ]

parseSigma :: Parser Sigma
parseSigma = Forall <$> (reserved "forall"
                     *> (bind
                           <$> (many1 tyvar)
                           <*> ((,) <$> parseDelta <*> (symbol "." *> parseType))
                        )
                    )
         <|> trivForall <$> parseType
  where
    trivForall ty = Forall (bind [] ([], ty))

parseDelta :: Parser Delta
parseDelta = symbol "[" *> sepBy parseQType (symbol ",") <* symbol "]"

parseQType :: Parser QType
parseQType = QType <$> parseQual <*> parseTypeAtom

parseQual :: Parser Qual
parseQual = QNum <$ reserved "num"
        <|> QSub <$ reserved "sub"

expr :: Parser Expr
expr = whiteSpace *> parseExpr <* eof

tm :: String -> Expr
tm s = case parse expr s of
  Left err -> error (show err)
  Right e  -> e

------------------------------------------------------------
-- main
------------------------------------------------------------

main :: IO ()
main = return ()

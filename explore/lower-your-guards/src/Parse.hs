{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parse where

import Control.Applicative (some)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, satisfy)
import Text.Megaparsec.Char as C (alphaNumChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

data FunctionDecl where
  FunctionDecl :: Text -> Type -> Type -> FunctionDecl
  deriving (Show, Eq)

data FunctionDef where
  FunctionDef :: FunctionDecl -> [Clause] -> FunctionDef
  deriving (Show, Eq)

data Type where
  TInt :: Type
  TBool :: Type
  TFn :: Type -> Type -> Type
  deriving (Show, Eq)

data Clause where
  Clause :: Pattern -> Expr -> Clause
  deriving (Show, Eq)

data Pattern where
  PLit :: Int -> Pattern
  PWild :: Pattern
  PVar :: Var -> Pattern
  deriving (Show, Eq)

newtype Var = Var Text
  deriving (Show, Eq)

newtype Expr = Expr Text
  deriving (Show, Eq)

---
---
---

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pName :: Parser Text
pName = lexeme $ T.pack <$> some alphaNumChar

pInteger :: Parser Int
pInteger = lexeme L.decimal

pFnDecl :: Parser FunctionDecl
pFnDecl = do
  name <- pName
  _ <- symbol ":"
  tFrom <- pType
  _ <- symbol "->"
  tTo <- pType

  return $ FunctionDecl name tFrom tTo

pClause :: Text -> Parser Clause
pClause name = do
  _ <- lexeme (string name)

  pat <-
    choice
      [ -- pInteger >>= return . PLit
        pInteger <&> PLit,
        symbol "_" $> PWild,
        pName <&> PVar . Var
      ]

  _ <- symbol "="

  expr <- Expr . T.pack <$> some (satisfy (/= '\n')) <* sc

  return $ Clause pat expr

pFn :: Parser FunctionDef
pFn = do
  fnDecl <- pFnDecl
  let FunctionDecl name _ _ = fnDecl
  clauses <- some (pClause name)

  return $ FunctionDef fnDecl clauses

pType :: Parser Type
pType =
  choice
    [ TInt <$ lexeme (string "Int"),
      TBool <$ lexeme (string "Bool")
    ]

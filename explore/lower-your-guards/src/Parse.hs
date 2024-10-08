{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parse where

import Control.Applicative (some)
import Data.Char (isUpper)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Unicode (isDigit)
import Text.Megaparsec (Parsec, choice, lookAhead, satisfy)
import Text.Megaparsec.Char as C (alphaNumChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Types as Ty

data FunctionDecl where
  FunctionDecl :: Text -> Ty.Type -> Ty.Type -> FunctionDecl
  deriving (Show, Eq)

data FunctionDef where
  FunctionDef :: FunctionDecl -> [Clause] -> FunctionDef
  deriving (Show, Eq)

-- data Type where
--   TInt :: Type
--   TBool :: Type
--   TPair :: Type -> Type -> Type
--   TFn :: Type -> Type -> Type
--   ColinMistake :: Type
--   deriving (Show, Eq, Ord)

data Clause where
  Clause :: Pattern -> Ty.Type -> Expr -> Clause
  deriving (Show, Eq)

data Pattern where
  -- PLit :: Int -> Pattern
  PWild :: Pattern
  PVar :: Text -> Pattern
  PMatch :: Ty.DataConstructor -> [Pattern] -> Pattern
  deriving (Show, Eq, Ord)

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

pDataCons :: [Ty.DataConstructor] -> Parser Ty.DataConstructor
pDataCons possible = choice $ map (\dc -> dc <$ symbol (Ty.dcName dc)) possible

pDataConsMatch :: Ty.Type -> Parser Pattern
pDataConsMatch typeIn =
  do
    _ <- lookAhead $ satisfy (\x -> isUpper x || x == ',' || isDigit x)
    let cons = Ty.dataCons typeIn
    case cons of
      Just dc -> do
        dataCon <- pDataCons dc
        terms <- mapM pPattern (Ty.dcTypes dataCon)
        return $ PMatch dataCon terms
      Nothing ->
        if typeIn /= Ty.int
          then error "Found opaque type that's not an int while parsing. How??"
          else do
            i <- Ty.intCon <$> pInteger
            return $ PMatch i []

pPattern :: Ty.Type -> Parser Pattern
pPattern typeIn =
  choice
    [ symbol "_" $> PWild
    , pDataConsMatch typeIn
    , pName <&> PVar
    ]

pClause :: Text -> Ty.Type -> Parser Clause
pClause name typeIn = do
  _ <- lexeme (string name)

  pat <- pPattern typeIn

  _ <- symbol "="

  expr <- Expr . T.pack <$> some (satisfy (/= '\n')) <* sc

  return $ Clause pat typeIn expr

pFn :: Parser FunctionDef
pFn = do
  fnDecl <- pFnDecl
  let FunctionDecl name typeIn _ = fnDecl
  clauses <- some (pClause name typeIn)

  return $ FunctionDef fnDecl clauses

pType :: Parser Ty.Type
pType =
  choice
    [ Ty.int <$ lexeme (string "Int")
    , Ty.bool <$ lexeme (string "Bool")
    , Ty.throol <$ lexeme (string "Throol")
    , do
        _ <- symbol ","
        l <- pType
        r <- pType
        return $ Ty.pair l r
    , do
        _ <- lexeme (string "Either")
        l <- pType
        r <- pType
        return $ Ty.either l r
    ]

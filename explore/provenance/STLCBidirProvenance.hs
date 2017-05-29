{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- A version of the STLC extended with natural number arithmetic, with
-- bidirectional type checking/inference.  Type annotations on lambdas
-- are optional.  Note that this system does not have full type
-- reconstruction if type annotations are left off of lambdas.  But
-- this way no constraint solving has to be done, and the type
-- checking/inference algorithm returns a typing derivation which can
-- be used to display errors.  Hopefully without constraint solving
-- the resulting errors will be more easily understandable.

module STLCBidirProvenance where

import           Parsing2

import Data.List (intercalate)
import qualified Data.Map    as M
import           Data.Maybe
import           Text.Printf

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Op = Plus | Minus | Times
  deriving (Show, Eq)

data Expr where
  EVar  :: String -> Expr

  EInt  :: Integer -> Expr
  EBin  :: Op -> Expr -> Expr -> Expr

  ELam  :: String -> Maybe Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  -- EPair :: Expr -> Expr -> Expr
  -- EFst  :: Expr
  -- ESnd  :: Expr

  deriving Show

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  -- VPair    :: Value -> Value -> Value
  -- VFst     :: Value
  -- VSnd     :: Value
  deriving Show

type Env = M.Map String Value

data Type where
  TyInt  :: Type
  TyFun  :: Type -> Type -> Type
--  TyPair :: Type -> Type -> Type
  deriving (Show, Eq)

type Ctx = M.Map String Type

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser emptyDef
  { reservedNames = ["fst", "snd"]
  , opStart       = oneOf "+*-"
  , opLetter      = oneOf "+*-"
  }

parens :: Parser a -> Parser a
parens = getParens lexer

angles :: Parser a -> Parser a
angles = getAngles lexer

identifier :: Parser String
identifier = getIdentifier lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

integer :: Parser Integer
integer = getInteger lexer

parseAtom :: Parser Expr
parseAtom
  =   EVar  <$> identifier
  <|> EInt  <$> integer
  -- <|> EFst  <$  reserved "fst"
  -- <|> ESnd  <$  reserved "snd"
  <|> ELam  <$> (reservedOp "^" *> identifier)
            <*> (optionMaybe (reservedOp ":" *> parseType))
            <*> (reservedOp "." *> parseExpr)
  -- <|> angles (
  --       EPair <$> (parseExpr <* symbol ",")
  --             <*> parseExpr
  --       )
  <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (EApp <$ reservedOp "")   AssocLeft ]
            , [ Infix (EBin Times <$ reservedOp "*") AssocLeft ]
            , [ Infix (EBin Plus  <$ reservedOp "+") AssocLeft
              , Infix (EBin Minus <$ reservedOp "-") AssocLeft
              ]
            ]

parseTypeAtom :: Parser Type
parseTypeAtom =
      (TyInt <$ (reserved "Z" <|> reserved "Int"))
--  <|> angles (TyPair <$> parseType <*> (symbol "," *> parseType))
  <|> parens parseType

parseType :: Parser Type
parseType = buildExpressionParser table parseTypeAtom
  where
    table = [ [ Infix (TyFun <$ reservedOp "->") AssocRight ] ]

expr :: Parser Expr
expr = whiteSpace *> parseExpr <* eof

tm :: String -> Expr
tm s = case parse expr s of
  Left err -> error (show err)
  Right e  -> e

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0 L

  prettyPrec :: Prec -> Associativity -> p -> String
  prettyPrec _ _ = pretty

instance Pretty Type where
  prettyPrec _ _ TyInt     = "Int"
  prettyPrec p _ (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2
  -- prettyPrec _ _ (TyPair ty1 ty2) =
  --   printf "<%s, %s>" (pretty ty1) (pretty ty2)

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

instance Pretty Op where
  pretty Times = " * "
  pretty Plus  = " + "
  pretty Minus = " - "

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = x

  prettyPrec _ _ (EInt i) = show i
  prettyPrec p a (EBin Times e1 e2) =
    mparens (p>2 || (p==2 && a == R)) $
      (prettyPrec 2 L e1 ++ pretty Times ++ prettyPrec 2 R e2)
  prettyPrec p a (EBin op e1 e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ pretty op ++ prettyPrec 1 R e2)

  prettyPrec p _ (ELam x mty body) =
    mparens (p>0) $
      ("^" ++ x ++ maybe "" (\ty -> " : " ++ pretty ty) mty
           ++ ". " ++ prettyPrec 0 L body)
  prettyPrec p a (EApp e1 e2) =
    mparens (p>3 || (p==3 && a == R)) $
      (prettyPrec 3 L e1 ++ " " ++ prettyPrec 3 R e2)

  -- prettyPrec _ _ (EPair e1 e2) =
  --   printf "<%s, %s>" (pretty e1) (pretty e2)
  -- prettyPrec _ _ EFst = "fst"
  -- prettyPrec _ _ ESnd = "snd"

instance Pretty Env where
  pretty env = "[" ++ intercalate ", " bindings ++ "]"
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = x ++ " -> " ++ pretty v

instance Pretty Value where
  pretty (VInt n) = show n
  pretty (VClosure env x body)
    = printf "<%s: %s %s>"
      x (pretty body) (pretty env)
  -- pretty (VPair v1 v2)
  --   = printf "<%s, %s>" (pretty v1) (pretty v2)
  -- pretty VFst = "fst"
  -- pretty VSnd = "snd"

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

data TypeError where
  UnboundVar   :: Ctx -> String -> TypeError
  -- Mismatch     :: Expr -> Type -> Type -> TypeError
  -- MismatchFun  :: Expr -> Type -> TypeError
  -- CantInfer    :: Expr -> TypeError
  -- NotAFunction :: Expr -> Type -> TypeError

-- instance Pretty TypeError where
--   pretty (UnboundVar x) = "Unbound variable " ++ x
--   pretty (Mismatch e expected actual)
--     = printf "Type error: %s should have type %s, but has type %s."
--              (pretty e) (pretty expected) (pretty actual)
--   pretty (MismatchFun e expected)
--     = printf "Type error: %s should have type %s, but has a function type."
--              (pretty e) (pretty expected)
--   pretty (NotAFunction e ty)
--     = printf "Type error: %s should be a function, but has type %s."
--              (pretty e) (pretty ty)
--   pretty (CantInfer e)
--     = printf "Can't infer the type of %s."
--              (pretty e)

-- A typing derivation is a proof that an expression has a given
-- type.
data Typing where
  DVar :: Ctx -> String -> Type -> Typing
  DInt :: Integer -> Typing

getType :: Typing -> Type
getType (DVar _ _ ty)     = ty
getType (DInt _)          = TyInt

infer :: Ctx -> Expr -> Either TypeError Typing
infer ctx (EVar x) =
  case M.lookup x ctx of
    Just ty -> return $ DVar ctx x ty
    Nothing -> Left   $ UnboundVar ctx x
infer _   (EInt n)      = DInt n
infer ctx (EBin op e1 e2) = do
  check ctx e1 TyInt
  check ctx e2 TyInt
  return TyInt
infer ctx (EApp e1 e2) = do
  (argTy, resTy) <- checkFun ctx e1
  check ctx e2 argTy
  return resTy
infer ctx (ELam x (Just argTy) body) = do
  resTy <- infer (M.insert x argTy ctx) body
  return $ TyFun argTy resTy

-- Can't infer type of a bare lambda

-- XXX can we do something where we try inferring the type of the body
-- and hope that will suffice to constrain the type of the argument as
-- well?  But note in that case we still aren't willing to use
-- information about how the lambda is used in its context to infer
-- its type.  So it's a little bit of constraint solving but hopefully
-- a rather tame, local sort.  Will have to try this later & see how
-- it interacts with the sort of derivations returned and the sort of
-- error messages we can produce.
infer _ e@(ELam _ Nothing _) = Left $ CantInfer e

-- Note we run into trouble with fst, snd.  If they are just new
--   function constants, what is their type?  There are several options:
--
--   1. Treat applications of fst and snd as syntax (i.e. the grammar
--   has productions e ::= ... | fst e | snd e | ...); then given such
--   an application we can infer the type of e and then easily infer
--   the type of the whole expression.
--
--   2. If our system has polymorphism then we can simply assign
--   polymorphic types to the function constants fst and snd.
--
--   3. But what about in the bare STLC with fst and snd as function
--   constants? How far can we get without constraint solving,
--   especially when they are used in a higher-order way?
--
-- Hmm, here's another idea: when inferring the type of an
-- application, try it BOTH ways: infer the type of the function and
-- then check the argument type, OR if that fails then try inferring
-- the type of the argument and checking/inferring the type of the
-- function (we check the input type and infer the output type).

-- infer _ EFst = Left $ CantInfer EFst
-- infer _ ESnd = Left $ CantInfer ESnd

-- infer ctx (EPair e1 e2) = do
--   ty1 <- infer ctx e1
--   ty2 <- infer ctx e2
--   return $ TyPair ty1 ty2

check :: Ctx -> Expr -> Type -> Either TypeError ()
check ctx e@(ELam x Nothing body) ty =
  case ty of
    TyFun argTy resTy -> check (M.insert x argTy ctx) body resTy
    _                 -> Left $ MismatchFun e ty
check ctx e ty = do
  ty' <- infer ctx e
  case ty == ty' of
    True  -> return ()
    False -> Left $ Mismatch e ty ty'

checkFun :: Ctx -> Expr -> Either TypeError (Type, Type)
checkFun ctx e = do
  ty <- infer ctx e
  case ty of
    TyFun argTy resTy -> return (argTy, resTy)
    _                 -> Left $ NotAFunction e ty

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _   (EInt n) = VInt n
interp' env (EBin op ea eb)   =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (interpOp op va vb)
    _ -> error "Impossible! interp' EPlus on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case (interp' env fun, interp' env arg) of
    (VClosure env' x body, varg) ->
      interp' (M.insert x varg env') body
    -- (VFst, VPair v1 _) -> v1
    -- (VSnd, VPair _ v2) -> v2
    _ -> error $ printf "Impossible! interp' EApp with (%s) (%s)" (pretty fun) (pretty arg)
-- interp' _ EFst = VFst
-- interp' _ ESnd = VSnd
-- interp' env (EPair e1 e2) =
--   VPair (interp' env e1) (interp' env e2)

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Times = (*)
interpOp Plus  = (+)
interpOp Minus = (-)

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> case infer M.empty e of
    Left tyerr -> putStrLn $ pretty tyerr
    Right _    -> putStrLn $ pretty (interp e)

-- Example to show that closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.

main :: IO ()
main = getLine >>= eval

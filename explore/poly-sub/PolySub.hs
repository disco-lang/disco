{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- System F extended with a bit of arithmetic, some simple subtyping,
-- and let expressions.

module PolySub where

import           Parsing2

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List (intercalate)
import qualified Data.Map    as M
import           Data.Maybe
import           Text.Printf

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Op = Plus | Minus
  deriving (Show, Eq)

data Expr where
  EVar  :: String -> Expr

  ENat  :: Integer -> Expr
  EBin  :: Op -> Expr -> Expr -> Expr

  ELam  :: String -> Maybe Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  ELet  :: String -> Expr -> Expr -> Expr

  EAnn  :: Expr -> Sigma -> Expr

  deriving Show

-- Non-polymorphic types.
data Type where
  TyVar  :: String -> Type
  TyNat  :: Type
  TyInt  :: Type
  TyFun  :: Type -> Type -> Type
  deriving (Show, Eq)

-- Sigma types: top-level polymorphism.
data Sigma = Forall [String] Type
  deriving Show

mono :: Type -> Sigma
mono = Forall []

type Ctx = M.Map String Sigma

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser emptyDef
  { reservedNames = ["let", "in"]
  , opStart       = oneOf "+-"
  , opLetter      = oneOf "+-"
  }

parens :: Parser a -> Parser a
parens = getParens lexer

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

natural :: Parser Integer
natural = getNatural lexer

parseAtom :: Parser Expr
parseAtom
  =   EVar  <$> identifier
  <|> ENat  <$> natural
  <|> ELam  <$> (reservedOp "^" *> identifier)
            <*> (optionMaybe (reservedOp ":" *> parseType))
            <*> (reservedOp "." *> parseExpr)
  <|> ELet  <$> (reserved "let" *> identifier)
            <*> (symbol "=" *> parseExpr)
            <*> (reserved "in" *> parseExpr)
  <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (EApp <$ reservedOp "")   AssocLeft ]
            , [ Infix (EBin Plus  <$ reservedOp "+") AssocLeft
              , Infix (EBin Minus <$ reservedOp "-") AssocLeft
              ]
            ]

parseTypeAtom :: Parser Type
parseTypeAtom =
      (TyNat <$ (reserved "N" <|> reserved "Nat"))
  <|> (TyInt <$ (reserved "Z" <|> reserved "Int"))
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
  prettyPrec _ _ (TyVar v) = v
  prettyPrec _ _ TyNat     = "N"
  prettyPrec _ _ TyInt     = "Z"
  prettyPrec p _ (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

instance Pretty Op where
  pretty Plus  = " + "
  pretty Minus = " - "

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = x

  prettyPrec _ _ (ENat i) = show i
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

  prettyPrec p a (ELet x e1 e2) =
    "let " ++ x ++ " = " ++ prettyPrec 0 L e1 ++ " in " ++ prettyPrec 0 R e2

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

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

--------------------------------------------------
-- Type checking monad

newtype TC a = TC { unTC :: StateT Int (ReaderT Ctx (Except TypeError)) a }
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadState Int, MonadError TypeError)

runTC :: TC a -> Either TypeError a
runTC (TC t) = runExcept . flip runReaderT M.empty . flip evalStateT 0 $ t

extend :: String -> Sigma -> TC a -> TC a
extend x s = local (M.insert x s)

--------------------------------------------------
-- Inference mode

data SubC = Type :<: Type

data TypeError where
  Unbound  :: String -> TypeError
  NonNum   :: Type   -> TypeError
  NoLub    :: Type -> Type -> TypeError
  NotFunTy :: Type -> TypeError

-- Infer returns a *monotype* (which can contain type variables).
-- There is a separate function for inferring a polytype, which first
-- runs 'infer' and then generalizes over free type variables.

infer :: Expr -> TC (Type, [SubC])
infer (EVar x) = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing  -> throwError $ Unbound x
    Just sig -> undefined   -- generate fresh ty vars
infer (ENat _) = return (TyNat, [])
infer (EBin Plus e1 e2 ) = do
  (v1, d1) <- infer e1
  (v2, d2) <- infer e2
  checkNumTy v1
  checkNumTy v2
  (, d1 ++ d2) <$> lub v1 v2
infer (EBin Minus e1 e2) = do
  d1 <- check e1 TyInt
  d2 <- check e2 TyInt
  return (TyInt, d1 ++ d2)
infer (ELam x (Just ty1) e) = extend x (mono ty1) $ do
  (ty2, d) <- infer e
  return (TyFun ty1 ty2, d)
infer (EApp e1 e2) = do
  (funty, d1) <- infer e1
  case funty of
    TyFun ty1 ty2 -> do
      d2 <- check e2 ty1
      return (ty2, d1 ++ d2)
    _             -> throwError $ NotFunTy funty
infer (EAnn t sig) = undefined
  -- instantiate sig with fresh ty vars
  -- check t with the resulting type
  -- ensure that there are no nontrivial constraints containing the fresh ty vars
  -- return sig along with only those constraints not mentioning the fresh ty vars

  -- Should write something to simplify subtyping constraints as we go
  -- along.  At this point I don't think it's realistic to only
  -- collect them and solve at the very end.

checkNumTy :: Type -> TC ()
checkNumTy TyNat = return ()
checkNumTy TyInt = return ()
checkNumTy ty    = throwError $ NonNum ty

lub :: Type -> Type -> TC Type
lub t1 t2 | t1 == t2 = return t1
lub TyNat TyInt = return TyInt
lub TyInt TyNat = return TyInt
lub t1@(TyFun t11 t12) t2@(TyFun t21 t22)
  | t11 /= t21 = throwError $ NoLub t1 t2
  | otherwise  = do
      t2 <- lub t12 t22
      return $ TyFun t11 t2

--------------------------------------------------
-- Checking mode

check :: Expr -> Type -> TC [SubC]
check e ty = undefined

-- check ctx e@(ELam x Nothing body) ty =
--   case ty of
--     TyFun argTy resTy -> check (M.insert x argTy ctx) body resTy
--     _                 -> Left $ MismatchFun e ty
-- check ctx e ty = do
--   ty' <- infer ctx e
--   case ty == ty' of
--     True  -> return ()
--     False -> Left $ Mismatch e ty ty'

-- checkFun :: Ctx -> Expr -> Either TypeError (Type, Type)
-- checkFun ctx e = do
--   ty <- infer ctx e
--   case ty of
--     TyFun argTy resTy -> return (argTy, resTy)
--     _                 -> Left $ NotAFunction e ty

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  deriving Show

type Env = M.Map String Value

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _   (ENat n) = VInt n
interp' env (EBin op ea eb)   =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (interpOp op va vb)
    _ -> error "Impossible! interp' EBin on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case (interp' env fun, interp' env arg) of
    (VClosure env' x body, varg) ->
      interp' (M.insert x varg env') body
    _ -> error $ printf "Impossible! interp' EApp with (%s) (%s)" (pretty fun) (pretty arg)
interp' env (ELet x e1 e2) =
  interp' (M.insert x (interp' env e1) env) e2

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Plus  = (+)
interpOp Minus = (-)

-- eval :: String -> IO ()
-- eval s = case parse expr s of
--   Left err -> print err
--   Right e -> case infer M.empty e of
--     Left tyerr -> putStrLn $ pretty tyerr
--     Right _    -> putStrLn $ pretty (interp e)

-- Example to show that closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.

-- main :: IO ()
-- main = getLine >>= eval

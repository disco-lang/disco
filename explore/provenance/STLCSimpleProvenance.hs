{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- A version of the STLC extended with natural number arithmetic, with
-- bidirectional type checking/inference.  Type annotations on lambdas
-- are optional.  Note that this system does not have full type
-- reconstruction if type annotations are left off of lambdas.  But
-- this way no constraint solving has to be done, and the type
-- checking/inference algorithm returns a typing derivation which can
-- be used to display errors.  Hopefully without constraint solving
-- the resulting errors will be more easily understandable.

-- Optional idea to try after getting the basic system working: when
-- inferring the type of an application, try it BOTH ways: infer the
-- type of the function and then check the argument type, OR if that
-- fails then try inferring the type of the argument and
-- checking/inferring the type of the function (we check the input
-- type and infer the output type).


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

  deriving Show

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  deriving Show

type Env = M.Map String Value

data Type where
  TyInt  :: Type
  TyFun  :: Type -> Type -> Type
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
  <|> ELam  <$> (reservedOp "^" *> identifier)
            <*> (optionMaybe (reservedOp ":" *> parseType))
            <*> (reservedOp "." *> parseExpr)
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

-- Note we *don't* want two different types for typing derivations and
-- type errors; they would have a lot of shared structure.

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
  And 'True 'True = 'True
  And _     _     = 'False

-- A typing derivation is a proof that an expression has a given
-- type, OR a proof that it has no type.
data Typing :: Bool -> * where
  DVar     :: Ctx -> String -> Type -> Typing 'True
  DUnbound :: Ctx -> String -> Typing 'False
  DInt     :: Integer -> Typing 'True
  DOp      :: Op -> Typing b1 -> Typing b2 -> Typing (And b1 b2)

instance Pretty (Typing b) where
  pretty = undefined

-- We can always extract the final type assigned to the term by a
-- successful typing derivation.
getType :: Typing 'True -> Type
getType (DVar _ _ ty)     = ty
getType (DInt _)          = TyInt
getType (DOp _ _ _)       = TyInt

-- We can also extract the original expression from any typing derivation.
getExpr :: Typing b -> Expr
getExpr (DVar _ x _)     = EVar x
getExpr (DUnbound _ x)   = EVar x
getExpr (DInt n)         = EInt n
getExpr (DOp op ty1 ty2) = EBin op (getExpr ty1) (getExpr ty2)

data ATyping where
  ATyping :: Typing b -> ATyping
  -- XXX todo: Singleton constraint or something like that, to recover b?


-- Note we *don't* want an error monad, since we don't want to
-- short-circuit if a recursive call generates a type error, rather,
-- we want to continue building up a Typing as we go up, explaining
-- why we were checking the thing that went wrong.

infer :: Ctx -> Expr -> ATyping
infer ctx (EVar x) =
  case M.lookup x ctx of
    Just ty -> ATyping $ DVar ctx x ty
    Nothing -> ATyping $ DUnbound ctx x
infer _   (EInt n) = ATyping $ DInt n
infer ctx (EBin op e1 e2) = dOp (check ctx e1 TyInt) (check ctx e2 TyInt)
  where
    dOp (ATyping ty1) (ATyping ty2) = ATyping (DOp op ty1 ty2)

-- infer ctx (EApp e1 e2) = do
--   (argTy, resTy) <- checkFun ctx e1
--   check ctx e2 argTy
--   return resTy
-- infer ctx (ELam x (Just argTy) body) = do
--   resTy <- infer (M.insert x argTy ctx) body
--   return $ TyFun argTy resTy

-- -- Can't infer type of a bare lambda
-- infer _ e@(ELam _ Nothing _) = Left $ CantInfer e


check :: Ctx -> Expr -> Type -> ATyping
check = undefined
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
    _ -> error $ printf "Impossible! interp' EApp with (%s) (%s)" (pretty fun) (pretty arg)

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Times = (*)
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

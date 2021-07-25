{-# LANGUAGE GADTs #-}

-- STLC + integers and addition.

-- There's some nontrivial stuff here that happens with bidirectional
-- type checking, Church vs Curry style, etc.  How to break off a
-- reasonable chunk of it to do after finishing closures on Thursday?
-- Just give them a bunch of discussion questions to think about?
-- Introduce them to turnstile + inference rule notation?  Can I
-- typeset it via Pandoc + MathJax?

import           Parsing2

import qualified Data.Map    as M
import           Data.Maybe
import           Text.Printf

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Expr where
  EVar  :: String -> Expr
  EInt  :: Integer -> Expr
  EPlus :: Expr -> Expr -> Expr
  ELam  :: String -> Maybe Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr
  deriving Show

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  deriving Show

type Env = M.Map String Value

data Type where
  TyInt :: Type
  TyFun :: Type -> Type -> Type
  deriving (Show, Eq)

type Ctx = M.Map String Type

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser String
identifier = getIdentifier lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

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
            , [ Infix (EPlus <$ reservedOp "+") AssocLeft ]
            ]

parseTypeAtom :: Parser Type
parseTypeAtom =
  (TyInt <$ reserved "Int") <|> parens parseType

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

prettyType :: Type -> String
prettyType = prettyTyPrec 0
  where
    prettyTyPrec :: Prec -> Type -> String
    prettyTyPrec _ TyInt = "Int"
    prettyTyPrec p (TyFun ty1 ty2) =
      mparens (p > 0) $ prettyTyPrec 1 ty1 ++ " -> " ++ prettyTyPrec 0 ty2

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

pretty :: Expr -> String
pretty = prettyPrec 0 L
  where
    prettyPrec :: Prec -> Associativity -> Expr -> String
    prettyPrec _ _ (EVar x) = x
    prettyPrec _ _ (EInt i) = show i
    prettyPrec p a (EPlus e1 e2) =
      mparens (p>1 || (p==1 && a == R)) $
        (prettyPrec 1 L e1 ++ " + " ++ prettyPrec 1 R e2)
    prettyPrec p _ (ELam x mty body) =
      mparens (p>0) $
        ("^" ++ x ++ maybe "" (\ty -> " : " ++ prettyType ty) mty
             ++ ". " ++ prettyPrec 0 L body)
    prettyPrec p a (EApp e1 e2) =
      mparens (p>2 || (p==2 && a == R)) $
        (prettyPrec 2 L e1 ++ " " ++ prettyPrec 2 R e2)

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

data TypeError where
  UnboundVar   :: String -> TypeError
  Mismatch     :: Expr -> Type -> Type -> TypeError
  MismatchFun  :: Expr -> Type -> TypeError
  CantInfer    :: Expr -> TypeError
  NotAFunction :: Expr -> Type -> TypeError

prettyTypeError :: TypeError -> String
prettyTypeError (UnboundVar x) = "Unbound variable " ++ x
prettyTypeError (Mismatch e expected actual)
  = printf "Type error: %s should have type %s, but has type %s."
           (pretty e) (prettyType expected) (prettyType actual)
prettyTypeError (MismatchFun e expected)
  = printf "Type error: %s should have type %s, but has a function type."
           (pretty e) (prettyType expected)
prettyTypeError (NotAFunction e ty)
  = printf "Type error: %s should be a function, but has type %s."
           (pretty e) (prettyType ty)
prettyTypeError (CantInfer e)
  = printf "Can't infer the type of %s."
           (pretty e)

infer :: Ctx -> Expr -> Either TypeError Type
infer ctx (EVar x) =
  case M.lookup x ctx of
    Just ty -> return ty
    Nothing -> Left $ UnboundVar x
infer _   (EInt _)      = return TyInt
infer ctx (EPlus e1 e2) = do
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
infer _ e = Left $ CantInfer e

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

prettyValue :: Value -> String
prettyValue = show

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _   (EInt n) = VInt n
interp' env (EPlus ea eb)   =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (va + vb)
    _ -> error "Impossible! interp' EPlus on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case interp' env fun of
    VClosure env' x body ->
      interp' (M.insert x (interp' env arg) env') body
    _ -> error "Impossible! interp' EApp on non-closure"

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> case infer M.empty e of
    Left tyerr -> putStrLn $ prettyTypeError tyerr
    Right _    -> putStrLn $ prettyValue (interp e)

-- Example to show that closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.

main :: IO ()
main = getLine >>= eval

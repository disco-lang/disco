{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- H-M + integers and addition, with type reconstruction.

import           Text.Parsec            (parse)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token      as T

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Void
import           Text.Printf

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Expr where
  EVar  :: String -> Expr
  EInt  :: Integer -> Expr
  EPlus :: Expr -> Expr -> Expr
  ELam  :: String -> Maybe Type -> Expr -> Expr
  ETLam :: String -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr
  deriving Show

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  deriving Show

type Env = M.Map String Value

data Type' v where
  TyVar :: v -> Type' v
  TyInt :: Type' v
  TyFun :: Type' v -> Type' v -> Type' v
  deriving (Show, Eq, Functor)

translate :: (u -> v) -> Type' u -> Type' v
translate = fmap

type Type = Type' Void

type Ctx' v = M.Map String (Type' v)

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: T.TokenParser u
lexer = T.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = T.parens lexer

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

integer :: Parser Integer
integer = T.integer lexer

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
tm s = case parse expr "" s of
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

instance Pretty Void where
  pretty = absurd

instance Pretty v => Pretty (Type' v) where
  prettyPrec _ _ (TyVar v) = pretty v
  prettyPrec _ _ TyInt     = "Int"
  prettyPrec p _ (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = x
  prettyPrec _ _ (EInt i) = show i
  prettyPrec p a (EPlus e1 e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " + " ++ prettyPrec 1 R e2)
  prettyPrec p _ (ELam x mty body) =
    mparens (p>0) $
      ("^" ++ x ++ maybe "" (\ty -> " : " ++ pretty ty) mty
           ++ ". " ++ prettyPrec 0 L body)
  prettyPrec p a (EApp e1 e2) =
    mparens (p>2 || (p==2 && a == R)) $
      (prettyPrec 2 L e1 ++ " " ++ prettyPrec 2 R e2)

instance Pretty Env where
  pretty env = "[" ++ intercalate ", " bindings ++ "]"
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = x ++ " -> " ++ pretty v

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

data TypeError where
  UnboundVar   :: String -> TypeError
  Infinite     :: UVar  -> UType -> TypeError
  CantUnify    :: UType -> UType -> TypeError

instance Pretty TypeError where
  pretty (UnboundVar x)      = printf "Unbound variable %s" x
  pretty (Infinite x ty)     = printf "Infinite type %s = %s" (pretty x) (pretty ty)
  pretty (CantUnify ty1 ty2) = printf "Can't unify %s and %s" (pretty ty1) (pretty ty2)

data UVar = UVar String
  deriving (Show, Eq, Ord)

instance Pretty UVar where
  pretty (UVar v) = v

type UType = Type' UVar

embed :: Type -> UType
embed = translate absurd

data Constraint = UType :=: UType
  deriving Show

instance Pretty Constraint where
  pretty (ty1 :=: ty2) = pretty ty1 ++ " = " ++ pretty ty2

data InferState = InferState
  { _nameSupply  :: [String]
  , _constraints :: [Constraint]
  }

initInferState :: InferState
initInferState = InferState names []
  where
    names = map (("u"++) . show) [0 :: Int ..]

makeLenses ''InferState

type InferM = ReaderT (Ctx' UVar) (StateT InferState (Except TypeError))

runInferM :: InferM a -> Except TypeError (a, [Constraint])
runInferM
  = (traverse . _2 %~ view constraints)
  . flip runStateT initInferState
  . flip runReaderT M.empty

fresh :: InferM UType
fresh = do
  names <- use nameSupply
  nameSupply %= tail
  return . TyVar . UVar $ head names

withBinding :: String -> UType -> InferM a -> InferM a
withBinding x ty = local (M.insert x ty)

(===) :: UType -> UType -> InferM ()
ty1 === ty2 = constraints %= ((ty1 :=: ty2) :)

infer :: Expr -> InferM UType
infer (EVar x) = do
  ctx <- ask
  case M.lookup x ctx of
    Just ty -> return ty
    Nothing -> throwError $ UnboundVar x
infer (EInt _)      = return TyInt
infer (EPlus e1 e2) = do
  check e1 TyInt
  check e2 TyInt
  return TyInt
infer (EApp e1 e2) = do
  funTy <- infer e1
  argTy <- infer e2

  resTy <- fresh
  funTy === TyFun argTy resTy
  return resTy
infer (ELam x margTy body) = do
  argTy <- case margTy of
    Nothing -> fresh
    Just ty -> return (embed ty)
  withBinding x argTy $ do
    resTy <- infer body
    return $ TyFun argTy resTy

check :: Expr -> UType -> InferM ()
check e ty = do
  ty' <- infer e
  ty === ty'

newtype Subst = Subst (M.Map UVar UType)
  deriving Show

(.@) :: Subst -> Subst -> Subst
s2@(Subst m2) .@ Subst m1 = Subst $ M.union (M.map (applySubst s2) m1) m2

idSubst :: Subst
idSubst = Subst M.empty

(|->) :: UVar -> UType -> Subst
x |-> ty = Subst $ M.singleton x ty

applySubst :: Subst -> UType -> UType
applySubst (Subst s) ty@(TyVar x)
  = case M.lookup x s of
      Nothing  -> ty
      Just ty' -> ty'
applySubst _ TyInt           = TyInt
applySubst s (TyFun ty1 ty2) = TyFun (applySubst s ty1) (applySubst s ty2)

substConstraints :: Subst -> [Constraint] -> [Constraint]
substConstraints = map . substConstraint
  where
    substConstraint sub (ty1 :=: ty2) = applySubst sub ty1 :=: applySubst sub ty2

unify :: [Constraint] -> Except TypeError Subst
unify []     = return idSubst
unify (c:cs) = do
  u <- unifyOne c
  case u of
    Left sub    -> (.@ sub) <$> unify (substConstraints sub cs)
    Right newCs -> unify (newCs ++ cs)

occurs :: UVar -> UType -> Bool
occurs x (TyVar t)     = x == t
occurs _ TyInt         = False
occurs x (TyFun t1 t2) = occurs x t1 || occurs x t2

unifyOne :: Constraint -> Except TypeError (Either Subst [Constraint])
unifyOne (ty1 :=: ty2)
  | ty1 == ty2 = return $ Left idSubst
unifyOne (TyVar x :=: ty2)
  | occurs x ty2 = throwError $ Infinite x ty2
  | otherwise    = return $ Left (x |-> ty2)
unifyOne (ty1 :=: x@(TyVar _))
  = unifyOne (x :=: ty1)
unifyOne ((TyFun ty11 ty12) :=: (TyFun ty21 ty22))
  = return $ Right [ty11 :=: ty21, ty12 :=: ty22]
unifyOne (ty1 :=: ty2) =
  throwError $ CantUnify ty1 ty2

resolveUTy :: UType -> Type
resolveUTy (TyVar _)     = TyInt
resolveUTy TyInt         = TyInt
resolveUTy (TyFun u1 u2) = TyFun (resolveUTy u1) (resolveUTy u2)

recon :: Expr -> Except TypeError Type
recon e = do
  (uty, cs) <- runInferM (infer e)
  sub <- unify cs
  let uty' = applySubst sub uty
  return $ resolveUTy uty'

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

instance Pretty Value where
  pretty (VInt n) = show n
  pretty (VClosure env x body)
    = printf "<%s: %s %s>"
      x (pretty body) (pretty env)

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _   (EInt n) = VInt n
interp' env (EPlus ea eb)   =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (va + vb)
    _                  -> error "Impossible! interp' EPlus on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case interp' env fun of
    VClosure env' x body ->
      interp' (M.insert x (interp' env arg) env') body
    _ -> error "Impossible! interp' EApp on non-closure"

eval :: String -> IO ()
eval s = case parse expr "" s of
  Left err -> print err
  Right e -> case runExcept (recon e) of
    Left tyerr -> putStrLn $ pretty tyerr
    Right ty   -> do
      putStrLn $ pretty e ++ " : " ++ pretty ty
      putStrLn $ pretty (interp e)

------------------------------------------------------------
-- main
------------------------------------------------------------

main = do
  s <- getLine
  eval s

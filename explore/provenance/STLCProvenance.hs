-- A version of the STLC extended with pairs and natural number
-- arithmetic, with constraint-based type
-- checking/inference/reconstruction, and tracking provenance of
-- constraints and errors, in order to produce informative/explorable
-- error messages.

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module STLCProvenance where

-- TODO:
--
-- Provenance for constraints and errors
-- Replace Parsing2 with something more modern? megaparsec?

import           Parsing2

import           Data.Tree
import           Data.Monoid ((<>))
import           Control.Arrow ((***))
import           Control.Lens (makeLenses, _2, (%~), view, (<%=), (%=))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Void
import           Text.Printf

------------------------------------------------------------
-- AST
------------------------------------------------------------

data Op = Plus | Minus | Times
  deriving (Show, Eq)

data Expr where
  EVar  :: String -> Expr

  EInt  :: Integer -> Expr
  EBin  :: Op -> Expr -> Expr -> Expr

  ELam  :: String -> Maybe Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  EPair :: Expr -> Expr -> Expr
  EFst  :: Expr
  ESnd  :: Expr
  deriving Show

data Value where
  VInt     :: Integer -> Value
  VClosure :: Env -> String -> Expr -> Value
  VPair    :: Value -> Value -> Value
  VFst     :: Value
  VSnd     :: Value
  deriving Show

type Env = M.Map String Value

data Type' v where

  -- Include a case for type variables which will be used later during
  -- unification.
  TyVar  :: v -> Type' v

  TyInt  :: Type' v
  TyFun  :: Type' v -> Type' v -> Type' v
  TyPair :: Type' v -> Type' v -> Type' v
  deriving (Show, Eq, Functor)

translate :: (u -> v) -> Type' u -> Type' v
translate = fmap

-- Normal STLC types have no type variables.
type Type = Type' Void

type Ctx' v = M.Map String (Type' v)

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
  <|> EFst  <$  reserved "fst"
  <|> ESnd  <$  reserved "snd"
  <|> ELam  <$> (reservedOp "^" *> identifier)
            <*> (optionMaybe (reservedOp ":" *> parseType))
            <*> (reservedOp "." *> parseExpr)
  <|> angles (
        EPair <$> (parseExpr <* symbol ",")
              <*> parseExpr
        )
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
  <|> angles (TyPair <$> parseType <*> (symbol "," *> parseType))
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

instance Pretty Void where
  pretty = absurd

instance Pretty v => Pretty (Type' v) where
  prettyPrec _ _ (TyVar v) = pretty v
  prettyPrec _ _ TyInt     = "Int"
  prettyPrec p _ (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2
  prettyPrec _ _ (TyPair ty1 ty2) =
    printf "<%s, %s>" (pretty ty1) (pretty ty2)

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

  prettyPrec _ _ (EPair e1 e2) =
    printf "<%s, %s>" (pretty e1) (pretty e2)
  prettyPrec _ _ EFst = "fst"
  prettyPrec _ _ ESnd = "snd"

instance Pretty Env where
  pretty env = "[" ++ intercalate ", " bindings ++ "]"
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = x ++ " -> " ++ pretty v

------------------------------------------------------------
-- Type checking/reconstruction
------------------------------------------------------------

--------------------------------------------------
-- Types with unification variables

data UVar = UVar String
  deriving (Show, Eq, Ord)

instance Pretty UVar where
  pretty (UVar v) = v

type UType = Type' UVar

embed :: Type -> UType
embed = translate absurd

--------------------------------------------------
-- Substitutions

newtype Subst = Subst (M.Map UVar (UType, Reason))
  deriving Show

(.@) :: Subst -> Subst -> Subst
s2@(Subst m2) .@ Subst m1 = Subst $ M.union (M.map (applySubst s2 *** RSubst s2) m1) m2

idSubst :: Subst
idSubst = Subst M.empty

isEmptySubst :: Subst -> Bool
isEmptySubst (Subst s) = M.null s

(|->) :: UVar -> (UType, Reason) -> Subst
x |-> ty = Subst $ M.singleton x ty

applySubst :: Subst -> UType -> UType
applySubst (Subst s) ty@(TyVar x)
  = case M.lookup x s of
      Nothing      -> ty
      Just (ty',_) -> ty'
applySubst _ TyInt            = TyInt
applySubst s (TyFun ty1 ty2)  = TyFun (applySubst s ty1) (applySubst s ty2)
applySubst s (TyPair ty1 ty2) = TyPair (applySubst s ty1) (applySubst s ty2)

substConstraints :: Subst -> [Constraint] -> [Constraint]
substConstraints = map . substConstraint
  where
    substConstraint sub ((ty1 :=: ty2) :? p)
      = (applySubst sub ty1 :=: applySubst sub ty2) :? RSubst sub p

--------------------------------------------------
-- Constraints

data Reason where
  RUnknown :: Reason
  RFun    :: Expr -> Expr -> Reason
  RApp    :: Expr -> Expr -> Reason
  RCheck  :: Expr -> Reason

  RSym    :: Reason -> Reason

  RFunArg :: RawConstraint -> Reason -> Reason
  RFunRes :: RawConstraint -> Reason -> Reason
  RPairFst :: Reason -> Reason
  RPairSnd :: Reason -> Reason

  RSubst  :: Subst -> Reason -> Reason
  -- application of substitution

  deriving Show

withIndent indent s = replicate indent ' ' <> s

prettyConstraint :: RawConstraint -> Reason -> String
prettyConstraint c r = layoutTree 0 (explainConstraint c r)
  where
    layoutTree indent (Node s ts) = intercalate "\n" (s ++ map (layoutTree (indent+2) ts))

explainConstraint :: RawConstraint -> Reason -> Tree [String]
explainConstraint indent c@(ty1 :=: ty2) reason
  = Node (printf "Trying to equate %s and %s" (pretty ty1) (pretty ty2))
    <> (withIndent indent "because ")
    <> prettyReason c reason

prettyReason :: RawConstraint -> Reason -> String
prettyReason _ RUnknown
  = "Unknown reason."
prettyReason (ty1 :=: ty2) (RApp e1 e2) = error "RApp"
  -- = printf "Checking the application of %s (of type %s) to %s (of type %s)."
  --     (pretty e1) (pretty ty1) (pretty e2) (pretty ty2)
prettyReason _ (RCheck e)  = error "RCheck"
prettyReason _ (RSym r)    = error "RSym"
prettyReason (ty1 :=: ty2) (RFunArg c@(fun1 :=: fun2) r) =
  printf "the argument types of %s and %s should match.\n" (pretty fun1) (pretty fun2)
  <> explainConstraint c r
prettyReason _ (RFunRes c r) = error "RFunRes"
prettyReason _ (RPairFst r) = error "RPairFst"
prettyReason _ (RPairSnd r) = error "RPairSnd"
prettyReason c (RSubst s r)
  | isEmptySubst s = prettyReason c r
  | otherwise      = error "RSubst"

data RawConstraint = UType :=: UType
  deriving Show

instance Pretty RawConstraint where
  pretty (ty1 :=: ty2) = printf "%s = %s" (pretty ty1) (pretty ty2)

data Constraint = RawConstraint :? Reason

instance Pretty Constraint where
  pretty (c :? p) = prettyConstraint c p

--------------------------------------------------
-- Type errors

data TypeError where
  UnboundVar   :: String -> TypeError
  Infinite     :: Constraint -> TypeError
  CantUnify    :: Constraint -> TypeError

instance Pretty TypeError where
  pretty (UnboundVar x)
    = printf "Unbound variable %s" x
  pretty (Infinite (c@(x :=: ty) :? r))
    = printf "Infinite type %s = %s (%s)" (pretty x) (pretty ty) (prettyConstraint c r)
  pretty (CantUnify (c@(ty1 :=: ty2) :? r))
    = printf "Can't unify %s and %s\n" (pretty ty1) (pretty ty2)
   <> prettyConstraint c r

--------------------------------------------------
-- Inference algorithm

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
fresh = (TyVar . UVar . head) <$> (nameSupply <%= tail)

withBinding :: String -> UType -> InferM a -> InferM a
withBinding x ty = local (M.insert x ty)

-- For convenience when converting the system without provenance.  We
-- should never have any of these in the finished version.
(=?=) :: UType -> UType -> InferM ()
ty1 =?= ty2 = constraints %= (((ty1 :=: ty2) :? RUnknown) :)

(===) :: UType -> UType -> Reason -> InferM ()
(ty1 === ty2) reason = constraints %= (((ty1 :=: ty2) :? reason) :)

infer :: Expr -> InferM UType
infer (EVar x) = do
  ctx <- ask
  case M.lookup x ctx of
    Just ty -> return ty
    Nothing -> throwError $ UnboundVar x
infer (EInt _)      = return TyInt
infer (EBin _ e1 e2) = do
  check e1 TyInt
  check e2 TyInt
  return TyInt
infer (EApp e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2

  argTy <- fresh
  resTy <- fresh
  (ty1 === TyFun argTy resTy) (RFun e1 e2)
  (ty2 === argTy)             (RApp e1 e2)
  return resTy
infer (ELam x margTy body) = do
  argTy <- case margTy of
    Nothing -> fresh
    Just ty -> return (embed ty)
  withBinding x argTy $ do
    resTy <- infer body
    return $ TyFun argTy resTy

infer (EPair e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  return (TyPair ty1 ty2)

infer EFst = do
  ty1 <- fresh
  ty2 <- fresh
  return (TyFun (TyPair ty1 ty2) ty1)
infer ESnd = do
  ty1 <- fresh
  ty2 <- fresh
  return (TyFun (TyPair ty1 ty2) ty2)

check :: Expr -> UType -> InferM ()
check e ty = do
  ty' <- infer e
  (ty' === ty) (RCheck e)

--------------------------------------------------
-- Unification/constraint solving

solve :: [Constraint] -> Except TypeError Subst
solve []     = return idSubst
solve (c:cs) = do
  u <- solveOne c
  case u of
    Left sub    -> (.@ sub) <$> solve (substConstraints sub cs)
    Right newCs -> solve (newCs ++ cs)

occurs :: UVar -> UType -> Bool
occurs x (TyVar t)      = x == t
occurs _ TyInt          = False
occurs x (TyFun t1 t2)  = occurs x t1 || occurs x t2
occurs x (TyPair t1 t2) = occurs x t1 || occurs x t2

solveOne :: Constraint -> Except TypeError (Either Subst [Constraint])
solveOne ((ty1 :=: ty2) :? _)
  | ty1 == ty2 = return $ Left idSubst
solveOne c@((TyVar x :=: ty2) :? p)
  | occurs x ty2 = throwError $ Infinite c
  | otherwise    = return $ Left (x |-> (ty2, p))
solveOne ((ty1 :=: x@(TyVar _)) :? p)
  = solveOne ((x :=: ty1) :? RSym p)
solveOne (c@(TyFun ty11 ty12 :=: TyFun ty21 ty22) :? p)
  = return $ Right
      [ (ty11 :=: ty21) :? RFunArg c p
      , (ty12 :=: ty22) :? RFunRes c p
      ]
solveOne ((TyPair ty11 ty12 :=: TyPair ty21 ty22) :? p)
  = return $ Right
      [ (ty11 :=: ty21) :? RPairFst p
      , (ty12 :=: ty22) :? RPairSnd p
      ]
solveOne c =
  throwError $ CantUnify c

resolveUTy :: UType -> Type
resolveUTy (TyVar _)      = TyInt
resolveUTy TyInt          = TyInt
resolveUTy (TyFun u1 u2)  = TyFun (resolveUTy u1) (resolveUTy u2)
resolveUTy (TyPair u1 u2) = TyPair (resolveUTy u1) (resolveUTy u2)

--------------------------------------------------
-- Top-level type reconstruction algorithm

recon :: Expr -> Except TypeError Type
recon e = do
  (uty, cs) <- runInferM (infer e)
  sub <- solve cs
  return $ resolveUTy (applySubst sub uty)

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

instance Pretty Value where
  pretty (VInt n) = show n
  pretty (VClosure env x body)
    = printf "<%s: %s %s>"
      x (pretty body) (pretty env)
  pretty (VPair v1 v2)
    = printf "<%s, %s>" (pretty v1) (pretty v2)
  pretty VFst = "fst"
  pretty VSnd = "snd"

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _   (EInt n) = VInt n
interp' env (EBin op ea eb)   =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (interpOp op va vb)
    _ -> error "Impossible! interp' EBin on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case interp' env fun of
    VFst ->
      case interp' env arg of
        VPair v1 _ -> v1
        _ -> error "Impossible! VFst applied to non-pair."
    VSnd ->
      case interp' env arg of
        VPair _ v2 -> v2
        _ -> error "Impossible! VSnd applied to non-pair."
    VClosure env' x body ->
      interp' (M.insert x (interp' env arg) env') body
    _ -> error "Impossible! interp' EApp on non-closure"
interp' env (EPair e1 e2) = VPair (interp' env e1) (interp' env e2)
interp' _ EFst = VFst
interp' _ ESnd = VSnd

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Plus  = (+)
interpOp Minus = (-)
interpOp Times = (*)

eval :: String -> IO ()
eval s = case parse expr s of
  Left err -> print err
  Right e -> case runExcept (recon e) of
    Left tyerr -> putStrLn $ pretty tyerr
    Right ty   -> do
      putStrLn $ pretty e ++ " : " ++ pretty ty
      putStrLn $ pretty (interp e)

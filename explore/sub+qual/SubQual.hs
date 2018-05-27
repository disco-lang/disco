{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}

-- 26 May 2018.  Study for disco type system.

-- STLC extended with arithmetic, subtyping, and type qualifiers; a
-- bidirectional checker with constraint generation.

import           Prelude hiding (lookup)

import           Data.Coerce
import           Data.List (intercalate)
import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)
import           System.IO

import           Control.Monad.Reader
import           Control.Monad.Except

import           Data.Char    (toLower)
import           Data.Maybe   (fromJust)
import           Data.Map     (Map, (!))
import qualified Data.Map     as M
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Text.Printf

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
  CAnd  :: [Constraint] -> Constraint
  CTrue :: Constraint
  CImpl :: Delta -> Constraint -> Constraint
  CAll  :: Bind [Name Type] Constraint -> Constraint

  deriving (Show, Generic)

instance Alpha Qual
instance Alpha QType
instance Alpha Constraint

deltaToConstraint :: Delta -> Constraint
deltaToConstraint = cAnd . map CQual

cAnd :: [Constraint] -> Constraint
cAnd cs = case filter nontrivial cs of
  []   -> CTrue
  [c]  -> c
  cs'  -> CAnd cs'
  where
    nontrivial CTrue = False
    nontrivial _     = True

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

newtype Sigma = Forall (Bind [Name Type] (Delta, Type))
  deriving (Show, Generic)

instance Alpha Sigma

toSigma :: Type -> Sigma
toSigma ty = Forall (bind [] ([], ty))

------------------------------------------------------------
-- Terms
------------------------------------------------------------

data Const = Plus | Times | Minus
  deriving (Show, Eq, Ord, Generic)

gamma0 :: Map Const Sigma
gamma0 = M.fromList
  [ (Plus,  ty "forall a [num a]. a -> a -> a")
  , (Times, ty "forall a [num a]. a -> a -> a")
  , (Minus, ty "forall a [sub a]. a -> a -> a")
  ]

data Expr where
  EVar   :: Name Expr -> Expr
  ENat   :: Integer -> Expr
  EConst :: Const -> Expr
  ELam   :: Bind (Name Expr) Expr -> Expr
  EApp   :: Expr -> Expr -> Expr
  EAnnot :: Expr -> Sigma -> Expr
  ELet   :: Expr -> Sigma -> Bind (Name Expr) Expr -> Expr

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
  <|> ENat  <$> natural
  <|> eLam  <$> (symbol "\\" *> identifier)
            <*> (symbol "." *> parseExpr)
  <|> eLet  <$> (reserved "let" *> identifier)
            <*> (symbol ":" *> parseSigma)
            <*> (symbol "=" *> parseExpr)
            <*> (reserved "in" *> parseExpr)
  <|> parens parseExpr
  where
    eLam x e = ELam (bind x e)
    eLet x ty e1 e2 = ELet e1 ty (bind x e2)

parseExpr :: Parser Expr
parseExpr = ascribe <$> parseExpr' <*> optionMaybe (symbol ":" *> parseSigma)
  where
    ascribe t Nothing   = t
    ascribe t (Just ty) = EAnnot t ty


parseExpr' :: Parser Expr
parseExpr' = buildExpressionParser table parseAtom
  where
    table = [ [ Infix  (EApp <$ reservedOp "")   AssocLeft ]
            , [ Infix  (eTimes <$ reservedOp "*") AssocLeft ]
            , [ Infix  (ePlus  <$ reservedOp "+") AssocLeft
              , Infix  (eMinus <$ reservedOp "-") AssocLeft
              ]
            ]
    eTimes e1 e2 = EApp (EApp (EConst Times) e1) e2
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
         <|> toSigma <$> parseType

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

ty :: String -> Sigma
ty s = case parse (whiteSpace *> parseSigma <* eof) s of
  Left err -> error (show err)
  Right e  -> e

------------------------------------------------------------
-- Typechecking
------------------------------------------------------------

--------------------------------------------------
-- Typechecking monad

type Ctx = Map (Either Const (Name Expr)) Sigma

data TypeError where
  Unbound   :: Name Expr -> TypeError
  CantInfer :: Expr -> TypeError
  Unknown   :: TypeError
  deriving (Show, Generic)

newtype TC a = TC { unTC :: ReaderT Ctx (ExceptT TypeError FreshM) a }
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError TypeError, Fresh)

runTC :: TC a -> Either TypeError a
runTC (TC t) = runFreshM . runExceptT . flip runReaderT (M.mapKeys Left gamma0) $ t

extend :: Name Expr -> Sigma -> TC a -> TC a
extend x s = local (M.insert (Right x) s)

lookup :: Either Const (Name Expr) -> TC Sigma
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> case x of
      Left c  -> error $ "Impossible!  Unknown constant in lookup, " ++ show c
      Right x -> throwError $ Unbound x
    Just ty -> return ty

lookupVar :: Name Expr -> TC Sigma
lookupVar = lookup . Right

lookupConst :: Const -> TC Sigma
lookupConst = lookup . Left

freshTy :: TC Type
freshTy = TyVar <$> fresh (string2Name "a")

--------------------------------------------------
-- Inference mode

infer :: Expr -> TC (Type, Constraint)
infer (ENat _)     = return (TyNat, CTrue)
infer (EConst c)   = inferName (Left c)
infer (EVar x)     = inferName (Right x)
infer (EApp t1 t2) = do
  (tau, c1) <- infer t1
  (tau1, tau2, c2) <- ensureArrow tau
  c3 <- check t2 tau1
  return (tau2, cAnd [c1, c2, c3])
infer (EAnnot e sig) = do
  c1 <- checkSigma e sig
  (tau, c2) <- inferSubsumption sig
  return (tau, cAnd [c1,c2])
infer (ELet e1 sig b) = do
  c1 <- checkSigma e1 sig
  (x,body) <- unbind b
  (tau, c2) <- extend x sig $ infer body
  return (tau, cAnd [c1, c2])
infer e = throwError (CantInfer e)

inferName :: Either Const (Name Expr) -> TC (Type, Constraint)
inferName nm = do
  sig <- lookup nm
  inferSubsumption sig

ensureArrow :: Type -> TC (Type, Type, Constraint)
ensureArrow (TyFun tau1 tau2) = return (tau1, tau2, CTrue)
ensureArrow tau               = do
  tau1 <- freshTy
  tau2 <- freshTy
  return (tau1, tau2, CEq tau (TyFun tau1 tau2))

inferSubsumption :: Sigma -> TC (Type, Constraint)
inferSubsumption (Forall sig) = do
  (as, (delta, tau)) <- unbind sig
  return (tau, deltaToConstraint delta)

--------------------------------------------------
-- Checking mode

checkSigma :: Expr -> Sigma -> TC Constraint
checkSigma expr (Forall sig) = do
  (as, (delta, tau)) <- unbind sig
  c <- check expr tau
  return $ case as of
    [] -> c
    _  -> CAll (bind as (CImpl delta c))

check :: Expr -> Type -> TC Constraint
check (EConst c) tau = checkName (Left c) tau
check (EVar x)   tau = checkName (Right x) tau
check (ELam lam) tau = do
  (tau1, tau2, c1) <- ensureArrow tau
  (x, body) <- unbind lam
  c2 <- extend x (toSigma tau1) $ check body tau2
  return $ cAnd [c1, c2]
check (EAnnot e sig) tau = do
  c1 <- checkSigma e sig
  c2 <- checkSubsumption sig tau
  return $ cAnd [c1, c2]
check e tau2 = do
  (tau1, c1) <- infer e
  return $ cAnd [c1, CSub tau1 tau2]

checkName :: Either Const (Name Expr) -> Type -> TC Constraint
checkName nm tau = do
  sig <- lookup nm
  checkSubsumption sig tau

checkSubsumption :: Sigma -> Type -> TC Constraint
checkSubsumption (Forall sig) tau2 = do
  (as, (delta, tau1)) <- unbind sig
  return $ cAnd [CSub tau1 tau2, deltaToConstraint delta]

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

type Env = Map (Name Expr) Value

data Value where
  VInt     :: Integer -> Value
  VClosure :: Bind (Name Expr) Expr -> Env -> Value
  VFun     :: (Value -> Value) -> Value

extendV :: MonadReader Env m => Name Expr -> Value -> m a -> m a
extendV x v = local (M.insert x v)

withEnv :: MonadReader Env m => Env -> m a -> m a
withEnv e = local (const e)

interp :: Expr -> Value
interp = runLFreshM . flip runReaderT M.empty . interp'

interp' :: Expr -> ReaderT Env LFreshM Value
interp' (EVar x)       = (fromJust . M.lookup x) <$> ask
interp' (ENat n)       = return $ VInt n
interp' (EConst c)     = return $ interpConst c
interp' (ELam b)       = VClosure b <$> ask
interp' (EApp fun arg) = do
  vf <- interp' fun
  va <- interp' arg
  case vf of
    VClosure b env ->
      withEnv env $
      lunbind b $ \(x,body) ->
      extendV x va $ do
        interp' body
    VFun f -> return (f va)
    _ -> error $ printf "Impossible! interp' EApp with (%s) (%s)" (show fun) (show arg)
interp' (EAnnot e _) = interp' e
interp' (ELet e1 _ b) = do
  ve1 <- interp' e1
  lunbind b $ \(x,body) ->
    extendV x ve1 $ interp' body

interpConst :: Const -> Value
interpConst Times   = VFun (\(VInt i) -> VFun (\(VInt j) -> VInt (i*j)))
interpConst Plus    = VFun (\(VInt i) -> VFun (\(VInt j) -> VInt (i+j)))
interpConst Minus   = VFun (\(VInt i) -> VFun (\(VInt j) -> VInt (i-j)))

------------------------------------------------------------
-- Pretty-printing
------------------------------------------------------------

type Prec = Int
data Associativity = L | R
  deriving (Show, Eq)

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0 L

  prettyPrec :: Prec -> Associativity -> p -> String
  prettyPrec _ _ = pretty

instance Pretty Value where
  pretty (VInt n) = show n
  pretty (VClosure b env)
    = printf "<%s: %s %s>"
      (show x) (pretty body) (pretty env)
    where
      (x, body) = unsafeUnbind b
  pretty (VFun _) = "<fun>"

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = show x

  prettyPrec _ _ (ENat i) = show i
  prettyPrec p a (EApp (EApp (EConst Times) e1) e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " * " ++ prettyPrec 1 R e2)
  prettyPrec p a (EApp (EApp (EConst Plus) e1) e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " + " ++ prettyPrec 1 R e2)
  prettyPrec p a (EApp (EApp (EConst Minus) e1) e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " - " ++ prettyPrec 1 R e2)

  prettyPrec p _ (ELam b) =
    mparens (p>0) $
      let (x,body) = unsafeUnbind b
      in  ("\\" ++ show x ++ ". " ++ prettyPrec 0 L body)
  prettyPrec p a (EApp e1 e2) =
    mparens (p>3 || (p==3 && a == R)) $
      (prettyPrec 3 L e1 ++ " " ++ prettyPrec 3 R e2)

instance Pretty Type where
  pretty (TyVar x) = show x
  pretty TyNat     = "N"
  pretty TyInt     = "Z"
  pretty (TyFun ty1 ty2) = "(" ++ pretty ty1 ++ " -> " ++ pretty ty2 ++ ")"

instance Pretty Env where
  pretty env = prettyList bindings
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = show x ++ " -> " ++ pretty v

prettyList xs = "[" ++ intercalate ", " xs ++ "]"

instance Pretty Qual where
  pretty = map toLower . tail . show

instance Pretty QType where
  pretty (QType q tau) = pretty q ++ " " ++ pretty tau

instance Pretty Delta where
  pretty = intercalate ", " . map pretty

instance Pretty Constraint where
  pretty (CSub tau1 tau2) = pretty tau1 ++ " <= " ++ pretty tau2
  pretty (CEq  tau1 tau2) = pretty tau1 ++ " == " ++ pretty tau2
  pretty (CQual (QType qual tau)) = pretty qual ++ " " ++ pretty tau
  pretty (CAnd cs) = "(" ++ intercalate " /\\ " (map pretty cs) ++ ")"
  pretty CTrue = "true"
  pretty (CImpl delta c) = "(" ++ pretty delta ++ ") ==> " ++ pretty c
  pretty (CAll b) =
    let (as, c) = unsafeUnbind b in "forall " ++ intercalate " " (map show as) ++ ". " ++ pretty c

------------------------------------------------------------
-- main
------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl

repl :: IO ()
repl = forever $ do
  putStr "> "
  inp <- getLine
  case parse expr inp of
    Left err -> print err
    Right e  -> case runTC (infer e) of
      Left err -> print err
      Right (ty, c) -> do
        putStrLn (pretty ty)
        putStrLn (pretty c)
        putStrLn (pretty (interp e))



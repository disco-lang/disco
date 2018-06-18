{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TupleSections              #-}

-- 29 May 2018.  Study for disco type system.

-- Hindley-Milner extended with arithmetic and subtyping; a
-- bidirectional checker with constraint generation.
--
-- Like sub+qual but without type qualifiers in the concrete syntax;
-- they still have to show up as constraints for the constraint
-- solver.

import           Prelude                                 hiding (lookup)

import           Data.Coerce
import           GHC.Generics                            (Generic)
import           System.IO
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Arrow                           ((&&&))
import           Data.Bifunctor                          (first, second)
import           Data.Char                               (toLower)
import           Data.List                               (intercalate)
import           Data.Map                                (Map, (!))
import qualified Data.Map                                as M
import           Data.Maybe                              (fromJust)
import           Data.Semigroup
import           Data.Set                                (Set)
import qualified Data.Set                                as S
import           Text.Printf

import           Parsing2

------------------------------------------------------------
-- Atomic types
------------------------------------------------------------

data BaseTy where
  Nat :: BaseTy
  Int :: BaseTy
  deriving (Show, Eq, Ord, Generic)

instance Alpha BaseTy

data Var where
  U :: Name Type -> Var   -- unification variable
  S :: Name Type -> Var   -- skolem variable
  deriving (Show, Eq, Ord, Generic)

instance Alpha Var

data Atom where
  AVar  :: Var -> Atom
  ABase :: BaseTy -> Atom
  deriving (Show, Eq, Ord, Generic)

instance Alpha Atom

instance Subst Atom Var
instance Subst Atom BaseTy

-- don't substitute for skolem vars
instance Subst Atom Atom where
  isvar (AVar (U x)) = Just (SubstName (coerce x))
  isvar _            = Nothing

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

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

instance Subst Type Var
instance Subst Type BaseTy
instance Subst Type Atom
instance Subst Type Cons
instance Subst Type Qual
instance Subst Type Type where
  isvar (TyAtom (AVar (U x))) = Just (SubstName x)
  isvar _                     = Nothing

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
pattern TyVar v = TyAtom (AVar (U v))

pattern Skolem :: Name Type -> Type
pattern Skolem v = TyAtom (AVar (S v))

pattern TyNat :: Type
pattern TyNat   = TyAtom (ABase Nat)

pattern TyInt :: Type
pattern TyInt   = TyAtom (ABase Int)

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

-- pattern TyPair :: Type -> Type -> Type
-- pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

data Qual = QAdd | QSub | QMul
  deriving (Show, Eq, Ord, Generic)

data Constraint where
  CSub  :: Type -> Type -> Constraint
  CEq   :: Type -> Type -> Constraint
  CQual :: Qual -> Type -> Constraint
  CAnd  :: [Constraint] -> Constraint
  CTrue :: Constraint
  CAll  :: Bind [Name Type] Constraint -> Constraint

  deriving (Show, Generic)

instance Alpha Qual
instance Alpha Constraint

instance Subst Type Constraint

cAnd :: [Constraint] -> Constraint
cAnd cs = case filter nontrivial cs of
  []  -> CTrue
  [c] -> c
  cs' -> CAnd cs'
  where
    nontrivial CTrue = False
    nontrivial _     = True

------------------------------------------------------------
-- Subtyping and qualifier rules
------------------------------------------------------------

isSubA :: Atom -> Atom -> Bool
isSubA a1 a2                 | a1 == a2 = True
isSubA (ABase t1) (ABase t2) = isSubB t1 t2
isSubA _ _                   = False

isSubB :: BaseTy -> BaseTy -> Bool
isSubB Nat Int = True
isSubB _ _     = False

qualifications :: Map Qual (Set BaseTy)
qualifications = M.fromList $
  [ (QAdd, S.fromList [Nat, Int])
  , (QMul, S.fromList [Nat, Int])
  , (QSub, S.fromList [Int])
  ]

------------------------------------------------------------
-- Polytypes
------------------------------------------------------------

newtype Sigma = Forall (Bind [Name Type] Type)
  deriving (Show, Generic)

instance Alpha Sigma

toSigma :: Type -> Sigma
toSigma ty = Forall (bind [] ty)

------------------------------------------------------------
-- Terms
------------------------------------------------------------

data BOp = Plus | Times | Minus
  deriving (Show, Eq, Ord, Generic)

data Expr where
  EVar   :: Name Expr -> Expr
  ENat   :: Integer -> Expr
  EBin   :: BOp -> Expr -> Expr -> Expr
  ELam   :: Bind (Name Expr) Expr -> Expr
  EApp   :: Expr -> Expr -> Expr
  EAscribe :: Expr -> Sigma -> Expr
  ELet   :: Expr -> Sigma -> Bind (Name Expr) Expr -> Expr

  deriving (Show, Generic)

instance Alpha BOp
instance Alpha Expr

instance Subst Expr BOp where
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
  { reservedNames = ["let", "in", "N", "Nat", "Z", "Int", "forall"]
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
    ascribe t (Just ty) = EAscribe t ty


parseExpr' :: Parser Expr
parseExpr' = buildExpressionParser table parseAtom
  where
    table = [ [ Infix  (EApp <$ reservedOp "")   AssocLeft ]
            , [ Infix  (EBin Times <$ reservedOp "*") AssocLeft ]
            , [ Infix  (EBin Plus  <$ reservedOp "+") AssocLeft
              , Infix  (EBin Minus <$ reservedOp "-") AssocLeft
              ]
            ]

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
                           <*> (symbol "." *> parseType)
                        )
                    )
         <|> toSigma <$> parseType

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

type Ctx = Map (Name Expr) Sigma

data TypeError where
  Unbound   :: Name Expr -> TypeError
  CantInfer :: Expr -> TypeError
  Unknown   :: TypeError
  deriving (Show, Generic)

newtype TC a = TC { unTC :: ReaderT Ctx (ExceptT TypeError FreshM) a }
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError TypeError, Fresh)

runTC :: TC a -> Either TypeError a
runTC (TC t) = runFreshM . runExceptT . flip runReaderT M.empty $ t

extend :: Name Expr -> Sigma -> TC a -> TC a
extend x s = local (M.insert x s)

lookup :: Name Expr -> TC Sigma
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError $ Unbound x
    Just ty -> return ty

freshTy :: TC Type
freshTy = TyVar <$> fresh (string2Name "a")

freshSkolem :: TC Var
freshSkolem = S <$> fresh (string2Name "s")

--------------------------------------------------
-- Inference mode

infer :: Expr -> TC (Type, Constraint)
infer (ENat _)     = return (TyNat, CTrue)
infer (EVar x)     = do
  sig <- lookup x
  tau <- inferSubsumption sig
  return (tau, CTrue)
infer (EBin bop t1 t2) = do
  (tau1, c1) <- infer t1
  (tau2, c2) <- infer t2
  tau3 <- freshTy
  return (tau3, cAnd [c1, c2, cSub tau1 tau3, cSub tau2 tau3, CQual (opQual bop) tau3])
infer (EApp t1 t2) = do
  (tau, c1) <- infer t1
  (tau1, tau2, c2) <- ensureArrow tau
  c3 <- check t2 tau1
  return (tau2, cAnd [c1, c2, c3])
infer (EAscribe e sig) = do
  c1 <- checkSigma e sig
  tau <- inferSubsumption sig
  return (tau, c1)
infer (ELet e1 sig b) = do
  c1 <- checkSigma e1 sig
  (x,body) <- unbind b
  (tau, c2) <- extend x sig $ infer body
  return (tau, cAnd [c1, c2])
infer e = throwError (CantInfer e)

opQual :: BOp -> Qual
opQual Plus  = QAdd
opQual Minus = QSub
opQual Times = QMul

ensureArrow :: Type -> TC (Type, Type, Constraint)
ensureArrow (TyFun tau1 tau2) = return (tau1, tau2, CTrue)
ensureArrow tau               = do
  tau1 <- freshTy
  tau2 <- freshTy
  return (tau1, tau2, CEq tau (TyFun tau1 tau2))

inferSubsumption :: Sigma -> TC Type
inferSubsumption (Forall sig) = do
  (_, tau) <- unbind sig
  return tau

--------------------------------------------------
-- Checking mode

checkSigma :: Expr -> Sigma -> TC Constraint
checkSigma expr (Forall sig) = do
  (as, tau) <- unbind sig
  c <- check expr tau
  return $ case as of
    [] -> c
    _  -> CAll (bind as c)

check :: Expr -> Type -> TC Constraint
check (EVar x)   tau = do
  sig <- lookup x
  checkSubsumption sig tau
check (EBin op t1 t2) tau = do
  c1 <- check t1 tau
  c2 <- check t2 tau
  return $ cAnd [c1, c2, CQual (opQual op) tau]
check (ELam lam) tau = do
  (tau1, tau2, c1) <- ensureArrow tau
  (x, body) <- unbind lam
  c2 <- extend x (toSigma tau1) $ check body tau2
  return $ cAnd [c1, c2]
check (EAscribe e sig) tau = do
  c1 <- checkSigma e sig
  c2 <- checkSubsumption sig tau
  return $ cAnd [c1, c2]
check e tau2 = do
  (tau1, c1) <- infer e
  return $ cAnd [c1, cSub tau1 tau2]

checkSubsumption :: Sigma -> Type -> TC Constraint
checkSubsumption (Forall sig) tau2 = do
  (as, tau1) <- unbind sig
  return $ cSub tau1 tau2

-- Opportunity to get rid of trivial constraints
cSub :: Type -> Type -> Constraint
cSub tau1 tau2
  | isKnownSub tau1 tau2 = CTrue
  | otherwise            = CSub tau1 tau2

isKnownSub tau1 tau2   | tau1 == tau2 = True
isKnownSub TyNat TyInt = True
isKnownSub _ _         = False

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
interp' (EVar x)        = (fromJust . M.lookup x) <$> ask
interp' (ENat n)        = return $ VInt n
interp' (EBin op e1 e2) = do
  v1 <- interp' e1
  v2 <- interp' e2
  return $ interpOp op v1 v2
interp' (ELam b)        = VClosure b <$> ask
interp' (EApp fun arg)  = do
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
interp' (EAscribe e _) = interp' e
interp' (ELet e1 _ b) = do
  ve1 <- interp' e1
  lunbind b $ \(x,body) ->
    extendV x ve1 $ interp' body

interpOp op (VInt i) (VInt j) = VInt (interpVOp op i j)

interpVOp Plus  = (+)
interpVOp Minus = (-)
interpVOp Times = (*)

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
  prettyPrec p a (EBin Times e1 e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " * " ++ prettyPrec 1 R e2)
  prettyPrec p a (EBin Plus e1 e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " + " ++ prettyPrec 1 R e2)
  prettyPrec p a (EBin Minus e1 e2) =
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
  pretty (TyVar x)       = show x
  pretty TyNat           = "N"
  pretty TyInt           = "Z"
  pretty (TyFun ty1 ty2) = "(" ++ pretty ty1 ++ " -> " ++ pretty ty2 ++ ")"

instance Pretty Env where
  pretty env = prettyList bindings
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = show x ++ " -> " ++ pretty v

prettyList xs = "[" ++ intercalate ", " xs ++ "]"

instance Pretty Qual where
  pretty = map toLower . tail . show

instance Pretty Constraint where
  pretty (CSub tau1 tau2) = pretty tau1 ++ " <= " ++ pretty tau2
  pretty (CEq  tau1 tau2) = pretty tau1 ++ " == " ++ pretty tau2
  pretty (CQual qual tau) = pretty qual ++ " " ++ pretty tau
  pretty (CAnd cs) = "(" ++ intercalate " /\\ " (map pretty cs) ++ ")"
  pretty CTrue = "true"
  pretty (CAll b) =
    let (as, c) = unsafeUnbind b in "forall " ++ intercalate " " (map show as) ++ ". " ++ pretty c

------------------------------------------------------------
-- Constraint solving
------------------------------------------------------------

--------------------------------------------------
-- Substitutions

-- This is an inefficient representation of substitutions; and ideally
-- we would implement constraint solving without using explicit
-- substitutions at all.  But this will do for now.

type S' a = [(Name a, a)]

idS :: S' a
idS = []

-- Construct a singleton substitution
(|->) :: Name a -> a -> S' a
x |-> t = [(x,t)]

-- Substitution composition
(@@) :: Subst a a => S' a -> S' a -> S' a
s1 @@ s2 = (map . second) (substs s1) s2 ++ s1

-- Compose a whole bunch of substitutions
compose :: (Subst a a, Foldable t) => t (S' a) -> S' a
compose = foldr (@@) idS

type S = S' Type

--------------------------------------------------
-- Solver errors

-- | Type of errors which can be generated by the constraint solving
--   process.
data SolveError where
  NoWeakUnifier :: SolveError
  NoUnify       :: SolveError
  UnqualBase    :: Qual -> BaseTy    -> SolveError
  Unqual        :: Qual -> Type      -> SolveError
  QualSkolem    :: Qual -> Name Type -> SolveError
  deriving Show

-- | Convert 'Nothing' into the given error.
maybeError :: e -> Maybe a -> Except e a
maybeError e Nothing  = throwError e
maybeError _ (Just a) = return a

--------------------------------------------------
-- Solver monad

type SolveM a = ExceptT SolveError FreshM a

--------------------------------------------------
-- Simple constraints and qualifier maps

data SimpleConstraint where
  (:<:) :: Type -> Type -> SimpleConstraint
  (:=:) :: Type -> Type -> SimpleConstraint

newtype QualMap = QM { unQM :: Map (Name Type) (Set Qual) }

instance Semigroup QualMap where
  QM qm1 <> QM qm2 = QM (M.unionWith (<>) qm1 qm2)

instance Monoid QualMap where
  mempty  = QM M.empty
  mappend = (<>)

--------------------------------------------------
-- Top-level solver algorithm

solveConstraint :: Constraint -> SolveM S
solveConstraint c = do

  -- Step 1. Open foralls (instantiating with skolem variables) and
  -- collect wanted qualifiers.  Should result in just a list of
  -- equational and subtyping constraints in addition to qualifiers.

  (quals, cs) <- decomposeConstraint c

  return undefined

-- Step 2. Check for weak unification to ensure termination. (a la
-- Traytel et al).

-- Step 3. Simplify constraints, resulting in a set of atomic
-- subtyping constraints.  Also simplify/update qualifier set
-- accordingly.

-- Step 4. Turn the atomic constraints into a directed constraint
-- graph.

-- Step 5. Check for any weakly connected components containing more
-- than one skolem, or a skolem and a base type; such components are
-- not allowed.

-- Step 6. Eliminate cycles.

-- Step 7. Solve the graph.

-- Step 8. Unify any remaining variables in weakly connected
-- components.

--------------------------------------------------
-- Step 1. Constraint decomposition.

decomposeConstraint :: Constraint -> SolveM (QualMap, [SimpleConstraint])
decomposeConstraint (CSub t1 t2) = return (mempty, [t1 :<: t2])
decomposeConstraint (CEq  t1 t2) = return (mempty, [t1 :=: t2])
decomposeConstraint (CQual q ty) = (, []) <$> decomposeQual q ty
decomposeConstraint (CAnd cs)    = mconcat <$> mapM decomposeConstraint cs
decomposeConstraint CTrue        = return mempty
decomposeConstraint (CAll ty)    = do
  (vars, c) <- unbind ty
  let c' = substs (mkSkolems vars) c
  decomposeConstraint c'

  where
    mkSkolems :: [Name Type] -> [(Name Type, Type)]
    mkSkolems = map (id &&& Skolem)

decomposeQual :: Qual -> Type -> SolveM QualMap
decomposeQual q (TyAtom a)        = checkQual q a
decomposeQual q ty@(TyCons c tys) = throwError $ Unqual q ty
  -- Right now, no structured type (functions) can satisfy a
  -- qualifier.  In a bigger system this would be different: this
  -- function would decompose a qualifier on a structured type into
  -- appropriate qualifiers on its component types.  Once the rest is
  -- working we could e.g. add product types and specify that numeric
  -- operations lift over pairs componentwise.

checkQual :: Qual -> Atom -> SolveM QualMap
checkQual q (AVar (U v)) = return . QM $ M.singleton v (S.singleton q)
checkQual q (AVar (S v)) = throwError $ QualSkolem q v
checkQual q (ABase bty)  =
  case bty `S.member` (qualifications ! q) of
    True  -> return mempty
    False -> throwError $ UnqualBase q bty

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

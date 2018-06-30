{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- 29 May 2018.  Study for disco type system.

-- Hindley-Milner extended with arithmetic and subtyping; a
-- bidirectional checker with constraint generation.
--
-- Like sub+qual but without type qualifiers in the concrete syntax;
-- they still have to show up as constraints for the constraint
-- solver.

import           Prelude                                 hiding (lookup)
import qualified Prelude                                 as P

import           Data.Coerce
import           GHC.Generics                            (Generic)
import           System.IO
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           System.Console.Haskeline                as H
import           System.Exit

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Arrow                           ((&&&), (***))
import           Control.Lens                            (anyOf, both, each,
                                                          makeLenses, over,
                                                          toListOf, use, (%=),
                                                          (.=), (^..))
import           Data.Bifunctor                          (first, second)
import           Data.Char                               (toLower)
import           Data.Either                             (isRight,
                                                          partitionEithers)
import           Data.Function                           ((&))
import           Data.List                               (find, foldl',
                                                          intercalate,
                                                          intersect, partition)
import           Data.Map                                (Map, (!))
import qualified Data.Map                                as M
import           Data.Maybe                              (catMaybes, fromJust,
                                                          fromMaybe)
import           Data.Semigroup
import           Data.Set                                (Set)
import qualified Data.Set                                as S
import           Data.Tuple                              (swap)
import           Graph                                   (Graph)
import qualified Graph                                   as G
import           Text.Printf

import           Parsing2

import           Debug.Trace

------------------------------------------------------------
-- Atomic types
------------------------------------------------------------

data BaseTy where
  Nat  :: BaseTy
  Int  :: BaseTy
  Bool :: BaseTy
  deriving (Show, Eq, Ord, Generic)

instance Alpha BaseTy

instance Subst BaseTy BaseTy

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

type UAtom = Either BaseTy (Name Type)  -- unifiable atoms, i.e. no skolems

uatomToAtom :: UAtom -> Atom
uatomToAtom (Left b)  = ABase b
uatomToAtom (Right x) = AVar (U x)

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

isSkolem :: Atom -> Bool
isSkolem (AVar (S _)) = True
isSkolem _            = False

------------------------------------------------------------
-- Type structure
------------------------------------------------------------

data Con where
  CArr  :: Con
  CPair :: Con
  deriving (Show, Eq, Ord, Generic)

instance Alpha Con

data Variance = Co | Contra  -- could add 'In' as well

arity :: Con -> [Variance]
arity CArr  = [Contra, Co]
arity CPair = [Co, Co]

------------------------------------------------------------
-- Monotypes
------------------------------------------------------------

data Type where
  TyAtom :: Atom -> Type
  TyCon  :: Con -> [Type] -> Type
  deriving (Show, Eq, Ord, Generic)

instance Alpha Type

instance Subst Type Var
instance Subst Type BaseTy
instance Subst Type Atom
instance Subst Type Con
instance Subst Type Qual
instance Subst Type SimpleConstraint
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

nameToTypeSubst :: S' (Name Type) -> S' Type
nameToTypeSubst = map (coerce *** TyVar)

atomToTypeSubst :: S' Atom -> S' Type
atomToTypeSubst = map (coerce *** TyAtom)

uatomToTypeSubst :: S' UAtom -> S' Type
uatomToTypeSubst = atomToTypeSubst . map (coerce *** uatomToAtom)

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

pattern TyBool :: Type
pattern TyBool  = TyAtom (ABase Bool)

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCon CArr [ty1, ty2]

pattern TyPair :: Type -> Type -> Type
pattern TyPair ty1 ty2 = TyCon CPair [ty1, ty2]

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

-- | A 'Qual' is a type qualifier.  It represents a particular set of
--   types which have some property or support some operation.
data Qual = QAdd | QSub | QMul
  deriving (Show, Eq, Ord, Generic)

-- | A 'Sort' represents a set of qualifiers, and also represents a
--   set of types (in general, the intersection of the sets
--   corresponding to the qualifiers).
type Sort = Set Qual

-- | The special sort "top" which includes all types.
topSort :: Sort
topSort = S.empty

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
-- Subtyping and qualifier/sort rules
------------------------------------------------------------

isSubA :: Atom -> Atom -> Bool
isSubA a1 a2                 | a1 == a2 = True
isSubA (ABase t1) (ABase t2) = isSubB t1 t2
isSubA _ _                   = False

isSubB :: BaseTy -> BaseTy -> Bool
isSubB b1 b2   | b1 == b2 = True
isSubB Nat Int = True
isSubB _ _     = False

isDirB :: Dir -> BaseTy -> BaseTy -> Bool
isDirB Sub   b1 b2 = isSubB b1 b2
isDirB Super b1 b2 = isSubB b2 b1

qualifications :: Map Qual (Set BaseTy)
qualifications = M.fromList $
  [ (QAdd, S.fromList [Nat, Int])
  , (QMul, S.fromList [Nat, Int])
  , (QSub, S.fromList [Int])
  ]

hasQual :: BaseTy -> Qual -> Bool
hasQual b q = maybe False (S.member b) (M.lookup q qualifications)

hasSort :: BaseTy -> Sort -> Bool
hasSort = all . hasQual

(==>) :: a -> b -> (a,b)
(==>) = (,)

-- (c, (q, qs)) means that  TyCon c t1 t2 ... tn  is logically equivalent to
-- q1 t1 /\ q2 t2 /\ ... /\ qn tn.
--
-- Note in Disco I think we can get away with any given qualifier
-- requiring *at most one* qualifier on each type argument.  Then we
-- can derive the sortRules by combining qualRules.  In general,
-- however, you could imagine some particular qualifier requiring a
-- set of qualifiers (i.e. a sort) on a type argument.  Then we would
-- just have to encode sortRules directly.
qualRules :: Map Con (Map Qual [Maybe Qual])
qualRules = M.fromList
  [ CPair ==> M.fromList [ QAdd ==> [Just QAdd, Just QAdd]
                         , QSub ==> [Just QSub, Just QSub]
                         , QMul ==> [Just QMul, Nothing]   -- Just to make things interesting,
                                                           -- <x,y> * <u,v> = <x*u, y>
                         ]
  ]

-- sortRules T s = [s1, ..., sn] means that sort s holds of
-- type (T t1 ... tn) if and only if  s1 t1 /\ ... /\ sn tn.
-- For now this is just derived directly from qualRules.
--
-- This is the 'arity' function described in section 4.1 of Traytel et
-- al.
sortRules :: Con -> Sort -> Maybe [Sort]
sortRules c s = do
  -- If tycon c is not in the qualRules map, there's no way to make it
  -- an instance of any sort, so fail
  qualMap   <- M.lookup c qualRules

  -- If any of the quals q in sort s are not in the map corresponding
  -- to tycon c, there's no way to make c an instance of q, so fail
  -- (the mapM will succeed only if all lookups succeed)
  needQuals <- mapM (flip M.lookup qualMap) (S.toList s)

  -- Otherwise we are left with a list (corresponding to all the quals
  -- in sort s) of lists (each one corresponds to the type args of c).
  -- We zip them together to produce a list of sorts.
  return $ foldl' (zipWith (\srt -> maybe srt (`S.insert` srt))) (repeat topSort) needQuals

data Dir = Sub | Super
  deriving (Eq, Ord, Read, Show)

other :: Dir -> Dir
other Sub   = Super
other Super = Sub

supertypes :: BaseTy -> [BaseTy]
supertypes Nat = [Nat, Int]
supertypes b   = [b]

subtypes :: BaseTy -> [BaseTy]
subtypes Int = [Nat, Int]
subtypes b   = [b]

dirtypes :: Dir -> BaseTy -> [BaseTy]
dirtypes Sub   = subtypes
dirtypes Super = supertypes

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
  EBool  :: Bool -> Expr
  EBin   :: BOp -> Expr -> Expr -> Expr
  ELam   :: Bind (Name Expr) Expr -> Expr
  EApp   :: Expr -> Expr -> Expr
  EPair  :: Expr -> Expr -> Expr
  EAscribe :: Expr -> Sigma -> Expr
  ELet   :: Expr -> Maybe Sigma -> Bind (Name Expr) Expr -> Expr

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
  { reservedNames = [ "let", "in", "N", "Nat", "Z", "Int", "forall"
                    , "true", "false"
                    ]
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
  <|> EBool <$> (True <$ reserved "true" <|> False <$ reserved "false")
  <|> eLam  <$> (symbol "\\" *> identifier)
            <*> (symbol "." *> parseExpr)
  <|> EPair <$> (symbol "<" *> parseExpr)
            <*> (symbol "," *> parseExpr <* symbol ">")
  <|> eLet  <$> (reserved "let" *> identifier)
            <*> optionMaybe (symbol ":" *> parseSigma)
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
    table = [ [ Infix  (EApp <$ reservedOp "")   AssocLeft
              ]
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
    table = [ [ Infix (TyPair <$ reservedOp "*")  AssocRight ]
            , [ Infix (TyFun  <$ reservedOp "->") AssocRight ]
            ]

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
  Mismatch  :: Con -> Type -> TypeError
  Unknown   :: TypeError
  deriving (Show, Generic)

newtype TC a = TC { unTC :: ReaderT Ctx (ExceptT TypeError FreshM) a }
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError TypeError, Fresh)

gamma0 :: Ctx
gamma0 = M.fromList
  [ (string2Name "fst", ty "forall a b. a*b -> a")
  , (string2Name "snd", ty "forall a b. a*b -> b")
  ]

runTC :: TC a -> Either TypeError a
runTC (TC t) = runFreshM . runExceptT . flip runReaderT gamma0 $ t

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
infer (EBool _)    = return (TyBool, CTrue)
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
  ([tau1, tau2], c2) <- ensure CArr tau
  c3 <- check t2 tau1
  return (tau2, cAnd [c1, c2, c3])
infer (EPair t1 t2) = do
  (tau1, c1) <- infer t1
  (tau2, c2) <- infer t2
  return (TyPair tau1 tau2, cAnd [c1, c2])

infer (EAscribe e sig) = do
  c1 <- checkSigma e sig
  tau <- inferSubsumption sig
  return (tau, c1)
infer (ELet e1 msig b) = do
  case msig of
    Nothing -> do
      (tau1, c1) <- infer e1
      (x, body)  <- unbind b
      (tau, c2)  <- extend x (toSigma tau1) $ infer body
      return (tau, cAnd [c1, c2])
    Just sig -> do
      c1 <- checkSigma e1 sig
      (x,body) <- unbind b
      (tau, c2) <- extend x sig $ infer body
      return (tau, cAnd [c1, c2])
infer e = throwError (CantInfer e)

opQual :: BOp -> Qual
opQual Plus  = QAdd
opQual Minus = QSub
opQual Times = QMul

ensure :: Con -> Type -> TC ([Type], Constraint)
ensure c ty@(TyCon d args)
  | c == d    = return (args, CTrue)
  | otherwise = throwError $ Mismatch c ty
ensure c tau = do
  vars <- mapM (const freshTy) (arity c)
  return (vars, CEq tau (TyCon c vars))

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
  ([tau1, tau2], c1) <- ensure CArr tau
  (x, body) <- unbind lam
  c2 <- extend x (toSigma tau1) $ check body tau2
  return $ cAnd [c1, c2]
check (EPair t1 t2) tau = do
  ([tau1, tau2], c) <- ensure CPair tau
  c1 <- check t1 tau1
  c2 <- check t2 tau2
  return $ cAnd [c, c1, c2]
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
  VBool    :: Bool -> Value
  VPair    :: Value -> Value -> Value
  VClosure :: Bind (Name Expr) Expr -> Env -> Value
  VFun     :: (Value -> Value) -> Value

extendV :: MonadReader Env m => Name Expr -> Value -> m a -> m a
extendV x v = local (M.insert x v)

withEnv :: MonadReader Env m => Env -> m a -> m a
withEnv e = local (const e)

rho0 :: Env
rho0 = M.fromList
  [ (string2Name "fst", VFun $ \(VPair v1 _) -> v1)
  , (string2Name "snd", VFun $ \(VPair _ v2) -> v2)
  ]

interp :: Expr -> Value
interp = runLFreshM . flip runReaderT rho0 . interp'

interp' :: Expr -> ReaderT Env LFreshM Value
interp' (EVar x)        = (fromJust . M.lookup x) <$> ask
interp' (ENat n)        = return $ VInt n
interp' (EBool b)       = return $ VBool b
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
interp' (EPair e1 e2)  = VPair <$> interp' e1 <*> interp' e2

interp' (EAscribe e _) = interp' e
interp' (ELet e1 _ b) = do
  ve1 <- interp' e1
  lunbind b $ \(x,body) ->
    extendV x ve1 $ interp' body

interpOp op (VInt i) (VInt j) = VInt (interpVOp op i j)
interpOp Times (VPair v11 v12) (VPair v21 _)
  = VPair (interpOp Times v11 v21) v12
interpOp op (VPair v11 v12) (VPair v21 v22)
  = VPair (interpOp op v11 v21) (interpOp op v12 v22)

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
  pretty (VBool b) = map toLower (show b)
  pretty (VPair v1 v2) = printf "<%s, %s>" (pretty v1) (pretty v2)
  pretty (VClosure b env)
    = printf "<%s: %s %s>"
      (show x) (pretty body) (pretty env)
    where
      (x, body) = unsafeUnbind b
  pretty (VFun _) = "<fun>"

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = show x

  prettyPrec _ _ (ENat i)  = show i
  prettyPrec _ _ (EBool b) = map toLower (show b)
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

  prettyPrec _ _ (EPair e1 e2) =
    "<" ++ prettyPrec 0 L e1 ++ ", " ++ prettyPrec 0 L e2 ++ ">"

-- infixr -> 1
-- infixr *  3
instance Pretty Type where
  prettyPrec _ _ (TyAtom a)      = pretty a
  prettyPrec p a (TyFun ty1 ty2) =
    mparens (p > 1 || p==1 && a == L) $
      prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 1 R ty2
  prettyPrec p a (TyPair ty1 ty2) =
    mparens (p > 3 || p==3 && a == L) $
      prettyPrec 3 L ty1 ++ " * " ++ prettyPrec 3 R ty2

instance Pretty Atom where
  pretty (AVar v)  = pretty v
  pretty (ABase b) = pretty b

instance Pretty Var where
  pretty (U v) = show v
  pretty (S v) = "%" ++ show v

instance Pretty BaseTy where
  pretty Nat  = "N"
  pretty Int  = "Z"
  pretty Bool = "B"

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

instance Pretty SortMap where
  pretty (SM quals) = "SM " ++ pretty (M.assocs quals)

-- instance Pretty a => Pretty (S' a) where
--   pretty =

instance Pretty (Name a) where
  pretty = show

instance Pretty SimpleConstraint where
  pretty (t1 :<: t2) = pretty t1 ++ " < " ++ pretty t2
  pretty (t1 :=: t2) = pretty t1 ++ " = " ++ pretty t2

instance Pretty a => Pretty [a] where
  pretty = prettyList . map pretty

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x,y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance Pretty a => Pretty (Set a) where
  pretty = ("{"++) . (++"}") . intercalate ", " . map pretty . S.toList

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

dom :: S' a -> [Name a]
dom = map fst

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
-- Unification

-- XXX todo: might be better if unification took sorts into account
-- directly.  As it is, however, I think it works properly;
-- e.g. suppose we have a with sort {sub} and we unify it with Bool.
-- unify will just return a substitution [a |-> Bool].  But then when
-- we call extendSubst, and in particular substSortMap, the sort {sub}
-- will be applied to Bool and decomposed which will throw an error.

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible).
--
--   This is not the most efficient way to implement unification but
--   it is simple.
unify :: [(Type, Type)] -> Maybe S
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving.
weakUnify :: [(Type, Type)] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' :: (BaseTy -> BaseTy -> Bool) -> [(Type, Type)] -> Maybe S
unify' _ [] = Just idS
unify' baseEq (e:es) = do
  u <- unifyOne baseEq e
  case u of
    Left sub    -> (@@ sub) <$> unify' baseEq (substs sub es)
    Right newEs -> unify' baseEq (newEs ++ es)

equate :: [Type] -> Maybe S
equate tys = unify eqns
  where
    eqns = zipWith (,) tys (tail tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)

unifyOne :: (BaseTy -> BaseTy -> Bool) -> (Type, Type) -> Maybe (Either S [(Type, Type)])

unifyOne _ (ty1, ty2)
  | ty1 == ty2 = return $ Left idS

unifyOne _ (TyVar x, ty2)
  | occurs x ty2 = Nothing
  | otherwise    = Just $ Left (x |-> ty2)
unifyOne baseEq (ty1, x@(TyVar _))
  = unifyOne baseEq (x, ty1)

-- At this point we know ty2 isn't the same skolem nor a unification variable.
-- Skolems don't unify with anything.
unifyOne _ (Skolem _, _) = Nothing
unifyOne _ (_, Skolem _) = Nothing

unifyOne _ (TyCon c1 tys1, TyCon c2 tys2)
  | c1 == c2  = return $ Right (zipWith (,) tys1 tys2)
  | otherwise = Nothing
unifyOne baseEq (TyAtom (ABase b1), TyAtom (ABase b2))
  | baseEq b1 b2 = return $ Left idS
  | otherwise    = Nothing
unifyOne _ _ = Nothing  -- Atom = Cons

unifyAtoms :: [Atom] -> Maybe (S' Atom)
unifyAtoms = fmap convert . equate . map TyAtom
  where
    -- Guaranteed that this will get everything in the list, since we
    -- started with all atoms.
    convert s = [(coerce x, a) | (x, TyAtom a) <- s]

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
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e Nothing  = throwError e
maybeError _ (Just a) = return a

--------------------------------------------------
-- Solver monad

type SolveM a = FreshMT (Except SolveError) a

runSolveM :: SolveM a -> Either SolveError a
runSolveM = runExcept . runFreshMT

liftExcept :: MonadError e m => Except e a -> m a
liftExcept = either throwError return . runExcept

--------------------------------------------------
-- Simple constraints and qualifier maps

data SimpleConstraint where
  (:<:) :: Type -> Type -> SimpleConstraint
  (:=:) :: Type -> Type -> SimpleConstraint
  deriving (Show, Generic)

instance Alpha SimpleConstraint

newtype SortMap = SM { unSM :: Map (Name Type) Sort }
  deriving (Show)

instance Semigroup SortMap where
  SM sm1 <> SM sm2 = SM (M.unionWith (<>) sm1 sm2)

instance Monoid SortMap where
  mempty  = SM M.empty
  mappend = (<>)

getSort :: SortMap -> Name Type -> Sort
getSort (SM sm) v = fromMaybe topSort (M.lookup v sm)

--------------------------------------------------
-- Simplifier types

-- Uses TH to generate lenses so it has to go here before other stuff.

-- The simplification stage maintains a mutable state consisting of
-- the current qualifier map (containing wanted qualifiers for type variables),
-- the list of remaining SimpleConstraints, and the current substitution.
data SimplifyState = SS
  { _ssSortMap     :: SortMap
  , _ssConstraints :: [SimpleConstraint]
  , _ssSubst       :: S
  }

makeLenses ''SimplifyState

lkup :: (Ord k, Show k, Show (Map k a)) => String -> Map k a -> k -> a
lkup msg m k = fromMaybe (error errMsg) (M.lookup k m)
  where
    errMsg = unlines
      [ "Key lookup error:"
      , "  Key = " ++ show k
      , "  Map = " ++ show m
      , "  Location: " ++ msg
      ]

--------------------------------------------------
-- Top-level solver algorithm

solveConstraint :: Constraint -> SolveM S
solveConstraint c = do

  -- Step 1. Open foralls (instantiating with skolem variables) and
  -- collect wanted qualifiers.  Should result in just a list of
  -- equational and subtyping constraints in addition to qualifiers.

  traceM "------------------------------"
  traceM "Decomposing constraints..."

  (quals, cs) <- decomposeConstraint c

  traceM (pretty quals)
  traceM (pretty cs)

  -- Step 2. Check for weak unification to ensure termination. (a la
  -- Traytel et al).

  let toEqn (t1 :<: t2) = (t1,t2)
      toEqn (t1 :=: t2) = (t1,t2)
  maybeError NoWeakUnifier $ weakUnify (map toEqn cs)

  -- Step 3. Simplify constraints, resulting in a set of atomic
  -- subtyping constraints.  Also simplify/update qualifier set
  -- accordingly.

  traceM "------------------------------"
  traceM "Running simplifier..."

  (sm, atoms, theta_simp) <- liftExcept (simplify quals cs)

  traceM (pretty sm)
  traceM (pretty atoms)
  traceM (pretty theta_simp)

  -- Step 4. Turn the atomic constraints into a directed constraint
  -- graph.

  traceM "------------------------------"
  traceM "Generating constraint graph..."
  let g = mkConstraintGraph atoms

  traceShowM g

  -- Step 5.
  -- Check for any weakly connected components containing more
  -- than one skolem, or a skolem and a base type; such components are
  -- not allowed.  Other WCCs with a single skolem simply unify to
  -- that skolem.

  traceM "------------------------------"
  traceM "Checking WCCs for skolems..."

  (g', theta_skolem) <- liftExcept (checkSkolems sm g)

  -- Step 6. Eliminate cycles from the graph, turning each strongly
  -- connected component into a single node, unifying all the atoms in
  -- each component.

  traceM "------------------------------"
  traceM "Collapsing SCCs..."

  (g'', theta_cyc) <- liftExcept (elimCycles g')

  traceShowM g''

  -- Steps 7 & 8: solve the graph, iteratively finding satisfying
  -- assignments for each type variable based on its successor and
  -- predecessor base types in the graph; then unify all the type
  -- variables in any remaining weakly connected components.

  traceM "------------------------------"
  traceM "Solving for type variables..."

  theta_sol       <- solveGraph sm g''

  traceM "------------------------------"
  traceM "Composing final substitution..."

  let theta_final = (theta_sol @@ theta_cyc @@ theta_skolem @@ theta_simp)

  return theta_final


--------------------------------------------------
-- Step 1. Constraint decomposition.

decomposeConstraint :: Constraint -> SolveM (SortMap, [SimpleConstraint])
decomposeConstraint (CSub t1 t2) = return (mempty, [t1 :<: t2])
decomposeConstraint (CEq  t1 t2) = return (mempty, [t1 :=: t2])
decomposeConstraint (CQual q ty) = (, []) <$> decomposeQual ty q
decomposeConstraint (CAnd cs)    = mconcat <$> mapM decomposeConstraint cs
decomposeConstraint CTrue        = return mempty
decomposeConstraint (CAll ty)    = do
  (vars, c) <- unbind ty
  let c' = substs (mkSkolems vars) c
  decomposeConstraint c'

  where
    mkSkolems :: [Name Type] -> [(Name Type, Type)]
    mkSkolems = map (id &&& Skolem)

decomposeQual :: Type -> Qual -> SolveM SortMap
decomposeQual (TyAtom a) q       = checkQual q a
decomposeQual ty@(TyCon c tys) q
  = case (M.lookup c >=> M.lookup q) qualRules of
      Nothing -> throwError $ Unqual q ty
      Just qs -> mconcat <$> zipWithM (maybe (return mempty) . decomposeQual) tys qs

checkQual :: Qual -> Atom -> SolveM SortMap
checkQual q (AVar (U v)) = return . SM $ M.singleton v (S.singleton q)
checkQual q (AVar (S v)) = throwError $ QualSkolem q v
checkQual q (ABase bty)  =
  case bty `S.member` (lkup "checkQual" qualifications q) of
    True  -> return mempty
    False -> throwError $ UnqualBase q bty

--------------------------------------------------
-- Step 3. Constraint simplification.

-- SimplifyM a = StateT SimplifyState SolveM a
--
--   (we can't literally write that as the definition since SolveM is
--   a type synonym and hence must be fully applied)

type SimplifyM a = StateT SimplifyState (FreshMT (Except SolveError)) a

-- | This step does unification of equality constraints, as well as
--   structural decomposition of subtyping constraints.  For example,
--   if we have a constraint (x -> y) <: (z -> Int), then we can
--   decompose it into two constraints, (z <: x) and (y <: Int); if we
--   have a constraint v <: (a,b), then we substitute v ↦ (x,y) (where
--   x and y are fresh type variables) and continue; and so on.
--
--   After this step, the remaining constraints will all be atomic
--   constraints, that is, only of the form (v1 <: v2), (v <: b), or
--   (b <: v), where v is a type variable and b is a base type.

simplify :: SortMap -> [SimpleConstraint] -> Except SolveError (SortMap, [(Atom, Atom)], S)
simplify sm cs
  = (\(SS sm' cs' s') -> (sm', map extractAtoms cs', s'))
  <$> contFreshMT (execStateT simplify' (SS sm cs idS)) n
  where

    fvNums :: Alpha a => [a] -> [Integer]
    fvNums = map (name2Integer :: Name Type -> _) . toListOf fv

    -- Find first unused integer in constraint free vars and sort map
    -- domain, and use it to start the fresh var generation, so we don't
    -- generate any "fresh" names that interfere with existing names
    n1 = maximum0 . fvNums $ cs
    n = succ . maximum . (n1:) . fvNums . M.keys . unSM $ sm

    maximum0 [] = 0
    maximum0 xs = maximum xs

    -- Extract the type atoms from an atomic constraint.
    extractAtoms :: SimpleConstraint -> (Atom, Atom)
    extractAtoms (TyAtom a1 :<: TyAtom a2) = (a1, a2)
    extractAtoms c = error $ "Impossible: simplify left non-atomic or non-subtype constraint " ++ show c

    -- Iterate picking one simplifiable constraint and simplifying it
    -- until none are left.
    simplify' :: SimplifyM ()
    simplify' = do
      -- q <- gets fst
      -- traceM (pretty q)
      -- traceM ""

      mc <- pickSimplifiable
      case mc of
        Nothing -> return ()
        Just s  -> do

          -- traceM (pretty s)
          -- traceM "---------------------------------------"

          simplifyOne s
          simplify'

    -- Pick out one simplifiable constraint, removing it from the list
    -- of constraints in the state.  Return Nothing if no more
    -- constraints can be simplified.
    pickSimplifiable :: SimplifyM (Maybe SimpleConstraint)
    pickSimplifiable = do
      cs <- use ssConstraints
      case pick simplifiable cs of
        Nothing     -> return Nothing
        Just (a,as) -> do
          ssConstraints .= as
          return (Just a)

    -- Pick the first element from a list satisfying the given
    -- predicate, returning the element and the list with the element
    -- removed.
    pick :: (a -> Bool) -> [a] -> Maybe (a,[a])
    pick _ [] = Nothing
    pick p (a:as)
      | p a       = Just (a,as)
      | otherwise = second (a:) <$> pick p as

    -- Check if a constraint can be simplified.  An equality
    -- constraint can always be "simplified" via unification.  A
    -- subtyping constraint can be simplified if either it involves a
    -- type constructor (in which case we can decompose it), or if it
    -- involves two base types (in which case it can be removed if the
    -- relationship holds).
    simplifiable :: SimpleConstraint -> Bool
    simplifiable (_ :=: _)                               = True
    simplifiable (TyCon {} :<: TyCon {})                 = True
    simplifiable (TyVar {} :<: TyCon {})                 = True
    simplifiable (TyCon {} :<: TyVar  {})                = True
    simplifiable (TyAtom (ABase _) :<: TyAtom (ABase _)) = True

    simplifiable _                                       = False

    -- Simplify the given simplifiable constraint.
    simplifyOne :: SimpleConstraint -> SimplifyM ()

    -- If we have an equality constraint, run unification on it.  The
    -- resulting substitution is applied to the remaining constraints
    -- as well as prepended to the current substitution.
    simplifyOne eqn@(ty1 :=: ty2) =
      case unify [(ty1, ty2)] of
        Nothing -> throwError NoUnify
        Just s' -> extendSubst s'

    -- Given a subtyping constraint between two type constructors,
    -- decompose it if the constructors are the same (or fail if they
    -- aren't), taking into account the variance of each argument to
    -- the constructor.
    simplifyOne (TyCon c1 tys1 :<: TyCon c2 tys2)
      | c1 /= c2  = throwError NoUnify
      | otherwise =
          ssConstraints %= (zipWith3 variance (arity c1) tys1 tys2 ++)

    -- Given a subtyping constraint between a variable and a type
    -- constructor, expand the variable into the same constructor
    -- applied to fresh type variables.
    simplifyOne con@(TyVar a   :<: TyCon c _) = expandStruct a c con
    simplifyOne con@(TyCon c _ :<: TyVar a  ) = expandStruct a c con

    -- Given a subtyping constraint between two base types, just check
    -- whether the first is indeed a subtype of the second.  (Note
    -- that we only pattern match here on type atoms, which could
    -- include variables, but this will only ever get called if
    -- 'simplifiable' was true, which checks that both are base
    -- types.)
    simplifyOne (TyAtom (ABase b1) :<: TyAtom (ABase b2)) = do
      case isSubB b1 b2 of
        True  -> return ()
        False -> throwError NoUnify

    expandStruct :: Name Type -> Con -> SimpleConstraint -> SimplifyM ()
    expandStruct a c con = do
      as <- mapM (const (TyVar <$> fresh (string2Name "a"))) (arity c)
      let s' = a |-> TyCon c as
      ssConstraints %= (con:)
      extendSubst s'

    -- 1. compose s' with current subst
    -- 2. apply s' to constraints
    -- 3. apply s' to qualifier map and decompose
    extendSubst :: S -> SimplifyM ()
    extendSubst s' = do
      ssSubst %= (s'@@)
      ssConstraints %= substs s'
      substSortMap s'

    substSortMap :: S -> SimplifyM ()
    substSortMap s' = do
      SM sm <- use ssSortMap

      -- 1. Get quals for each var in domain of s' and match them with
      -- the types being substituted for those vars.

      let tySorts :: [(Type, Sort)]
          tySorts = catMaybes . map (traverse (flip M.lookup sm) . swap) $ s'

          tyQualList :: [(Type, Qual)]
          tyQualList = concatMap (sequenceA . second S.toList) $ tySorts

      -- 2. Decompose the resulting qualifier constraints

      SM sm' <- lift $ mconcat <$> mapM (uncurry decomposeQual) tyQualList

      -- 3. delete domain of s' from sm and merge in decomposed quals.
      --    Be sure to keep quals from before, via 'unionWith'!

      ssSortMap .= SM (M.unionWith S.union sm' (foldl' (flip M.delete) sm (map fst s')))

      -- The above works even when unifying two variables.  Say we have
      -- the SortMap
      --
      --   a |-> {add}
      --   b |-> {sub}
      --
      -- and we get back theta = [a |-> b].  The domain of theta
      -- consists solely of a, so we look up a in the SortMap and get
      -- {add}.  We therefore generate the constraint 'add (theta a)'
      -- = 'add b' which can't be decomposed at all, and hence yields
      -- the SortMap {b |-> {add}}.  We then delete a from the
      -- original SortMap and merge the result with the new SortMap,
      -- yielding {b |-> {sub,add}}.


    -- Create a subtyping constraint based on the variance of a type
    -- constructor argument position: in the usual order for
    -- covariant, and reversed for contravariant.
    variance Co     ty1 ty2 = ty1 :<: ty2
    variance Contra ty1 ty2 = ty2 :<: ty1

--------------------------------------------------
-- Step 4: Build constraint graph

-- | Given a list of atomic subtype constraints (each pair @(a1,a2)@
--   corresponds to the constraint @a1 <: a2@) build the corresponding
--   constraint graph.
mkConstraintGraph :: [(Atom, Atom)] -> Graph Atom
mkConstraintGraph cs = G.mkGraph nodes (S.fromList cs)
  where
    nodes = S.fromList $ cs ^.. traverse . each

--------------------------------------------------
-- Step 5: Check skolems

-- | Check for any weakly connected components containing more than
--   one skolem, or a skolem and a base type, or a skolem and any
--   variables with nontrivial sorts; such components are not allowed.
--   If there are any WCCs with a single skolem, no base types, and
--   only unsorted variables, just unify them all with the skolem and
--   remove those components.
checkSkolems :: SortMap -> Graph Atom -> Except SolveError (Graph UAtom, S)
checkSkolems (SM sm) g = do
  let skolemWCCs :: [Set Atom]
      skolemWCCs = filter (any isSkolem) $ G.wcc g

      ok wcc =  S.size (S.filter isSkolem wcc) <= 1
             && all (\case { ABase _ -> False
                           ; AVar (S _) -> True
                           ; AVar (U v) -> maybe True S.null (M.lookup v sm) })
                wcc

      (good, bad) = partition ok skolemWCCs

  when (not . null $ bad) $ throwError NoUnify

  -- take all good sets and
  --   (1) delete them from the graph
  --   (2) unify them all with the skolem
  unifyWCCs g idS good

  where
    noSkolems (ABase b)    = Left b
    noSkolems (AVar (U v)) = Right v
    noSkolems (AVar (S v)) = error $ "Skolem " ++ show v ++ " remaining in noSkolems"

    unifyWCCs g s []     = return (G.map noSkolems g, s)
    unifyWCCs g s (u:us) = do
      traceM $ "Unifying " ++ pretty (u:us) ++ "..."

      let g' = foldl' (flip G.delete) g u

          ms' = unifyAtoms (S.toList u)
      case ms' of
        Nothing -> throwError NoUnify
        Just s' -> unifyWCCs g' (atomToTypeSubst s' @@ s) us

--------------------------------------------------
-- Step 6: Eliminate cycles

-- | Eliminate cycles in the constraint set by collapsing each
--   strongly connected component to a single node, (unifying all the
--   types in the SCC). A strongly connected component is a maximal
--   set of nodes where every node is reachable from every other by a
--   directed path; since we are using directed edges to indicate a
--   subtyping constraint, this means every node must be a subtype of
--   every other, and the only way this can happen is if all are in
--   fact equal.
--
--   Of course, this step can fail if the types in a SCC are not
--   unifiable.  If it succeeds, it returns the collapsed graph (which
--   is now guaranteed to be acyclic, i.e. a DAG) and a substitution.
elimCycles :: Graph UAtom -> Except SolveError (Graph UAtom, S)
elimCycles g
  = maybeError NoUnify
  $ (G.map fst &&& (atomToTypeSubst . compose . S.map snd . G.nodes)) <$> g'
  where

    g' :: Maybe (Graph (UAtom, S' Atom))
    g' = G.sequenceGraph $ G.map unifySCC (G.condensation g)

    unifySCC :: Set UAtom -> Maybe (UAtom, S' Atom)
    unifySCC uatoms
      | S.null uatoms = error "Impossible! unifySCC on the empty set"
      | otherwise     = (flip substs a &&& id) <$> unifyAtoms (map uatomToAtom as)
      where
        as@(a:_) = S.toList uatoms

------------------------------------------------------------
-- Steps 7 and 8: Constraint resolution
------------------------------------------------------------

-- | Rels stores the set of base types and variables related to a
--   given variable in the constraint graph (either predecessors or
--   successors, but not both).
data Rels = Rels
  { baseRels :: Set BaseTy
  , varRels  :: Set (Name Type)
  }
  deriving (Show, Eq)

-- | A RelMap associates each variable to its sets of base type and
--   variable predecessors and successors in the constraint graph.
type RelMap = Map (Name Type, Dir) Rels

substRel :: Name Type -> BaseTy -> RelMap -> RelMap
substRel x ty
  = M.delete (x,Super)
  . M.delete (x,Sub)
  . M.map (\r@(Rels b v) -> if x `S.member` v then Rels (S.insert ty b) (S.delete x v) else r)

-- | Essentially dirtypesBySort sm rm dir t s x finds all the
--   dir-types (sub- or super-) of t which have sort s, relative to
--   the variables in x.  This is \overbar{T}_S^X (resp. \underbar...)
--   from Traytel et al.
dirtypesBySort :: SortMap -> RelMap -> Dir -> BaseTy -> Sort -> Set (Name Type) -> [BaseTy]
dirtypesBySort sm relMap dir t s x

    -- Keep only those supertypes t' of t
  = keep (dirtypes dir t) $ \t' ->
      -- which have the right sort, and such that
      hasSort t' s &&

      -- for all variables beta \in x,
      (forAll x $ \beta ->

       -- there is at least one type t'' which is a subtype of t'
       -- which would be a valid solution for beta, that is,
       exists (dirtypes (other dir) t') $ \t'' ->

          -- t'' has the sort beta is supposed to have, and
         (hasSort t'' (getSort sm beta)) &&

          -- t'' is a supertype of every base type predecessor of beta.
         (forAll (baseRels (lkup "dirtypesBySort, beta rel" relMap (beta, other dir))) $ \u ->
            isDirB dir t'' u
         )
      )

    -- The above comments are written assuming dir = Super; of course,
    -- if dir = Sub then just swap "super" and "sub" everywhere.

  where
    forAll, exists :: Foldable t => t a -> (a -> Bool) -> Bool
    forAll = flip all
    exists = flip any
    keep   = flip filter

-- | Sort-aware infimum or supremum.
limBySort :: SortMap -> RelMap -> Dir -> [BaseTy] -> Sort -> Set (Name Type) -> Maybe BaseTy
limBySort sm rm dir ts s x
  = (\is -> find (\lim -> all (\u -> isDirB dir u lim) is) is)
  . isects
  . map (\t -> dirtypesBySort sm rm dir t s x)
  $ ts
  where
    isects = foldr1 intersect

lubBySort, glbBySort :: SortMap -> RelMap -> [BaseTy] -> Sort -> Set (Name Type) -> Maybe BaseTy
lubBySort sm rm = limBySort sm rm Super
glbBySort sm rm = limBySort sm rm Sub

-- | From the constraint graph, build the sets of sub- and super- base
--   types of each type variable, as well as the sets of sub- and
--   supertype variables.  For each type variable x in turn, try to
--   find a common supertype of its base subtypes which is consistent
--   with the sort of x and with the sorts of all its sub-variables,
--   as well as symmetrically a common subtype of its supertypes, etc.
--   Assign x one of the two: if it has only successors, assign it
--   their inf; otherwise, assign it the sup of its predecessors.  If
--   it has both, we have a choice of whether to assign it the sup of
--   predecessors or inf of successors; both lead to a sound &
--   complete algorithm.  We choose to assign it the sup of its
--   predecessors in this case, since it seems nice to default to
--   "simpler" types lower down in the subtyping chain.
solveGraph :: SortMap -> Graph UAtom -> SolveM S
solveGraph sm g = (atomToTypeSubst . unifyWCC) <$> go topRelMap
  where
    unifyWCC :: S' BaseTy -> S' Atom
    unifyWCC s = concatMap mkEquateSubst wccVarGroups @@ (map (coerce *** ABase) s)
      where
        wccVarGroups :: [Set UAtom]
        wccVarGroups  = filter (all isRight) . substs s $ G.wcc g
        mkEquateSubst :: Set UAtom -> S' Atom
        mkEquateSubst _ = idS
            -- This was the code from before which doesn't type check any more.

            -- (\(a:as) -> map (\v -> (coerce v, a)) as) . map fromRight . S.toList

            -- However, since disco will never infer a polymorphic
            -- type (for now), it doesn't really matter.  I'll leave
            -- mkEquateSubst here as idS (+ the below comments) for
            -- now just as a reminder of where to put this code if we
            -- ever want it again in the future.

            -- After picking concrete base types for all the type
            -- variables we can, the only thing possibly remaining in
            -- the graph are components containing only type variables
            -- and no base types.  It is sound, and simplifies the
            -- generated types considerably, to simply unify any type
            -- variables which are related by subtyping constraints.
            -- That is, we collect all the type variables in each
            -- weakly connected component and unify them.
            --
            -- As an example where this final step makes a difference,
            -- consider a term like @\x. (\y.y) x@.  A fresh type
            -- variable is generated for the type of @x@, and another
            -- for the type of @y@; the application of @(\y.y)@ to @x@
            -- induces a subtyping constraint between the two type
            -- variables.  The most general type would be something
            -- like @forall a b. (a <: b) => a -> b@, but we want to
            -- avoid generating unnecessary subtyping constraints (the
            -- type system might not even support subtyping qualifiers
            -- like this).  Instead, we unify the two type variables
            -- and the resulting type is @forall a. a -> a@.

        fromRight (Right r) = r

    -- Get the successor and predecessor sets for all the type variables.
    topRelMap :: RelMap
    topRelMap
      = M.map (uncurry Rels . (S.fromAscList *** S.fromAscList) . partitionEithers . S.toList)
      $ M.mapKeys (,Super) subMap `M.union` M.mapKeys (,Sub) superMap

    subMap, superMap :: Map (Name Type) (Set UAtom)
    (subMap, superMap) = (onlyVars *** onlyVars) $ G.cessors g

    onlyVars :: Map UAtom (Set UAtom) -> Map (Name Type) (Set UAtom)
    onlyVars = M.mapKeys (\(Right n) -> n) . M.filterWithKey (\a _ -> isRight a)

    go :: RelMap -> SolveM (S' BaseTy)
    go relMap = case as of

      -- No variables left that have base type constraints.
      []    -> return idS

      -- Solve one variable at a time.  See below.
      (a:_) ->

        case solveVar a of
          Nothing       -> do
            traceM $ "Couldn't solve for " ++ show a
            throwError NoUnify

          -- If we solved for a, delete it from the maps, apply the
          -- resulting substitution to the remainder (updating the
          -- relMap appropriately), and recurse.  The substitution we
          -- want will be the composition of the substitution for a
          -- with the substitution generated by the recursive call.
          --
          -- Note we don't need to delete a from the SortMap; we never
          -- use the set of keys from the SortMap for anything
          -- (indeed, some variables might not be keys if they have an
          -- empty sort), so it doesn't matter if old variables hang
          -- around in it.
          Just s ->
            (@@ s) <$> go (substRel a (fromJust $ P.lookup (coerce a) s) $ relMap)

      where
        -- NOTE we can't solve a bunch in parallel!  Might end up
        -- assigning them conflicting solutions if some depend on
        -- others.  For example, consider the situation
        --
        --            Z
        --            |
        --            a3
        --           /  \
        --          a1   N
        --
        -- If we try to solve in parallel we will end up assigning a1
        -- -> Z (since it only has base types as an upper bound) and
        -- a3 -> N (since it has both upper and lower bounds, and by
        -- default we pick the lower bound), but this is wrong since
        -- we should have a1 < a3.
        --
        -- If instead we solve them one at a time, we could e.g. first
        -- solve a1 -> Z, and then we would find a3 -> Z as well.
        -- Alternately, if we first solve a3 -> N then we will have a1
        -- -> N as well.  Both are acceptable.
        --
        -- In fact, this exact graph comes from (^x.x+1) which was
        -- erroneously being inferred to have type Z -> N when I first
        -- wrote the code.

        -- Get only the variables we can solve on this pass, which
        -- have base types in their predecessor or successor set.
        as = map fst . filter (\k -> not . S.null . baseRels $ (lkup "solveGraph.go.as" relMap k)) $ M.keys relMap

        -- Solve for a variable, failing if it has no solution, otherwise returning
        -- a substitution for it.
        solveVar :: Name Type -> Maybe (S' BaseTy)
        solveVar v =
          case ((v,Super), (v,Sub)) & over both (S.toList . baseRels . (lkup "solveGraph.solveVar" relMap)) of
            ([], []) ->
              error $ "Impossible! solveGraph.solveVar called on variable "
                      ++ show v ++ " with no base type successors or predecessors"

            -- Only supertypes.  Just assign a to their inf, if one exists.
            (bsupers, []) ->
              trace (show v ++ " has only supertypes (" ++ show bsupers ++ ")") $
              (coerce v |->) <$> glbBySort sm relMap bsupers (lkup "solveVar bsupers, sort" (unSM sm) v)
                (varRels (lkup "solveVar bsupers, rels" relMap (v,Super)))

            -- Only subtypes.  Just assign a to their sup.
            ([], bsubs)   ->
              trace (show v ++ " has only subtypes (" ++ show bsubs ++ ")") $
              trace ("sortmap: " ++ show sm) $
              trace ("relmap: " ++ show relMap) $
              trace ("sort for " ++ show v ++ ": " ++ show (getSort sm v)) $
              trace ("relvars: " ++ show (varRels (relMap ! (v,Sub)))) $
              (coerce v |->) <$> lubBySort sm relMap bsubs (getSort sm v)
                (varRels (lkup "solveVar bsubs, rels" relMap (v,Sub)))

            -- Both successors and predecessors.  Both must have a
            -- valid bound, and the bounds must not overlap.  Assign a
            -- to the sup of its predecessors.
            (bsupers, bsubs) -> do
              ub <- glbBySort sm relMap bsupers (getSort sm v)
                      (varRels (relMap ! (v,Super)))
              lb <- lubBySort sm relMap bsubs   (getSort sm v)
                      (varRels (relMap ! (v,Sub)))
              case isSubB lb ub of
                True  -> Just (coerce v |-> lb)
                False -> Nothing

------------------------------------------------------------
-- main
------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let settings = defaultSettings
        { historyFile = Just ".bipolysub_history" }
  runInputT settings repl

repl :: InputT IO ()
repl = forever $ do
  minp <- getInputLine "> "
  case minp of
    Just inp -> case parse expr inp of
      Left err -> iprint err
      Right e  -> case runTC (infer e) of
        Left err -> iprint err
        Right (ty, c) -> do
          ipretty ty
          ipretty c
          case runSolveM $ solveConstraint c of
            Left err -> iprint err
            Right s  -> do
              ipretty s
              iputStrLn (pretty (interp e) ++ " : " ++ pretty (substs s ty))
    Nothing -> liftIO exitSuccess
  where
    iprint :: Show a => a -> InputT IO ()
    iprint    = liftIO . print

    iputStrLn = liftIO . putStrLn

    ipretty :: Pretty a => a -> InputT IO ()
    ipretty = iputStrLn . pretty

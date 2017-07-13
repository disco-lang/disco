{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- STLC extended with a bit of arithmetic and some simple subtyping.
-- Implementation of Traytel et al (APLAS 2011).  Just inference/type
-- reconstruction (will try a bidirectional variant later).

module Sub2 where

import           Parsing2

import Prelude hiding (lookup)
import qualified Prelude as P

import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Control.Lens (anyOf, toListOf, each, (^..), over, both)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import           Data.Either
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Data.Tuple
import           Data.Maybe
import           Data.Void
import           Text.Printf

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS    (condensation)

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Op = Plus | Minus
  deriving (Show, Eq, Generic)

data Expr where
  EVar  :: Name Expr -> Expr

  ENat  :: Integer -> Expr
  EPlus :: Expr
  ENeg  :: Expr

  ELam  :: Bind (Name Expr) Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  EPair :: Expr -> Expr -> Expr
  EFst  :: Expr -> Expr
  ESnd  :: Expr -> Expr

  deriving (Show, Generic)

data Atom where
  AVar :: Name Type -> Atom
  ANat :: Atom
  AInt :: Atom
  deriving (Show, Eq, Ord, Generic)

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isSub :: Atom -> Atom -> Bool
isSub a1 a2 | a1 == a2 = True
isSub ANat AInt = True
isSub _ _ = False

data Cons where
  CArr  :: Cons
  CPair :: Cons
  -- F     :: Cons  -- XXX for testing
  -- G     :: Cons
  -- H     :: Cons
  deriving (Show, Eq, Generic)

data Variance = Co | Contra

arity :: Cons -> [Variance]
arity CArr  = [Contra, Co]
arity CPair = [Co, Co]

pattern TyVar :: Name Type -> Type
pattern TyVar v = TyAtom (AVar v)

var :: String -> Type
var x = TyVar (string2Name x)

pattern TyNat :: Type
pattern TyNat   = TyAtom ANat

pattern TyInt :: Type
pattern TyInt   = TyAtom AInt

pattern TyFun :: Type -> Type -> Type
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

pattern TyPair :: Type -> Type -> Type
pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

data Type where
  TyAtom :: Atom -> Type
  TyCons :: Cons -> [Type] -> Type
  deriving (Show, Eq, Generic)

instance Alpha Cons
instance Alpha Atom
instance Alpha Op
instance Alpha Expr
instance Alpha Type
instance Alpha Void

------------------------------------------------------------
-- Substitutions and unification
------------------------------------------------------------

--------------------------------------------------
-- Subst instances

instance Subst Expr Op where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Type where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Expr where
  isvar (EVar x) = Just (SubstName x)
  isvar _        = Nothing

instance Subst Type Cons
instance Subst Type Atom
instance Subst Type Type where
  isvar (TyAtom (AVar x)) = Just (SubstName x)
  isvar _                 = Nothing

--------------------------------------------------
-- Substitutions

type S' a = [(Name a, a)]

type S = S' Type

idS :: S' a
idS = []

(|->) :: Name a -> a -> S' a
x |-> t = [(x,t)]

(@@) :: Subst a a => S' a -> S' a -> S' a
s1 @@ s2 = (map . second) (substs s1) s2 ++ s1

--------------------------------------------------
-- Unification

data Eqn where
  (:=:) :: Type -> Type -> Eqn
  deriving (Eq, Show, Generic)

instance Alpha Eqn
instance Subst Type Eqn

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible).
--
--   This is not the most efficient way to implement unification but
--   it is simple.
unify :: [Eqn] -> Maybe S
unify [] = Just idS
unify (e:es) = do
  u <- unifyOne e
  case u of
    Left sub    -> (@@ sub) <$> unify (substs sub es)
    Right newEs -> unify (newEs ++ es)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)

unifyOne :: Eqn -> Maybe (Either S [Eqn])
unifyOne (ty1 :=: ty2)
  | ty1 == ty2 = return $ Left idS
unifyOne (TyVar x :=: ty2)
  | occurs x ty2 = Nothing
  | otherwise    = Just $ Left (x |-> ty2)
unifyOne (ty1 :=: x@(TyVar _))
  = unifyOne (x :=: ty1)
unifyOne (TyCons c1 tys1 :=: TyCons c2 tys2)
  | c1 == c2  = return $ Right (zipWith (:=:) tys1 tys2)
  | otherwise = Nothing

-- exampleEqns :: [Eqn]
-- exampleEqns =
--   [ TyCons G [var "x2"] :=: var "x1"
--   , TyCons F [var "x1", TyCons H [var "x1"], var "x2"]
--     :=:
--     TyCons F [TyCons G [var "x3"], var "x4", var "x3"]
--   ]

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

identifier :: Parser (Name Expr)
identifier = string2Name <$> getIdentifier lexer

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
  <|> eLam  <$> (reservedOp "^" *> identifier)
            <*> (reservedOp "." *> parseExpr)
  <|> parens parseExpr
  where
    eLam x e = ELam (bind x e)

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix  (EApp <$ reservedOp "")   AssocLeft ]
            , [ Prefix (eNeg <$ reservedOp "-") ]
            , [ Infix  (ePlus  <$ reservedOp "+") AssocLeft
              ]
            ]
    ePlus e1 e2 = EApp (EApp EPlus e1) e2
    eNeg  e1    = EApp ENeg e1

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
-- Type checker
------------------------------------------------------------

type Ctx = M.Map (Name Expr) Type

data Ineqn where
  (:<:) :: Type -> Type -> Ineqn
  deriving (Eq, Show, Generic)

instance Alpha Ineqn
instance Subst Type Ineqn

type Constraint = Either Eqn Ineqn

(===) :: Type -> Type -> Constraint
ty1 === ty2 = Left (ty1 :=: ty2)

(=<=) :: Type -> Type -> Constraint
ty1 =<= ty2 = Right (ty1 :<: ty2)

--------------------------------------------------
-- Type checking monad

data TypeError where
  Unbound :: Name Expr -> TypeError
  NoUnify :: TypeError
  Unknown :: TypeError
  deriving (Show)

newtype TC a = TC { unTC :: WriterT [Constraint] (ReaderT Ctx (ExceptT TypeError FreshM)) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Constraint], MonadReader Ctx, MonadError TypeError, Fresh)

runTC :: TC a -> Either TypeError (a, [Constraint])
runTC (TC t) = runFreshM . runExceptT . flip runReaderT M.empty . runWriterT $ t

extend :: Name Expr -> Type -> TC a -> TC a
extend x s = local (M.insert x s)

lookup :: Name Expr -> TC Type
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError $ Unbound x
    Just ty -> return ty

freshTy :: TC Type
freshTy = TyVar <$> fresh (string2Name "a")

-- Step 1.
infer :: Expr -> TC Type
infer (EVar x) = lookup x
infer (ENat i) = return TyNat
infer EPlus    = do
  a <- freshTy
  tell [a =<= TyInt]
  return $ TyFun a (TyFun a a)
infer ENeg     = do
  a <- freshTy
  tell [a =<= TyInt]
  return $ TyFun a TyInt
infer (ELam b) = do
  (x,body) <- unbind b
  tyIn  <- freshTy
  tyOut <- extend x tyIn $ infer body
  return $ TyFun tyIn tyOut
infer (EApp e1 e2) = do
  a <- freshTy
  b <- freshTy
  ty1 <- infer e1
  ty2 <- infer e2
  tell [ty1 === TyFun a b, ty2 =<= a]
  return b

-- Call infer, then unify any equations and apply the generated
-- substitution. (Note, in a bidirectional system I don't think any
-- equations will be generated?)
--
-- Actually I am not sure this is what we are supposed to do.  Leaving
-- it here for reference.  But instead we should just leave all the
-- equations together and then start simplifying them. (Does it
-- matter?)
inferAndUnify :: Expr -> Either TypeError (Type, [Ineqn])
inferAndUnify e =
  case runTC (infer e) of
    Left err         -> Left err
    Right (ty, cons) ->
      let (eqns, ineqns) = partitionEithers cons
          ms = unify eqns
      in
        case ms of
          Nothing -> Left NoUnify
          Just s  -> Right (substs s ty, substs s ineqns)

-- XXX need to add LFreshM to SimplifyM, since we need to generate
-- fresh variables.  Start the computation with a call to 'avoid' with
-- the free type variables already in the constraint set.

type SimplifyM a = StateT ([Constraint], S) (ExceptT TypeError LFreshM) a

-- This is the next step after doing inference & constraint
-- generation.  After this step, the remaining constraints will all be
-- atomic constraints, of the form (v1 < v2), (v < b), or (b < v),
-- where v is a type variable and b is a base type.
--
-- Actually, there is one more step that should happen before this, to
-- check weak unification (to ensure this algorithm will terminate).
simplify :: [Constraint] -> Either TypeError ([(Atom, Atom)], S)
simplify cs
  = (fmap . first . map) extractAtoms
  $ runLFreshM (runExceptT (execStateT simplify' (cs, idS)))
  where
    extractAtoms (Right (TyAtom a1 :<: TyAtom a2)) = (a1, a2)
    extractAtoms c = error $ "simplify left non-atomic or non-subtype constraint " ++ show c

    simplify' :: SimplifyM ()
    simplify' = avoid (toListOf fvAny cs) $ do
      mc <- pickSimplifiable
      case mc of
        Nothing -> return ()
        Just s  -> simplifyOne s >> simplify'

    simplifyOne :: Constraint -> SimplifyM ()
    simplifyOne (Left eqn) =
      case unify [eqn] of
        Nothing -> throwError NoUnify
        Just s' -> modify (substs s' *** (s' @@))
    simplifyOne (Right (TyCons c1 tys1 :<: TyCons c2 tys2))
      | c1 /= c2  = throwError NoUnify
      | otherwise = modify (first (zipWith3 variance (arity c1) tys1 tys2 ++))
    simplifyOne con@(Right (TyVar a :<: TyCons c tys)) = do
      as <- mapM (const (TyVar <$> lfresh (string2Name "a"))) (arity c)
      let s' = a |-> TyCons c as
      modify ((substs s' . (con:)) *** (s'@@))
    simplifyOne con@(Right (TyCons c tys :<: TyVar a)) = do
      as <- mapM (const (TyVar <$> lfresh (string2Name "a"))) (arity c)
      let s' = a |-> TyCons c as
      modify ((substs s' . (con:)) *** (s'@@))
    simplifyOne (Right (TyAtom a1 :<: TyAtom a2)) = do
      case isSub a1 a2 of
        True  -> return ()
        False -> throwError NoUnify

    variance Co     ty1 ty2 = ty1 =<= ty2
    variance Contra ty1 ty2 = ty2 =<= ty1

    pickSimplifiable :: SimplifyM (Maybe Constraint)
    pickSimplifiable = do
      cs <- fst <$> get
      case pick simplifiable cs of
        Nothing     -> return Nothing
        Just (a,as) -> modify (first (const as)) >> return (Just a)

    pick :: (a -> Bool) -> [a] -> Maybe (a,[a])
    pick _ [] = Nothing
    pick p (a:as)
      | p a       = Just (a,as)
      | otherwise = second (a:) <$> pick p as

    simplifiable :: Constraint -> Bool
    simplifiable (Left _) = True
    simplifiable (Right (TyCons {} :<: TyCons {})) = True
    simplifiable (Right (TyVar  {} :<: TyCons {})) = True
    simplifiable (Right (TyCons {} :<: TyVar  {})) = True
    simplifiable (Right (TyAtom a1 :<: TyAtom a2)) = not (isVar a1) && not (isVar a2)

-- Given a list of atomic subtype constraints, build the corresponding
-- constraint graph.
mkConstraintGraph :: [(Atom, Atom)] -> Gr Atom ()
mkConstraintGraph cs = mkGraph nodes edges
  where
    nodes   = zip [0..] (nubify $ (cs ^.. traverse . each))
    nodeMap = M.fromList . map swap $ nodes
    edges   = nubify $ map mkEdge cs
    mkEdge  (a1,a2) = (getNode a1, getNode a2, ())
    getNode = fromJust . flip M.lookup nodeMap
    nubify :: Ord a => [a] -> [a]
    nubify = S.toList . S.fromList

-- Next step: eliminate strongly connected components in the
-- constraint set by collapsing them (unifying all the types in the
-- SCC). Can fail if the types in a SCC are not unifiable.  Returns
-- the collapsed graph (which is now guaranteed to be DAG) and an
-- updated substitution.
elimCycles :: Gr Atom () -> S -> Either TypeError (Gr Atom (), S)
elimCycles g s = undefined
  where
    nodeMap = M.fromList $ labNodes g   -- map   Int -> Atom

    -- Condense each SCC into a node labeled by its list of atoms,
    -- which will be unified
    g' :: Gr [Atom] ()
    g' = nmap (map (fromJust . flip M.lookup nodeMap)) . condensation $ g

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

data Value where
  VInt     :: Integer -> Value
  VClosure :: Bind (Name Expr) Expr -> Env -> Value
  deriving Show

type Env = M.Map (Name Expr) Value

extendV :: MonadReader Env m => Name Expr -> Value -> m a -> m a
extendV x v = local (M.insert x v)

withEnv :: MonadReader Env m => Env -> m a -> m a
withEnv e = local (const e)

interp :: Expr -> Value
interp = runLFreshM . flip runReaderT M.empty . interp'

interp' :: Expr -> ReaderT Env LFreshM Value
interp' (EVar x) = (fromJust . M.lookup x) <$> ask
interp' (ENat n) = return $ VInt n
interp' (EApp (EApp EPlus ea) eb) = do
  va <- interp' ea
  vb <- interp' eb
  case (va, vb) of
    (VInt na, VInt nb) -> return $ VInt (na + nb)
    _ -> error "Impossible! interp' EBin on non-Ints"
interp' (EApp ENeg ea) = do
  va <- interp' ea
  case va of
    VInt na -> return $ VInt (-na)
    _ -> error "Impossible! interp' ENeg on non-Int"
interp' (ELam b) = VClosure b <$> ask
interp' (EApp fun arg) = do
  vf <- interp' fun
  va <- interp' arg
  case vf of
    VClosure b env ->
      withEnv env $
      lunbind b $ \(x,body) ->
      extendV x va $ do
        interp' body
    _ -> error $ printf "Impossible! interp' EApp with (%s) (%s)" (show fun) (show arg)

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Plus  = (+)
interpOp Minus = (-)

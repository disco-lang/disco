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

-- Next steps:
--
--   1. Package up the entire inference process into a single function
--      that can either generate an error or return an inferred type
--      and a substitution.  Split out into module(s), try to
--      generalize as much as possible.  Figure out what input we need
--      (isSub function for base types, etc.) --- package in a record?
--      Whole thing operates in a monad something like ReaderT Record
--      (ExceptT Error) ?
--
--   2. Create a simple REPL wrapper.
--
--   3. Generalized version with qualified types and sorts.  (Add
--      booleans; num, sub, finite; generalize arity function, etc.)
--
--   4. Bidirectional variant of the above.  Will now need two kinds
--      of variables, skolem vars and unification vars.

module Sub2 where

import Data.Coerce

import           Parsing2
import qualified Graph as G
import           Graph (Graph)
import           Subst
import           Types
import           Constraints

import Prelude hiding (lookup)
import qualified Prelude as P

import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import           Data.Either
import           Data.List (intercalate)
import           Data.Map (Map, (!))
import qualified Data.Map    as M
import           Data.Set (Set)
import qualified Data.Set    as S
import           Data.Maybe
import           Data.Void
import           Text.Printf

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
  ESqrt :: Expr

  -- Introduced sqrt with a strange typing rule just to test out cycle
  -- elimination:  sqrt has type a -> a as long as a <: Nat.  Then sqrt 3
  -- generates constraints Nat <: a1 <: Nat which is a cycle.

  ELam  :: Bind (Name Expr) Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  EPair :: Expr -> Expr -> Expr
  EFst  :: Expr
  ESnd  :: Expr

  deriving (Show, Generic)

instance Alpha Op
instance Alpha Expr

instance Subst Expr Op where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Expr where
  isvar (EVar x) = Just (SubstName x)
  isvar _        = Nothing
instance Subst Expr Type where
  subst  _ _ = id
  substs _   = id

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser emptyDef
  { reservedNames = ["let", "in", "fst", "snd", "sqrt"]
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
  <|> EFst  <$  reserved "fst"
  <|> ESnd  <$  reserved "snd"
  <|> ESqrt <$  reserved "sqrt"
  <|> eLam  <$> (reservedOp "^" *> identifier)
            <*> (reservedOp "." *> parseExpr)
  <|> try (parens (EPair <$> parseExpr <*> (symbol "," *> parseExpr)))
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

type Ctx = Map (Name Expr) Type

--------------------------------------------------
-- Type checking monad

data TypeError where
  Unbound :: Name Expr -> TypeError
  NoUnify :: TypeError
  Unknown :: TypeError
  deriving (Show)

newtype TC a = TC { unTC :: WriterT [Constraint Type] (ReaderT Ctx (ExceptT TypeError FreshM)) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Constraint Type], MonadReader Ctx, MonadError TypeError, Fresh)

runTC :: TC a -> Either TypeError (a, [Constraint Type])
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
infer ESqrt    = do
  a <- freshTy
  tell [a =<= TyNat]
  return $ TyFun a a
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
infer EFst = do
  a <- freshTy
  b <- freshTy
  return (TyFun (TyPair a b) a)
infer ESnd = do
  a <- freshTy
  b <- freshTy
  return (TyFun (TyPair a b) b)
infer (EPair e1 e2) = TyPair <$> infer e1 <*> infer e2

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

data Value where
  VInt     :: Integer -> Value
  VClosure :: Bind (Name Expr) Expr -> Env -> Value
  deriving Show

type Env = Map (Name Expr) Value

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

------------------------------------------------------------
-- Pretty-printing
------------------------------------------------------------

type Prec = Int

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0 L

  prettyPrec :: Prec -> Associativity -> p -> String
  prettyPrec _ _ = pretty

instance Pretty Type where
  prettyPrec _ _ TyNat     = "N"
  prettyPrec _ _ TyInt     = "Z"
  prettyPrec p _ (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2
  prettyPrec _ _ (TyPair ty1 ty2) =
    mparens True $ prettyPrec 0 L ty1 ++ ", " ++ prettyPrec 0 R ty2
  prettyPrec _ _ (TyVar x) = show x

instance Pretty Atom where
  pretty ANat = "N"
  pretty AInt = "Z"
  pretty (AVar v) = show v

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

instance Pretty Op where
  pretty Plus  = " + "
  pretty Minus = " - "

instance Pretty Expr where
  prettyPrec _ _ (EVar x) = show x

  prettyPrec _ _ (ENat i) = show i
  prettyPrec p a (EApp (EApp EPlus e1) e2) =
    mparens (p>1 || (p==1 && a == R)) $
      (prettyPrec 1 L e1 ++ " + " ++ prettyPrec 1 R e2)
  prettyPrec p a (EApp ENeg e) = "-" ++ prettyPrec 2 R e

  prettyPrec p _ (ELam b) =
    mparens (p>0) $
      let (x,body) = unsafeUnbind b
      in  ("^" ++ show x ++ ". " ++ prettyPrec 0 L body)
  prettyPrec p a (EApp e1 e2) =
    mparens (p>3 || (p==3 && a == R)) $
      (prettyPrec 3 L e1 ++ " " ++ prettyPrec 3 R e2)

instance Pretty Env where
  pretty env = prettyList bindings
    where
      bindings = map prettyBinding (M.assocs env)
      prettyBinding (x, v) = show x ++ " -> " ++ pretty v

instance Pretty Value where
  pretty (VInt n) = show n
  pretty (VClosure b env)
    = printf "<%s: %s %s>"
      (show x) (pretty body) (pretty env)
    where
      (x, body) = unsafeUnbind b

prettyList xs = "[" ++ intercalate ", " xs ++ "]"

instance Pretty a => Pretty [a] where
  pretty as = prettyList (map pretty as)

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x,y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance Pretty t => Pretty (Eqn t) where
  pretty (ty1 :=: ty2) = pretty ty1 ++ " = " ++ pretty ty2

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a)  = pretty a
  pretty (Right b) = pretty b

instance Pretty t => Pretty (Ineqn t) where
  pretty (ty1 :<: ty2) = pretty ty1 ++ " < " ++ pretty ty2

instance (Pretty a, Ord a) => Pretty (Graph a) where
  pretty = prettyList . map prettyEdge . S.toList . G.edges
    where
      prettyEdge (a1,a2) = pretty a1 ++ " >> " ++ pretty a2

instance Pretty (Name Type) where
  pretty = show

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

module Sub where

import           Parsing2

import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

import Data.Equivalence.Monad

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map    as M
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
  EBin  :: Op -> Expr -> Expr -> Expr

  ELam  :: Maybe Type -> Bind (Name Expr) Expr -> Expr
  EApp  :: Expr -> Expr -> Expr

  EAnn  :: Expr -> Type -> Expr

  deriving (Show, Generic)

data Atom' v where
  AVar :: v -> Atom' v
  ANat :: Atom' v
  AInt :: Atom' v
  deriving (Show, Eq, Generic)

type Atom = Atom' (Name Type)

data Cons where
  CArr  :: Cons
  CPair :: Cons
  deriving (Show, Eq, Generic)

pattern TyVar :: v -> Type' v
pattern TyVar v = TyAtom (AVar v)

pattern TyNat :: Type' v
pattern TyNat   = TyAtom ANat

pattern TyInt :: Type' v
pattern TyInt   = TyAtom AInt

pattern TyFun :: Type' v -> Type' v -> Type' v
pattern TyFun ty1 ty2 = TyCons CArr [ty1, ty2]

pattern TyPair :: Type' v -> Type' v -> Type' v
pattern TyPair ty1 ty2 = TyCons CPair [ty1, ty2]

data Type' v where
  TyAtom :: Atom' v -> Type' v
  TyCons :: Cons -> [Type' v] -> Type' v
  deriving (Show, Eq, Generic)

type Type = Type' Void

instance Alpha Cons
instance Alpha v => Alpha (Atom' v)
instance Alpha Op
instance Alpha Expr
instance Alpha v => Alpha (Type' v)
instance Alpha Void

instance Subst Expr Op where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Type where
  subst  _ _ = id
  substs _   = id
instance Subst Expr Expr where
  isvar (EVar x) = Just (SubstName x)
  isvar _        = Nothing

type Ctx = M.Map (Name Expr) Type

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
            <*> (optionMaybe (reservedOp ":" *> parseType))
            <*> (reservedOp "." *> parseExpr)
  <|> parens parseExpr
  where
    eLam x ty e = ELam ty (bind x e)

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

-- ------------------------------------------------------------
-- -- Pretty printing
-- ------------------------------------------------------------

-- type Prec = Int

-- class Pretty p where
--   pretty :: p -> String
--   pretty = prettyPrec 0 L

--   prettyPrec :: Prec -> Associativity -> p -> String
--   prettyPrec _ _ = pretty

-- instance Pretty Type where
--   prettyPrec _ _ TyNat     = "N"
--   prettyPrec _ _ TyInt     = "Z"
--   prettyPrec p _ (TyFun ty1 ty2) =
--     mparens (p > 0) $ prettyPrec 1 L ty1 ++ " -> " ++ prettyPrec 0 R ty2

-- mparens :: Bool -> String -> String
-- mparens True  = ("("++) . (++")")
-- mparens False = id

-- data Associativity = L | R
--   deriving (Show, Eq)

-- instance Pretty Op where
--   pretty Plus  = " + "
--   pretty Minus = " - "

-- instance Pretty Expr where
--   prettyPrec _ _ (EVar x) = x

--   prettyPrec _ _ (ENat i) = show i
--   prettyPrec p a (EBin op e1 e2) =
--     mparens (p>1 || (p==1 && a == R)) $
--       (prettyPrec 1 L e1 ++ pretty op ++ prettyPrec 1 R e2)

--   prettyPrec p _ (ELam x mty body) =
--     mparens (p>0) $
--       ("^" ++ x ++ maybe "" (\ty -> " : " ++ pretty ty) mty
--            ++ ". " ++ prettyPrec 0 L body)
--   prettyPrec p a (EApp e1 e2) =
--     mparens (p>3 || (p==3 && a == R)) $
--       (prettyPrec 3 L e1 ++ " " ++ prettyPrec 3 R e2)

-- instance Pretty Env where
--   pretty env = "[" ++ intercalate ", " bindings ++ "]"
--     where
--       bindings = map prettyBinding (M.assocs env)
--       prettyBinding (x, v) = x ++ " -> " ++ pretty v

-- instance Pretty Value where
--   pretty (VInt n) = show n
--   pretty (VClosure env x body)
--     = printf "<%s: %s %s>"
--       x (pretty body) (pretty env)

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

--------------------------------------------------
-- Type checking monad

newtype TC a = TC { unTC :: StateT Int (ReaderT Ctx (Except TypeError)) a }
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadState Int, MonadError TypeError)

-- (EquivM' s Atom)

runTC :: TC a -> Either TypeError a
runTC (TC t) = runExcept . flip runReaderT M.empty . flip evalStateT 0 $ t

extend :: Name Expr -> Type -> TC a -> TC a
extend x s = local (M.insert x s)

newtype UVar = UVar Int
  deriving (Eq, Ord)

type UType = Type' UVar
type UAtom = Atom' UVar

newUVar :: TC UVar
newUVar = do
  i <- get
  put (i+1)
  return (UVar i)

type USubst = M.Map UVar UType

--------------------------------------------------
-- Inference mode

data SubC v = Type' v :<: Type' v

data Polarity = Co | Contra

data TypeError where
  Unbound  :: String -> TypeError
  NonNum   :: Type   -> TypeError
  NoLub    :: Type -> Type -> TypeError
  NotFunTy :: Type -> TypeError
  deriving Show

type MatchM s a = StateT ([SubC UVar], USubst) (EquivT' s UAtom TC) a

match :: [SubC UVar] -> TC USubst
match initcs = snd <$> runEquivT' (flip execStateT (initcs, M.empty) match')
  where
    getC :: MatchM s [SubC UVar]
    getC = fst <$> get

    putC :: [SubC UVar] -> MatchM s ()
    putC = modify . first . const

    addC :: SubC UVar -> MatchM s ()
    addC = modify . first . (:)

    match' :: MatchM s ()
    match' = do
      getC >>= \case
        []     -> return ()
        (c:cs) -> putC cs >> matchOne c

    matchOne :: SubC UVar -> MatchM s ()
    matchOne (TyCons c1 tys1 :<: TyCons c2 tys2)
      | c1 /= c2  = undefined   -- type mismatch error
      | otherwise = decompose c1 tys1 tys2
    matchOne (TyAtom a1 :<: TyAtom a2) = undefined

    decompose :: Cons -> [UType] -> [UType] -> MatchM s ()
    decompose c tys1 tys2 = zipWithM3_ constrain (polarities c) tys1 tys2

    polarities :: Cons -> [Polarity]
    polarities CArr  = [Contra, Co]
    polarities CPair = [Co, Co]

    constrain :: Polarity -> UType -> UType -> MatchM s ()
    constrain Co     ty1 ty2 = addC (ty1 :<: ty2)
    constrain Contra ty1 ty2 = addC (ty2 :<: ty1)

    zipWithM3 :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
    zipWithM3 _ [] _ _ = return []
    zipWithM3 _ _ [] _ = return []
    zipWithM3 _ _ _ [] = return []
    zipWithM3 f (a:as) (b:bs) (c:cs) = (:) <$> f a b c <*> zipWithM3 f as bs cs

    zipWithM3_ f as bs cs = zipWithM3 f as bs cs >> return ()

-- checkNumTy :: Type -> TC ()
-- checkNumTy TyNat = return ()
-- checkNumTy TyInt = return ()
-- checkNumTy ty    = throwError $ NonNum ty

-- lub :: Type -> Type -> TC Type
-- lub t1 t2 | t1 == t2 = return t1
-- lub TyNat TyInt = return TyInt
-- lub TyInt TyNat = return TyInt
-- lub t1@(TyFun t11 t12) t2@(TyFun t21 t22)
--   | t11 /= t21 = throwError $ NoLub t1 t2
--   | otherwise  = do
--       t2' <- lub t12 t22
--       return $ TyFun t11 t2'



--------------------------------------------------
-- Checking mode

-- check :: Expr -> Type -> TC [SubC]
-- check _ _ = undefined

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
interp' (EBin op ea eb) = do
  va <- interp' ea
  vb <- interp' eb
  case (va, vb) of
    (VInt na, VInt nb) -> return $ VInt (interpOp op na nb)
    _ -> error "Impossible! interp' EBin on non-Ints"
interp' (ELam _ b) = VClosure b <$> ask
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
interp' (EAnn t _) = interp' t

interpOp :: Op -> (Integer -> Integer -> Integer)
interpOp Plus  = (+)
interpOp Minus = (-)

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

fvs :: Type' v -> [v]
fvs (TyVar v) = [v]
fvs TyInt = []
fvs (TyFun ty1 ty2)  = fvs ty1 ++ fvs ty2
fvs (TyPair ty1 ty2) = fvs ty1 ++ fvs ty2

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

-- XXX TODO: each unification variable should say where it came from!
-- e.g. "The type of expression e" or "the input type of function
-- expression e" and so on.

data UVar = UVar String
  deriving (Show, Eq, Ord)

instance Pretty UVar where
  pretty (UVar v) = v

type UType = Type' UVar
type UCtx  = Ctx' UVar

embed :: Type -> UType
embed = translate absurd

--------------------------------------------------
-- Substitutions

newtype Subst = Subst (M.Map UVar (UType, Reason))
  deriving Show

instance Pretty Subst where
  pretty (Subst m) = printf "[%s]" . intercalate ", " . map prettyMapping . M.assocs $ m
    where
      prettyMapping (x, (ty, _)) = printf "%s |-> %s" (pretty x) (pretty ty)

-- XXX FIXME.  What is the reason that should be associated with the result of composition?
(.@) :: Subst -> Subst -> Subst
s2@(Subst m2) .@ Subst m1 = Subst $ M.union (M.map (applySubst s2 *** RSubst undefined s2) m1) m2

idSubst :: Subst
idSubst = Subst M.empty

isEmptySubst :: Subst -> Bool
isEmptySubst (Subst s) = M.null s

(|->) :: UVar -> (UType, Reason) -> Subst
x |-> ty = Subst $ M.singleton x ty

restrictSubst :: [UVar] -> Subst -> Subst
restrictSubst vars (Subst m) = Subst $ M.filterWithKey (\k _ -> k `elem` vars) m

applySubst :: Subst -> UType -> UType
applySubst (Subst s) ty@(TyVar x)
  = case M.lookup x s of
      Nothing      -> ty
      Just (ty',_) -> ty'
applySubst _ TyInt            = TyInt
applySubst s (TyFun ty1 ty2)  = TyFun (applySubst s ty1) (applySubst s ty2)
applySubst s (TyPair ty1 ty2) = TyPair (applySubst s ty1) (applySubst s ty2)

--------------------------------------------------
-- Constraints

{- TODOS/thoughts:

  - Too much reliance on unification variables... reading explanations
    full of unification variables makes it very hard to follow.  Not
    sure how to get around that.  Also ought to somehow suppress some
    "administrative" sorts of substitutions.

  - I think we can probably simplify things, explanation-wise, by
    applying substitutions as *late* as possible.  That is, we should
    accumulate substitutions lazily, and only apply some substitution
    when it is actually necessary to make progress.

    More concretely, keep an accumulated substitution as an extra
    parameter to 'solve'; if a constraint can be decomposed
    structurally, just do so.  If a constraint involves a variable on
    either side of the :=:, first check whether that variable is in
    the domain of the substitution, and expand it if so.

  - We now have a way to explain the constraint solving process.  But
    we also need another data type of reasons that explains steps
    taken in the type inference process, that is, steps taken by the
    'infer' and 'check' functions. Then extend 'infer' and 'check' to
    return reasons as well as a result.  Actually, all this really
    boils down to is that infer and check should return not just types
    but actual *typing derivations*.
-}

{-

Some examples:

>>> eval "(^f : Int -> Int. f <3, 4>) (^x.x+1)"
Can't unify Int and <Int, Int>
- Checking that Int = <Int, Int>
  because the input types of Int -> Int and <Int, Int> -> u5 must match.
    - Checking that Int -> Int = <Int, Int> -> u5
      because it resulted from applying [u1 |-> <Int, Int>] to the constraint Int -> Int = u1 -> u5.
        - Inferred that u1 = <Int, Int>
          because <3, 4> is an argument to a function (namely, f), so its type <Int, Int> must be the same as the function's input type u1.
        - Checking that Int -> Int = u1 -> u5
          because it resulted from applying [u2 |-> u5] to the constraint Int -> Int = u1 -> u2.
            - Inferred that u2 = u5
              because the output types of (Int -> Int) -> u2 and (u3 -> Int) -> u5 must match.
                - Inferred that (Int -> Int) -> u2 = (u3 -> Int) -> u5
                  because it resulted from applying [u4 |-> u3 -> Int] to the constraint (Int -> Int) -> u2 = u4 -> u5.
                    - Inferred that u4 = u3 -> Int
                      because ^x. x + 1 is an argument to a function (namely, ^f : Int -> Int. f <3, 4>), so its type u3 -> Int must be the same as the function's input type u4.
                    - Inferred that (Int -> Int) -> u2 = u4 -> u5
                      because ^f : Int -> Int. f <3, 4> is applied to an argument (namely, ^x. x + 1), so its type ((Int -> Int) -> u2) must be a function type.
            - Checking that Int -> Int = u1 -> u2
              because f is applied to an argument (namely, <3, 4>), so its type (Int -> Int) must be a function type.


>>> eval "(^p. fst p + 3) <<2,5>, 6>"
Can't unify <Int, Int> and Int
- Checking that <Int, Int> = Int
  because it resulted from applying [u2 |-> <Int, Int>] to the constraint u2 = Int.
    - Inferred that u2 = <Int, Int>
      because the first components of <u2, u3> and <<Int, Int>, Int> must match.
        - Inferred that <u2, u3> = <<Int, Int>, Int>
          because the input types of <u2, u3> -> u2 and <<Int, Int>, Int> -> Int must match.
            - Inferred that <u2, u3> -> u2 = <<Int, Int>, Int> -> Int
              because it resulted from applying [u4 |-> <<Int, Int>, Int>] to the constraint <u2, u3> -> u2 = u4 -> Int.
                - Inferred that u4 = <<Int, Int>, Int>
                  because it resulted from applying [u1 |-> <<Int, Int>, Int>] to the constraint u1 = u4.
                    - Inferred that u1 = <<Int, Int>, Int>
                      because the input types of u1 -> Int and <<Int, Int>, Int> -> u7 must match.
                        - Inferred that u1 -> Int = <<Int, Int>, Int> -> u7
                          because it resulted from applying [u6 |-> <<Int, Int>, Int>] to the constraint u1 -> Int = u6 -> u7.
                            - Inferred that u6 = <<Int, Int>, Int>
                              because <<2, 5>, 6> is an argument to a function (namely, ^p. fst p + 3), so its type <<Int, Int>, Int> must be the same as the function's input type u6.
                            - Inferred that u1 -> Int = u6 -> u7
                              because ^p. fst p + 3 is applied to an argument (namely, <<2, 5>, 6>), so its type (u1 -> Int) must be a function type.
                    - Inferred that u1 = u4
                      because p is an argument to a function (namely, fst), so its type u1 must be the same as the function's input type u4.
                - Inferred that <u2, u3> -> u2 = u4 -> Int
                  because it resulted from applying [u5 |-> Int] to the constraint <u2, u3> -> u2 = u4 -> u5.
                    - Inferred that u5 = Int
                      because fst p, which was inferred to have type u5, must also have type Int.
                    - Inferred that <u2, u3> -> u2 = u4 -> u5
                      because fst is applied to an argument (namely, p), so its type (<u2, u3> -> u2) must be a function type.
    - Checking that u2 = Int
      because the output types of <u2, u3> -> u2 and <<Int, Int>, Int> -> Int must match.
        - Checking that <u2, u3> -> u2 = <<Int, Int>, Int> -> Int
          because it resulted from applying [u4 |-> <<Int, Int>, Int>] to the constraint <u2, u3> -> u2 = u4 -> Int.
            - Inferred that u4 = <<Int, Int>, Int>
              because it resulted from applying [u1 |-> <<Int, Int>, Int>] to the constraint u1 = u4.
                - Inferred that u1 = <<Int, Int>, Int>
                  because the input types of u1 -> Int and <<Int, Int>, Int> -> u7 must match.
                    - Inferred that u1 -> Int = <<Int, Int>, Int> -> u7
                      because it resulted from applying [u6 |-> <<Int, Int>, Int>] to the constraint u1 -> Int = u6 -> u7.
                        - Inferred that u6 = <<Int, Int>, Int>
                          because <<2, 5>, 6> is an argument to a function (namely, ^p. fst p + 3), so its type <<Int, Int>, Int> must be the same as the function's input type u6.
                        - Inferred that u1 -> Int = u6 -> u7
                          because ^p. fst p + 3 is applied to an argument (namely, <<2, 5>, 6>), so its type (u1 -> Int) must be a function type.
                - Inferred that u1 = u4
                  because p is an argument to a function (namely, fst), so its type u1 must be the same as the function's input type u4.
            - Checking that <u2, u3> -> u2 = u4 -> Int
              because it resulted from applying [u5 |-> Int] to the constraint <u2, u3> -> u2 = u4 -> u5.
                - Inferred that u5 = Int
                  because fst p, which was inferred to have type u5, must also have type Int.
                - Checking that <u2, u3> -> u2 = u4 -> u5
                  because fst is applied to an argument (namely, p), so its type (<u2, u3> -> u2) must be a function type.


-}

data RawConstraint = UType :=: UType
  deriving Show

instance Pretty RawConstraint where
  pretty (ty1 :=: ty2) = printf "%s = %s" (pretty ty1) (pretty ty2)

data Reason where
  RUnknown :: Reason
  RFun    :: Expr -> Expr -> Reason
  RApp    :: Expr -> Expr -> Reason
  RCheck  :: Expr -> Reason

  RSym    :: Reason -> Reason

  RFunArg  :: RawConstraint -> Reason -> Reason
  RFunRes  :: RawConstraint -> Reason -> Reason
  RPairFst :: RawConstraint -> Reason -> Reason
  RPairSnd :: RawConstraint -> Reason -> Reason

  RSubst  :: RawConstraint -> Subst -> Reason -> Reason
  -- application of substitution

  deriving Show

data Constraint = RawConstraint :? Reason

substConstraints :: Subst -> [Constraint] -> [Constraint]
substConstraints = map . substConstraint
  where
    substConstraint sub (c@(ty1 :=: ty2) :? p)
      = (applySubst sub ty1 :=: applySubst sub ty2)
          :? rSubst c (restrictSubst (fvs ty1 ++ fvs ty2) sub) p
    rSubst c s p
      | isEmptySubst s = p
      | otherwise      = RSubst c s p


instance Pretty Constraint where
  pretty (c :? p) = prettyConstraint c p

withIndent :: Int -> String -> String
withIndent indent s = replicate indent ' ' <> s

prettyConstraint :: RawConstraint -> Reason -> String
prettyConstraint c r = intercalate "\n" $ layoutTree (explainConstraint Check c r)
  where
    layoutTree :: Tree [String] -> [String]
    layoutTree (Node s ts)
      = s ++ concatMap (map (withIndent 4) . layoutTree) ts


data InferMode = Check | Infer
  deriving (Eq, Ord, Show)

explainConstraint :: InferMode -> RawConstraint -> Reason -> Tree [String]
explainConstraint mode c@(ty1 :=: ty2) reason
  = Node
      [ printf "- %s that %s = %s" (showMode mode) (pretty ty1) (pretty ty2)
      , "  because " <> explanation
      ]
      subreasons
  where
    (explanation, subreasons) = prettyReason mode c reason
    showMode Check = "Checking"
    showMode Infer = "Inferred"

prettyReason :: InferMode -> RawConstraint -> Reason -> (String, [Tree [String]])
prettyReason _ _ RUnknown = ("of unknown reason.", [])
prettyReason _ (ty1 :=: _) (RFun e1 e2) =
  ( printf "%s is applied to an argument (namely, %s), so its type (%s) must be a function type."
      (pretty e1) (pretty e2) (pretty ty1)
  , []
  )
prettyReason _ (ty1 :=: ty2) (RApp e1 e2) =
  ( printf "%s is an argument to a function (namely, %s), so its type %s must be the same as the function's input type %s."
      (pretty e2) (pretty e1) (pretty ty1) (pretty ty2)
  , []
  )
prettyReason mode (ty1 :=: ty2) (RCheck e) =
  ( printf "%s, which was inferred to have type %s, must also have type %s."
      (pretty e) (pretty ty1) (pretty ty2)
  , []
  )
  -- XXX explain how we inferred the type of e and why we are checking
  -- the type we are checking

prettyReason mode (ty1 :=: ty2) (RSym r)
  = prettyReason mode (ty2 :=: ty1) r
prettyReason mode _ (RFunArg c@(fun1 :=: fun2) r) =
  ( printf "the input types of %s and %s must match." (pretty fun1) (pretty fun2)
  , [explainConstraint mode c r]
  )
prettyReason mode _ (RFunRes c@(fun1 :=: fun2) r) =
  ( printf "the output types of %s and %s must match." (pretty fun1) (pretty fun2)
  , [explainConstraint mode c r]
  )
prettyReason mode _ (RPairFst c@(p1 :=: p2) r) =
  ( printf "the first components of %s and %s must match." (pretty p1) (pretty p2)
  , [explainConstraint mode c r]
  )
prettyReason mode _ (RPairSnd c@(p1 :=: p2) r) =
  ( printf "the second components of %s and %s must match." (pretty p1) (pretty p2)
  , [explainConstraint mode c r]
  )
prettyReason mode c (RSubst c2@(ty1 :=: ty2) s@(Subst m) r)
  | isEmptySubst s = prettyReason mode c r
  | otherwise      =
    ( printf "it resulted from applying %s to the constraint %s = %s."
        (pretty s) (pretty ty1) (pretty ty2)
    , map (\(x,(ty,r)) -> explainConstraint Infer (TyVar x :=: ty) r) (M.assocs m)
      ++ [explainConstraint mode c2 r]
    )

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
    = printf "%s = %s would result in an infinite type\n" (pretty x) (pretty ty)
   <> prettyConstraint c r
  pretty (CantUnify (c@(ty1 :=: ty2) :? r))
    = printf "Can't unify %s and %s\n" (pretty ty1) (pretty ty2)
   <> prettyConstraint c r

--------------------------------------------------
-- Typing derivations

data Typing' v where
  TDVar  :: Ctx' v -> String -> Type' v -> Typing' v
  TDInt  :: Integer -> Typing' v
  TDBin  :: Op -> Expr -> Typing' v -> Expr -> Typing' v -> Typing' v
  TDFiat :: Type' v -> Typing' v

type Typing = Typing' UVar

getType :: Typing' v -> Type' v
getType (TDVar _ _ ty) = ty
getType (TDInt _)      = TyInt
getType (TDBin _ _ _ _ _) = TyInt
getType (TDFiat ty)    = ty

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

type InferM = ReaderT UCtx (StateT InferState (Except TypeError))

runInferM :: InferM a -> Except TypeError (a, [Constraint])
runInferM
  = (traverse . _2 %~ view constraints)
  . flip runStateT initInferState
  . flip runReaderT M.empty

fresh :: InferM UType
fresh = (TyVar . UVar . head) <$> (nameSupply <%= tail)

withBinding :: String -> UType -> InferM a -> InferM a
withBinding x ty = local (M.insert x ty)

-- For convenience when converting the system without provenance or
-- adding new features, before explaining them.  Eventually there
-- should not be any uses of this operator.
(=?=) :: UType -> UType -> InferM ()
ty1 =?= ty2 = constraints %= (((ty1 :=: ty2) :? RUnknown) :)

(===) :: UType -> UType -> Reason -> InferM ()
(ty1 === ty2) reason = constraints %= (((ty1 :=: ty2) :? reason) :)

infer :: Expr -> InferM Typing
infer (EVar x) = do
  ctx <- ask
  case M.lookup x ctx of
    Just ty -> return (TDVar ctx x ty)
    Nothing -> throwError $ UnboundVar x
infer (EInt n)       = return (TDInt n)
infer (EBin op e1 e2) = do
  check e1 TyInt
  check e2 TyInt  -- XXX need check to return derivation too?
  return (TDBin op e1 undefined e2 undefined)
infer (EApp e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2

  argTy <- fresh
  resTy <- fresh
  (getType ty1 === TyFun argTy resTy) (RFun e1 e2)
  (getType ty2 === argTy)             (RApp e1 e2)
  return (TDFiat resTy)
infer (ELam x margTy body) = do
  argTy <- case margTy of
    Nothing -> fresh
    Just ty -> return (embed ty)
  withBinding x argTy $ do
    resTy <- infer body
    return $ TDFiat $ TyFun argTy (getType resTy)

infer (EPair e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  return $ TDFiat $ TyPair (getType ty1) (getType ty2)

infer EFst = do
  ty1 <- fresh
  ty2 <- fresh
  return $ TDFiat $ TyFun (TyPair ty1 ty2) ty1
infer ESnd = do
  ty1 <- fresh
  ty2 <- fresh
  return $ TDFiat $ TyFun (TyPair ty1 ty2) ty2

-- XXX Need to somehow pass along why we are checking this type
check :: Expr -> UType -> InferM ()
check e ty = do
  ty' <- infer e
  (getType ty' === ty) (RCheck e)

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
solveOne (c@(TyPair ty11 ty12 :=: TyPair ty21 ty22) :? p)
  = return $ Right
      [ (ty11 :=: ty21) :? RPairFst c p
      , (ty12 :=: ty22) :? RPairSnd c p
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
  return $ resolveUTy (applySubst sub (getType uty))

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

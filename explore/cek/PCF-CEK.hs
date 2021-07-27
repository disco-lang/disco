{-# LANGUAGE GADTs #-}

-- Eager PCF, with a CEK machine for evaluation.  Seems to work great.
-- Tail-recursive, doesn't use substitution at all.  Really amounts to
-- a defunctionalized CPS-style recursive interpreter, using an
-- environment rather than substitution.  I expect/hope that it will
-- be decently fast.
--
-- Have to be very careful with environments.  Just remember that each
-- expression that might have free variables needs to have/carry along
-- an environment with bindings for all those free variables.

-- Inspirations:
--
-- PFPL (Harper) chapters 19 and 28.  Chapter 28 taught me to have two
-- modes, "in" and "out".  Note that material like the below can get
-- away without this, because they can just tell syntactically whether
-- an expression is a value (e.g. if it is a lambda).  But in a
-- language like System T or PCF that has succ, we can't tell just
-- from looking at the top level of an expression whether it is a
-- value, and we certainly don't want to traverse the whole term to
-- check every time!  The in/out modes keep track of whether we
-- statically know the currently focused thing to be a value or not.
--
-- https://www.cs.umd.edu/class/summer2015/cmsc330/material/cek/ had
-- some helpful examples about the CEK machine.
--
-- https://www.cs.bham.ac.uk/~hxt/2015/compilers/compiling-functional.pdf
--   taught me to always make sure values and stack frames have any
--   environments they need, rather than pairing an environment with
--   every stack frame etc.  Seems to make the code a bit cleaner,
--   gives fewer opportunities to get environments mixed up, and
--   avoids making up useless environments where they are not needed.

import           Data.List
import           Data.Map               (Map, (!))
import qualified Data.Map               as M
import           Data.Maybe
import           Text.Printf

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token      as T

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

data Type where
  Nat    :: Type
  (:->:) :: Type -> Type -> Type
  deriving (Show, Eq)

data Expr where
  EVar  :: String -> Expr
  EZero :: Expr
  ESucc :: Expr -> Expr
  EIfz  :: Expr -> Expr -> String -> Expr -> Expr
  ELam  :: String -> Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr
  EFix  :: String -> Type -> Expr -> Expr
  deriving Show

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: T.TokenParser u
lexer = T.makeTokenParser
  emptyDef { T.reservedNames = ["z", "s", "ifz", "fix", "is", "let", "in", "N"] }

parens :: Parser a -> Parser a
parens = T.parens lexer

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

symbol = T.symbol lexer

parseAtom :: Parser Expr
parseAtom
  =   EVar  <$> identifier
  <|> EZero <$  reserved "z"
  <|> ESucc <$> (reserved "s" *> parens parseExpr)
  <|> EIfz  <$> (reserved "ifz" *> parseExpr)
            <*> (symbol "{" *> reserved "z" *> symbol "->" *> parseExpr)
            <*> (symbol "|" *> reserved "s" *> parens identifier)
            <*> (symbol "->" *> parseExpr <* symbol "}")
  <|> ELam  <$> (symbol "\\" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (symbol "." *> parseExpr)
  <|> EFix  <$> (reserved "fix" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (reserved "is" *> parseExpr)
  <|> eLet  <$> (reserved "let" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (symbol "=" *> parseExpr)
            <*> (reserved "in" *> parseExpr)
  <|> parens parseExpr

eLet :: String -> Type -> Expr -> Expr -> Expr
eLet x ty e1 e2 = EApp (ELam x ty e2) e1

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (EApp <$ symbol "")   AssocLeft ] ]

parseTypeAtom :: Parser Type
parseTypeAtom =
  (Nat <$ reserved "N") <|> parens parseType

parseType :: Parser Type
parseType = buildExpressionParser table parseTypeAtom
  where
    table = [ [ Infix ((:->:) <$ symbol "->") AssocRight ] ]

expr :: Parser Expr
expr = whiteSpace *> parseExpr <* eof

tm :: String -> Expr
tm s = case parse expr "" s of
  Left err -> error (show err)
  Right e  -> e

ptm :: String -> IO ()
ptm s = case parse expr "" s of
  Left err -> print err
  Right e  -> putStrLn (pretty e)

------------------------------------------------------------
-- Interpreter (CEK machine)
------------------------------------------------------------

-- | The result of evaluating an expression.  A Value always contains
--   any environment necessary with bindings for any free variables it
--   contains.
data Value where
  VZero  :: Value
  VSucc  :: Value -> Value

  -- A lambda together with an Env.
  VClo   :: Env -> String -> Type -> Expr -> Value

  -- With an eager dynamics, we want to specify that only values can
  -- be bound to variables in an environment, which is why we make a
  -- separate data type to represent values.  This also means that
  -- when we look up a variable in the environment, we usually do not
  -- have to evaluate it further; we can return immediately into Out
  -- (continuation) mode.  However, when interpreting a fix expression
  -- we must delay evaluating the recursive call; the name bound by
  -- the fixpoint must map in the environment to a Fix expression.  If
  -- we get one of these out of the environment, we must continue with
  -- In (evaluation) mode, NOT Out mode as we do with other values.
  VFix  :: Env -> String -> Type -> Expr -> Value
  deriving Show

-- | An environment is a mapping of variable names to values.
type Env = Map String Value

emptyEnv :: Env
emptyEnv = M.empty

-- | A frame explains what we are going to do next once we finish
--   evaluating the currently focused expression.  A frame always
--   contains an environment, if necessary, to give meaning to any
--   free variables it contains.
data Frame = FSucc | FIfz Env Expr String Expr | FArg Env Expr | FApp Value
   deriving Show

-- | A continuation is a stack of frames.
type Cont = [Frame]

-- | State of the CEK machine.  If we are going "in", we are focused
--   on an expression to be evaluated in the given context, with a
--   continuation telling us what to do with the value once we obtain
--   it. If we are going "out", we have a value and yield it to the
--   outermost frame of the continuation.
data CEK = In Expr Env Cont | Out Value Cont
  deriving Show

isFinal :: CEK -> Maybe Value
isFinal (Out v []) = Just v
isFinal _          = Nothing

step :: CEK -> CEK
step (In (EVar x) e k)
  = case e!x of
      VFix e x ty t -> In (EFix x ty t) e k
      v             -> Out v k
step (In EZero _ k)                       = Out VZero k
step (In (ESucc t) e k)                   = In t e (FSucc : k)
step (In (EIfz t z x s) e k)              = In t e (FIfz e z x s : k)
step (In (ELam x ty t) e k)               = Out (VClo e x ty t) k
step (In (EApp t1 t2) e k)                = In t1 e (FArg e t2 : k)
step (In t@(EFix x ty t') e k)            = In t' (M.insert x (VFix e x ty t') e) k
step (Out v (FSucc : k))                  = Out (VSucc v) k
step (Out VZero (FIfz e z _ _ : k))       = In z e k
step (Out (VSucc v) (FIfz e _ x s : k))   = In s (M.insert x v e) k
step (Out v1 (FArg e2 t2 : k))            = In t2 e2 (FApp v1 : k)
step (Out v2 (FApp (VClo e1 x _ t1) : k)) = In t1 (M.insert x v2 e1) k

-- Example to show that environments / closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.
--
-- let add : N -> N -> N = \\a:N. fix addA : N -> N is \\b:N. ifz b { z -> a | s(b') -> s(addA b') } in ((\\x:N. (\\f:N->N. add (f x) (f (add x (s(s(s(z)))))))) (s(z))) ((\\x:N. \\y:N. add x y) (s(s(s(z)))))

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

prettyType :: Type -> String
prettyType = prettyTyPrec 0
  where
    prettyTyPrec :: Prec -> Type -> String
    prettyTyPrec _ Nat = "N"
    prettyTyPrec p (ty1 :->: ty2) =
      mparens (p > 0) $ prettyTyPrec 1 ty1 ++ " -> " ++ prettyTyPrec 0 ty2

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

pretty :: Expr -> String
pretty = prettyPrec 0 L

prettyPrec :: Prec -> Associativity -> Expr -> String
prettyPrec _ _ (EVar x)  = x
prettyPrec _ _ EZero     = "z"
prettyPrec _ _ (ESucc e) = "s(" ++ pretty e ++ ")"
prettyPrec p a (EIfz e z x s) =
  "ifz " ++ pretty e ++ " {z -> " ++ pretty z
  ++ " | s(" ++ x ++ ") -> " ++ pretty s ++ "}"
prettyPrec p _ (ELam x ty body) =
  mparens (p>0) $
    "\\" ++ x ++ " : " ++ prettyType ty
         ++ ". " ++ pretty body
prettyPrec p a (EApp e1 e2) =
  mparens (p>2 || (p==2 && a == R)) $
    prettyPrec 2 L e1 ++ " " ++ prettyPrec 2 R e2
prettyPrec p a (EFix x ty e) =
  mparens (p>0) $
    "fix " ++ x ++ " : " ++ prettyType ty ++ " is " ++ pretty e

prettyValue :: Value -> String
prettyValue = pretty . valueToExpr

valueToExpr :: Value -> Expr
valueToExpr VZero           = EZero
valueToExpr (VSucc v)       = ESucc (valueToExpr v)
valueToExpr (VFix _ x ty t) = EFix x ty t
valueToExpr (VClo _ x ty t) = ELam x ty t

prettyCEK :: CEK -> String
prettyCEK (In c e k) = unlines
  [ "▶ " ++ pretty c
  , "  " ++ prettyEnv  e
  , "  " ++ prettyCont k
  ]
prettyCEK (Out v k) = unlines
  [ "◀ " ++ prettyValue v
  , "  " ++ prettyCont k
  ]

prettyEnv
  = mparens True
  . intercalate ", "
  . map prettyBinding
  . M.assocs

prettyBinding (x, v) = x ++ " |-> " ++ prettyValue v

prettyCont
  = (++"]")
  . ("["++)
  . intercalate " ; "
  . map prettyFrame

prettyFrame FSucc = "s(_)"
prettyFrame (FIfz _ z x s) =
  "ifz _ {z -> " ++ pretty z ++ " | s(" ++ x ++ ") -> " ++ pretty s ++ "}"
prettyFrame (FArg _ t2) = "_ " ++ prettyPrec 2 R t2
prettyFrame (FApp v1) = prettyPrec 2 L (valueToExpr v1) ++ " _"

-- ------------------------------------------------------------
-- -- Type checker
-- ------------------------------------------------------------

-- data TypeError where
--   UnboundVar   :: String -> TypeError
--   Mismatch     :: Expr -> Type -> Type -> TypeError
--   MismatchFun  :: Expr -> Type -> TypeError
--   CantInfer    :: Expr -> TypeError
--   NotAFunction :: Expr -> Type -> TypeError

-- prettyTypeError :: TypeError -> String
-- prettyTypeError (UnboundVar x) = "Unbound variable " ++ x
-- prettyTypeError (Mismatch e expected actual)
--   = printf "Type error: %s should have type %s, but has type %s."
--            (pretty e) (prettyType expected) (prettyType actual)
-- prettyTypeError (MismatchFun e expected)
--   = printf "Type error: %s should have type %s, but has a function type."
--            (pretty e) (prettyType expected)
-- prettyTypeError (NotAFunction e ty)
--   = printf "Type error: %s should be a function, but has type %s."
--            (pretty e) (prettyType ty)
-- prettyTypeError (CantInfer e)
--   = printf "Can't infer the type of %s."
--            (pretty e)

-- infer :: Ctx -> Expr -> Either TypeError Type
-- infer ctx (EVar x) =
--   case M.lookup x ctx of
--     Just ty -> return ty
--     Nothing -> Left $ UnboundVar x
-- infer _   (EInt _)      = return TyInt
-- infer ctx (EPlus e1 e2) = do
--   check ctx e1 TyInt
--   check ctx e2 TyInt
--   return TyInt
-- infer ctx (EApp e1 e2) = do
--   (argTy, resTy) <- checkFun ctx e1
--   check ctx e2 argTy
--   return resTy
-- infer ctx (ELam x (Just argTy) body) = do
--   resTy <- infer (M.insert x argTy ctx) body
--   return $ TyFun argTy resTy

-- -- Can't infer type of a bare lambda
-- infer _ e = Left $ CantInfer e

-- check :: Ctx -> Expr -> Type -> Either TypeError ()
-- check ctx e@(ELam x Nothing body) ty =
--   case ty of
--     TyFun argTy resTy -> check (M.insert x argTy ctx) body resTy
--     _                 -> Left $ MismatchFun e ty
-- check ctx e ty = do
--   ty' <- infer ctx e
--   case ty == ty' of
--     True  -> return ()
--     False -> Left $ Mismatch e ty ty'

-- checkFun :: Ctx -> Expr -> Either TypeError (Type, Type)
-- checkFun ctx e = do
--   ty <- infer ctx e
--   case ty of
--     TyFun argTy resTy -> return (argTy, resTy)
--     _                 -> Left $ NotAFunction e ty

------------------------------------------------------------
-- Running
------------------------------------------------------------

exec :: String -> IO ()
exec s = putStrLn (prettyValue (eval (tm s)))

eval :: Expr -> Value
eval = run . initCEK

initCEK :: Expr -> CEK
initCEK t = In t M.empty []

run :: CEK -> Value
run cek = case isFinal cek of
  Just v  -> v
  Nothing -> run (step cek)

steps :: CEK -> [CEK]
steps = takeUntil (isJust . isFinal) . iterate step

takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

showSteps :: String -> IO ()
showSteps s = do
  let ss = steps . initCEK . tm $ s
  mapM_ (putStrLn . prettyCEK) ss
  putStrLn $ "Completed in " ++ show (length ss) ++ " steps."

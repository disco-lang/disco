{-# LANGUAGE GADTs #-}

-- Eager KPCF (PCF + continuations), with a CEK machine for
-- evaluation. See PFPL chapter 30.

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
  Nat    :: Type                      -- N
  (:->:) :: Type -> Type -> Type      -- t1 -> t2
  TCont  :: Type -> Type              -- cont t
  deriving (Show, Eq)

data Expr where
  EVar   :: String -> Expr
  EZero  :: Expr
  ESucc  :: Expr -> Expr
  EIfz   :: Expr -> Expr -> String -> Expr -> Expr
  ELam   :: String -> Type -> Expr -> Expr
  EApp   :: Expr -> Expr -> Expr
  EFix   :: String -> Type -> Expr -> Expr
  ELetCC :: String -> Type -> Expr -> Expr
  EThrow :: Expr -> Expr -> Expr
  ECont  :: Cont -> Expr
  deriving Show

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: T.TokenParser u
lexer = T.makeTokenParser
  emptyDef { T.reservedNames = ["z", "s", "ifz", "fix", "is", "let", "in", "N", "letcc", "throw", "to", "Cont"] }

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
  <|> ELetCC <$> (reserved "letcc" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (reserved "in" *> parseExpr)
  <|> EThrow <$> (reserved "throw" *> parseExpr)
            <*> (reserved "to" *> parseExpr)
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
  (Nat <$ reserved "N")
  <|> (TCont <$> (reserved "Cont" *> parens parseType))
  <|> parens parseType

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

  VFix   :: Env -> String -> Type -> Expr -> Value

  VCont  :: Cont -> Value
  deriving Show

-- | An environment is a mapping of variable names to values.
type Env = Map String Value

emptyEnv :: Env
emptyEnv = M.empty

-- | A frame explains what we are going to do next once we finish
--   evaluating the currently focused expression.  A frame always
--   contains an environment, if necessary, to give meaning to any
--   free variables it contains.
data Frame
  = FSucc | FIfz Env Expr String Expr | FArg Env Expr | FApp Value
  | FThrowTo Env Expr | FThrow Value
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
step (In (ECont k') _ k)                  = Out (VCont k') k
step (In (ELetCC x ty t) e k)             = In t (M.insert x (VCont k) e) k
step (In (EThrow t1 t2) e k)              = In t1 e (FThrowTo e t2 : k)
step (Out v (FSucc : k))                  = Out (VSucc v) k
step (Out VZero (FIfz e z _ _ : k))       = In z e k
step (Out (VSucc v) (FIfz e _ x s : k))   = In s (M.insert x v e) k
step (Out v1 (FArg e2 t2 : k))            = In t2 e2 (FApp v1 : k)
step (Out v2 (FApp (VClo e1 x _ t1) : k)) = In t1 (M.insert x v2 e1) k
step (Out v1 (FThrowTo e t2 : k))         = In t2 e (FThrow v1 : k)
step (Out (VCont k') (FThrow v1 : k))     = Out v1 k'

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

prettyType :: Type -> String
prettyType = prettyTyPrec 0
  where
    prettyTyPrec :: Prec -> Type -> String
    prettyTyPrec _ Nat = "N"
    prettyTyPrec _ (TCont ty) = "Cont(" ++ prettyTyPrec 0 ty ++ ")"
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
prettyPrec p a (ELetCC x _ e) =
  mparens (p>0) $
    "letcc " ++ x ++ " in " ++ pretty e
prettyPrec p a (EThrow e1 e2) =
  mparens (p>0) $
    "throw " ++ pretty e1 ++ " to " ++ pretty e2
prettyPrec p a (ECont k) = prettyCont k

prettyValue :: Value -> String
prettyValue = pretty . valueToExpr

valueToExpr :: Value -> Expr
valueToExpr VZero           = EZero
valueToExpr (VSucc v)       = ESucc (valueToExpr v)
valueToExpr (VFix _ x ty t) = EFix x ty t
valueToExpr (VClo _ x ty t) = ELam x ty t
valueToExpr (VCont k)       = ECont k

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
prettyFrame (FThrowTo _ t) = "throw _ to " ++ pretty t
prettyFrame (FThrow v) = "throw " ++ prettyValue v ++ " to _"

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

------------------------------------------------------------

{- let iter : N -> (N->N) -> N -> N = \\a:N. \\b:N->N. fix it:N -> N is \\n:N. ifz n { z -> a | s(n') -> b(it(n')) } in
   let succ : N -> N = \\n:N.s(n) in
   let add : N -> N -> N = \\a:N. \\b:N. iter b succ a in
   let mul : N -> N -> N = \\a:N. \\b:N. iter z (add b) a in
   let comp : (N -> N) -> (N -> N) -> N -> N = \\f:N -> N. \\g:N -> N. \\x:N. f(g(x))
   let ms : (N -> N) -> N -> N = fix ms : (N -> N) -> N -> N is \\q:N->N. \\n:N. ifz n { z -> s(z) | s(n') -> mul (q z) (ms (comp q succ) n') } in ms (\\x:N.s(x)) (s(s(s(s(z)))))
-}

-- Takes 843 steps to evaluate 0*1*2*3 to get 0 (non-short-circuiting):
-- showSteps "let iter : N -> (N->N) -> N -> N = \\a:N. \\b:N->N. fix it:N -> N is \\n:N. ifz n { z -> a | s(n') -> b(it(n')) } in let succ : N -> N = \\n:N.s(n) in let add : N -> N -> N = \\a:N. \\b:N. iter b succ a in let mul : N -> N -> N = \\a:N. \\b:N. iter z (add b) a in let comp : (N -> N) -> (N -> N) -> N -> N = \\f:N -> N. \\g:N -> N. \\x:N. f(g(x)) in let ms : (N -> N) -> N -> N = fix ms : (N -> N) -> N -> N is \\q:N->N. \\n:N. ifz n { z -> s(z) | s(n') -> mul (q z) (ms (comp q succ) n') } in ms (\\x:N.x) (s(s(s(s(z)))))"

-- Takes only 77 steps to evaluate the same product but short-circuits at
-- the first 0, using continuations:
-- showSteps "let iter : N -> (N->N) -> N -> N = \\a:N. \\b:N->N. fix it:N -> N is \\n:N. ifz n { z -> a | s(n') -> b(it(n')) } in let succ : N -> N = \\n:N.s(n) in let add : N -> N -> N = \\a:N. \\b:N. iter b succ a in let mul : N -> N -> N = \\a:N. \\b:N. iter z (add b) a in let comp : (N -> N) -> (N -> N) -> N -> N = \\f:N -> N. \\g:N -> N. \\x:N. f(g(x)) in (\\q:N -> N. \\n:N. letcc ret : Cont(N) in let ms:(N -> N) -> N -> N = fix ms:(N -> N) -> N -> N is \\q: N -> N. \\n:N. ifz n {z -> s(z) | s(n') -> ifz (q z) { z -> throw z to ret | s(n'') -> mul (q z) (ms (comp q succ) n')} } in ms q n) (\\x:N.x) (s(s(s(s(z)))))"

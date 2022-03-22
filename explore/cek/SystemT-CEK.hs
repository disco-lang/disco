{-# LANGUAGE GADTs #-}

-- Gödel's System T, with an (untyped) CEK machine for evaluation.

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
  ERec  :: Expr -> Expr -> String -> String -> Expr -> Expr
  ELam  :: String -> Type -> Expr -> Expr
  EApp  :: Expr -> Expr -> Expr
  ELet  :: String -> Type -> Expr -> Expr -> Expr
  deriving Show

data Value where
  VZero  :: Value
  VSucc  :: Value -> Value
  VLam   :: String -> Type -> Expr -> Value

  -- For technical reasons only, see discussion below
  VEmbed :: Expr -> Value

-- Don't want to do this, especially with eager semantics!
-- Inefficient to recurse through the whole term at every step.  Just
-- run the machine and it will ensure the result is a value,
-- traversing it only once.
-- isValue :: Expr -> Bool
-- isValue EZero     = True
-- isValue (ESucc e) = isValue e
-- isValue (ELam{})  = True
-- isValue _         = False

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: T.TokenParser u
lexer = T.makeTokenParser
  emptyDef { T.reservedNames = ["z", "s", "with", "rec", "let", "in", "N"] }

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
  <|> ERec  <$> (reserved "rec" *> parseExpr)
            <*> (symbol "{" *> reserved "z" *> symbol "->" *> parseExpr)
            <*> (symbol "|" *> reserved "s" *> parens identifier)
            <*> (reserved "with" *> identifier)
            <*> (symbol "->" *> parseExpr <* symbol "}")
  <|> ELam  <$> (symbol "\\" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (symbol "." *> parseExpr)
  <|> ELet  <$> (reserved "let" *> identifier)
            <*> (symbol ":" *> parseType)
            <*> (symbol "=" *> parseExpr)
            <*> (reserved "in" *> parseExpr)
  <|> parens parseExpr

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
  where
    prettyPrec :: Prec -> Associativity -> Expr -> String
    prettyPrec _ _ (EVar x)  = x
    prettyPrec _ _ EZero     = "z"
    prettyPrec _ _ (ESucc e) = "s(" ++ pretty e ++ ")"
    prettyPrec p a (ERec e z x y s) =
      "rec " ++ pretty e ++ " {z -> " ++ pretty z
      ++ " | s(" ++ x ++ ") with " ++ y ++ " -> " ++ pretty s ++ "}"
    prettyPrec p _ (ELam x ty body) =
      mparens (p>0) $
        "\\" ++ x ++ " : " ++ prettyType ty
             ++ ". " ++ pretty body
    prettyPrec _ _ (ELet x ty e1 e2) =
      "let " ++ x ++ " : " ++ prettyType ty ++ " = " ++ pretty e1 ++ " in " ++ pretty e2
    prettyPrec p a (EApp e1 e2) =
      mparens (p>2 || (p==2 && a == R)) $
        (prettyPrec 2 L e1 ++ " " ++ prettyPrec 2 R e2)

prettyCEK :: CEK -> String
prettyCEK (CEK m c e k) = unlines
  [ prettyMode m ++ " " ++ pretty c
  , "  " ++ prettyEnv  e
  , "  " ++ prettyCont k
  ]

prettyMode In  = "▶"
prettyMode Out = "◀"

prettyEnv
  = mparens True
  . intercalate ", "
  . map prettyBinding
  . M.assocs

prettyBinding (x, clo) = x ++ " |-> " ++ prettyClosure clo

prettyClosure (Clo t _) = pretty t ++ " @_"

prettyCont
  = (++"]")
  . ("["++)
  . intercalate " ; "
  . map (\(f,_) -> prettyFrame f)

prettyFrame FSucc = "s(_)"
prettyFrame (FRec z x y s) =
  "rec _ {z -> " ++ pretty z ++ " | s(" ++ x ++ ") with " ++ y ++ " -> " ++ pretty s ++ "}"
prettyFrame (FArg t2) = "_ " ++ pretty t2
prettyFrame (FApp t1) = pretty t1 ++ " _"
prettyFrame (FLet x t2) = "let " ++ x ++ " = _ in " ++ pretty t2

-- data Frame = FSucc | FRec Expr String String Expr | FArg Expr | FApp Expr | FLet String Expr

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
-- Interpreter (untyped CEK machine)
------------------------------------------------------------

data Closure = Clo Value Env  deriving Show
type Env = Map String Closure
type Kont = [(Frame, Env)]
data Frame = FSucc | FRec Expr String String Expr | FArg Expr | FApp Expr | FLet String Expr
   deriving Show

data Control = In Expr | Out Value deriving Show
data CEK = CEK Control Env Kont deriving Show

isFinal :: CEK -> Maybe Expr
isFinal (CEK (Out t) _ []) = Just t
isFinal _                  = Nothing

exec :: String -> IO ()
exec s = putStrLn (prettyValue (eval (tm s)))

eval :: Expr -> Value
eval t = run (CEK (In t) M.empty [])

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

step :: CEK -> CEK
step (CEK (In EVar x) e k) = case e!x of
  Clo (VEmbed t) e' -> CEK (In t) e' k
  Clo v e'          -> CEK (Out v) e' k
step (CEK (In EZero) e k)            = CEK (Out VZero) e k
step (CEK (In (ESucc t)) e k)        = CEK (In  t) e ((FSucc, e) : k)
step (CEK (In (ERec t z x y s)) e k) = CEK (In  t) e ((FRec z x y s, e) : k)
step (CEK In (ELam x _ t) e k)       = CEK (Out (VLam x t)) e k
step (CEK (In (EApp t1 t2)) e k)     = CEK (In t1) e ((FArg t2, e) : k)
step (CEK (In (ELet x _ t1 t2)) e k) = CEK (In t1) e ((FLet x t2, e) : k)

step (CEK (Out v) e ((f, e') : k))   = CEK c (ext e') (maybe k (\f' -> ((f',e'):k)) mf)
  where
    (c, ext, mf) = contFrame f e v

fout v = (Out v, id, Nothing)
fin  t = (In  t, id, Nothing)

contFrame :: Frame -> Env -> Value -> (Mode, Env -> Env, Maybe Frame)
contFrame FSucc _ t                = fout (VSucc t)
contFrame (FRec z _ _ _) _ EZero   = fin z
-- contFrame (FRec z x y s) e (ESucc p) = (In (ERec p z x y s), id, Just (FRecS x y s

--                                     e |--> e'
-- --------------------------------------------------------------------------------
-- rec e { z -> Z | s(p) with y -> S }   |-->  rec e' { z -> S | s(p) with y -> S }
--
-- ---------------------------------------------
-- rec z { z -> Z | s(p) with y -> S }   |-->  Z
--
-- --------------------------------------------------------
-- rec (s(e)) { z -> Z | s(p) with y -> S }
--   |-->  [e/p, (rec e { z -> Z | s(p) with y -> S})/y](S)
--
-- This is weird.  Eager semantics suggests we should reduce the
-- argument to rec completely before doing anything else --- but then
-- we end up traversing it twice, once to reduce and once to do the
-- recursion.  And we would have to distinguish the recursive calls
-- somehow so they don't just end up re-traversing.  Above rules make
-- more sense---just reduce to WHNF so we know which branch to take.
--
-- Aha!  PFPL 9.2 specifies that the recursive call is substituted
-- *unevaluated*.  Not sure how to translate it into a CEK machine
-- model!  I was hoping everything in the environment would always be
-- values, but apparently we can't do that in this case...  Maybe just
-- embed expressions as a special type of "value".  That way we can
-- still avoid re-evaluating most values when taking them out of the
-- environment.
--
-- Note we probably have to do the same thing when interpreting a
-- 'fix' expression in PCF.

                                        -- M.insert y (Clo (ERec t z x y s) e) . M.insert x (Clo t e), Nothing)
contFrame (FArg t2) _ t            = (In, t2, id, Just (FApp t))
contFrame (FApp (ELam x _ t)) e t2 = (In, t, M.insert x (Clo t2 e), Nothing)
contFrame (FLet x t2) e t1         = (In, t2, M.insert x (Clo t1 e), Nothing)

-- -- Example to show that closures are working properly:
-- --
-- -- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
-- --
-- -- should yield 11 when evaluated.  Possible wrong results would be
-- -- (a) crash, or (b) 7.

-- main :: IO ()
-- main = getLine >>= eval

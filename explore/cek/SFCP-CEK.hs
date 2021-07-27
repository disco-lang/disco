{-# LANGUAGE GADTs #-}

-- Eager FCP + suspension (lazy) types with a CEK machine.  See PFPL
-- chapter 36.

import           Data.Bifunctor         (first)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
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
  Void   :: Type
  (:+:)  :: Type -> Type -> Type
  Unit   :: Type
  (:*:)  :: Type -> Type -> Type
  (:->:) :: Type -> Type -> Type
  TVar   :: String -> Type
  Rec    :: String -> Type -> Type
  Lazy   :: Type -> Type
  deriving (Show, Eq)

data Side = L | R deriving Show

data Expr where
  EVar    :: String -> Expr
  EInj    :: Side -> Expr -> Expr
  ECase   :: Expr -> String -> Expr -> String -> Expr -> Expr
  EUnit   :: Expr
  EPair   :: Expr -> Expr -> Expr
  EProj   :: Side -> Expr -> Expr
  ELam    :: String -> Type -> Expr -> Expr
  EApp    :: Expr -> Expr -> Expr
  EAscr   :: Type -> Expr -> Expr

  -- Don't need these in isorecursive system.
  EFold   :: Expr -> Expr
  EUnfold :: Expr -> Expr

  -- Delay + force give us by-need laziness, and also self-reference.
  EDelay  :: String -> Expr -> Expr
  EForce  :: Expr -> Expr

  ECell   :: Int -> Expr   -- Not in the surface language, only for
                           -- evaluating 'delay'.

    -- Actually possible to derive fix from fold, unfold + recursive
    -- types (see PFPL 20.3).  But we can also derive it in a nice way
    -- from delay, which includes a recursive binding.  This makes
    -- sense because once we have indirections to memory cells, we can
    -- get self-reference just by storing a reference to a memory cell
    -- within the memory cell itself.

  deriving Show

  -- (fix x:T. e) : T
  -- (delay x:Lazy T. e) : Lazy T   when  x : Lazy T |- e : T

  -- so can we just define
  --   (fix x:T. e) === force(delay x:Lazy T. [force(x)/x]e)

  -- See the parser below.

------------------------------------------------------------
-- Parser
------------------------------------------------------------

lexer :: T.TokenParser u
lexer = T.makeTokenParser
  emptyDef { T.reservedNames = ["abort", "inl", "inr", "case", "unit", "fst", "snd", "fold", "unfold", "delay", "let", "letrec", "in", "fix", "force", "Void", "Unit", "Rec", "Lazy", "N", "λ"] }

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
  =   EInj <$> ((L <$ reserved "inl") <|> (R <$ reserved "inr")) <*> parseAtom
  <|> ECase
      <$> (reserved "case" *> parseExpr)
      <*> (symbol "{" *> reserved "inl" *> identifier)
      <*> (symbol "->" *> parseExpr)
      <*> (symbol "|" *> reserved "inr" *> identifier)
      <*> (symbol "->" *> parseExpr <* symbol "}")
  <|> EUnit <$ reserved "unit"
  <|> EPair <$> (symbol "<" *> parseExpr) <*> (symbol "," *> parseExpr <* symbol ">")
  <|> EProj <$> ((L <$ reserved "fst") <|> (R <$ reserved "snd")) <*> parseAtom
  <|> ELam
      <$> ((symbol "\\" <|> symbol "λ") *> identifier)
      <*> (symbol ":" *> parseType)
      <*> (symbol "." *> parseExpr)
  <|> EAscr <$> (reserved "as" *> parseTypeAtom) <*> parseAtom
  <|> EFold <$> (reserved "fold" *> parseAtom)
  <|> EUnfold <$> (reserved "unfold" *> parseAtom)
  <|> EDelay
      <$> (reserved "delay" *> identifier)
      <*> (symbol "." *> parseExpr)
  <|> EForce <$> (reserved "force" *> parseAtom)
  <|> eFix
      <$> (reserved "fix" *> identifier)
      <*> (symbol ":" *> parseType)
      <*> (symbol "." *> parseExpr)
  <|> eLet
      <$> (reserved "let" *> identifier)
      <*> (symbol ":" *> parseType)
      <*> (symbol "=" *> parseExpr)
      <*> (reserved "in" *> parseExpr)
  <|> eLetrec
      <$> (reserved "letrec" *> identifier)
      <*> (symbol ":" *> parseType)
      <*> (symbol "=" *> parseExpr)
      <*> (reserved "in" *> parseExpr)
  <|> EVar   <$> identifier
  <|> parens parseExpr

eLet :: String -> Type -> Expr -> Expr -> Expr
eLet x ty e1 e2 = EApp (ELam x ty e2) e1

eLetrec :: String -> Type -> Expr -> Expr -> Expr
eLetrec x ty e1 e2 = eLet x ty (eFix x ty e1) e2

eFix :: String -> Type -> Expr -> Expr
eFix x ty t = EForce (EDelay x (subst x (EForce (EVar x)) t))

subst :: String -> Expr -> Expr -> Expr
subst x s t@(EVar y)
  | x == y = s
  | otherwise = t
subst x s (EInj i t) = EInj i (subst x s t)
subst x s (ECase t x1 t1 x2 t2)
  = ECase (subst x s t)
      x1 (if x == x1 then t1 else subst x s t1)
      x2 (if x == x2 then t2 else subst x s t2)
subst _ _ EUnit = EUnit
subst x s (EPair t1 t2) = EPair (subst x s t1) (subst x s t2)
subst x s (EProj i t)   = EProj i (subst x s t)
subst x s t@(ELam y ty t')
  | x == y = t
  | otherwise = ELam y ty (subst x s t')
subst x s (EApp t1 t2) = EApp (subst x s t1) (subst x s t2)
subst x s (EAscr ty t) = EAscr ty (subst x s t)
  -- var binding in EFold is for *type* vars so it doesn't affect subst here
subst x s (EFold t) = EFold (subst x s t)
subst x s (EUnfold t) = EUnfold (subst x s t)
subst x s t@(EDelay y t')
  | x == y = t
  | otherwise = EDelay y (subst x s t')
subst x s (EForce t) = EForce (subst x s t)
subst _ _ (ECell n) = ECell n

substTy :: String -> Type -> Type -> Type
substTy _ _ Void = Void
substTy _ _ Unit = Unit
substTy x s (ty1 :+: ty2) = substTy x s ty1 :+: substTy x s ty2
substTy x s (ty1 :*: ty2) = substTy x s ty1 :*: substTy x s ty2
substTy x s (ty1 :->: ty2) = substTy x s ty1 :->: substTy x s ty2
substTy x s (TVar y)
  | x == y = s
  | otherwise = TVar y
substTy x s (Rec y ty)
  | x == y = Rec y ty
  | otherwise = Rec y (substTy x s ty)
substTy x s (Lazy ty) = Lazy (substTy x s ty)

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ Infix (EApp <$ symbol "")   AssocLeft ] ]

parseTypeAtom :: Parser Type
parseTypeAtom
  =   Void <$ reserved "Void"
  <|> Unit <$ reserved "Unit"
  <|> (Rec "t" (Unit :+: TVar "t")) <$ reserved "N"
  <|> TVar <$> identifier
  <|> Rec  <$> (reserved "Rec" *> identifier) <*> (symbol "." *> parseType)
  <|> Lazy <$> (reserved "Lazy" *> parseTypeAtom)
  <|> parens parseType

parseType :: Parser Type
parseType = buildExpressionParser table parseTypeAtom
  where
    table =
      [ [ Infix ((:->:) <$ symbol "->") AssocRight ]
      , [ Infix ((:*:) <$ symbol "*") AssocRight ]
      , [ Infix ((:+:) <$ symbol "+") AssocRight ]
      ]

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
  VInj  :: Side -> Value -> Value
  VUnit :: Value
  VPair :: Value -> Value -> Value
  VClo  :: Env -> String -> Type -> Expr -> Value
  VFold :: Value -> Value
  VCell :: Int -> Value
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
  = FInj Side
  | FCase Env String Expr String Expr
  | FPairL Env Expr
  | FPairR Value
  | FProj Side
  | FArg Env Expr
  | FApp Value
  | FFold
  | FUnfold
  | FForce
  | FUpdate Int
  deriving Show

-- | A continuation is a stack of frames.
type Cont = [Frame]

data Cell = Blackhole | E Env Expr | V Value
  deriving Show

data Mem = Mem { next :: Int, mu :: IntMap Cell }
  deriving Show

allocate :: Env -> Expr -> Mem -> (Mem, Int)
allocate e t (Mem n m) = (Mem (n+1) (IntMap.insert n (E e t) m), n)

lkup :: Int -> Mem -> Maybe Cell
lkup n (Mem _ m) = IntMap.lookup n m

set :: Int -> Cell -> Mem -> Mem
set n c (Mem nxt m) = Mem nxt (IntMap.insert n c m)

-- | State of the CESK machine.  If we are going "in", we are focused
--   on an expression to be evaluated in the given context, with a
--   continuation telling us what to do with the value once we obtain
--   it. If we are going "out", we have a value and yield it to the
--   outermost frame of the continuation.
data CESK = In Expr Env Mem Cont | Out Value Mem Cont
  deriving Show

isFinal :: CESK -> Maybe Value
isFinal (Out v _ []) = Just v
isFinal _            = Nothing

step :: CESK -> CESK
step (In (EVar x) e m k) = Out (e!x) m k
step (In (EInj s t) e m k)                      = In t e m (FInj s : k)
step (In (ECase t x1 t1 x2 t2) e m k)           = In t e m (FCase e x1 t1 x2 t2 : k)
step (In EUnit e m k)                           = Out VUnit m k
step (In (EPair t1 t2) e m k)                   = In t1 e m (FPairL e t2 : k)
step (In (EProj s t) e m k)                     = In t e m (FProj s : k)
step (In (ELam x ty t) e m k)                   = Out (VClo e x ty t) m k
step (In (EApp t1 t2) e m k)                    = In t1 e m (FArg e t2 : k)
step (In (EAscr _ t) e m k)                     = In t e m k
step (In (EFold t) e m k)                       = In t e m (FFold : k)
step (In (EUnfold t) e m k)                     = In t e m (FUnfold : k)
step (In (EDelay x t) e m k)                    = Out (VCell loc) m' k
  where
    (m', loc) = allocate (M.insert x (VCell loc) e) t m
step (In (EForce t) e m k)                      = In t e m (FForce : k)

step (Out v m (FInj s : k))                     = Out (VInj s v) m k
step (Out (VInj L v) m (FCase e x1 t1 _ _ : k)) = In t1 (M.insert x1 v e) m k
step (Out (VInj R v) m (FCase e _ _ x2 t2 : k)) = In t2 (M.insert x2 v e) m k
step (Out v1 m (FPairL e t2 : k))               = In t2 e m (FPairR v1 : k)
step (Out v2 m (FPairR v1 : k))                 = Out (VPair v1 v2) m k
step (Out (VPair v1 _) m (FProj L : k))         = Out v1 m k
step (Out (VPair _ v2) m (FProj R : k))         = Out v2 m k
step (Out v1 m (FArg e2 t2 : k))                = In t2 e2 m (FApp v1 : k)
step (Out v2 m (FApp (VClo e1 x ty t1) : k))    = In t1 (M.insert x v2 e1) m k
step (Out v m (FFold : k))                      = Out (VFold v) m k
step (Out (VFold v) m (FUnfold : k))            = Out v m k
step (Out (VCell n) m (FForce : k))             =
  case lkup n m of
    Nothing        -> undefined
    Just (V v)     -> Out v m k
    Just (E e t)   -> In t e (set n Blackhole m) (FUpdate n : k)
    Just Blackhole -> error "Infinite loop detected!"
step (Out v m (FUpdate n : k)) = Out v (set n (V v) m) k

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

prettyType :: Type -> String
prettyType = prettyTyPrec 0 LA
  where
    prettyTyPrec :: Prec -> Associativity -> Type -> String
    prettyTyPrec _ _ Void = "Void"
    prettyTyPrec _ _ Unit = "Unit"
    prettyTyPrec p a (ty1 :->: ty2) =
      mparens (p > 1 || p == 1 && a == RA) $
        prettyTyPrec 1 LA ty1 ++ " -> " ++ prettyTyPrec 1 RA ty2
    prettyTyPrec p a (ty1 :+: ty2) =
      mparens (p > 2 || p == 2 && a == LA) $
        prettyTyPrec 2 LA ty1 ++ " + " ++ prettyTyPrec 2 RA ty2
    prettyTyPrec p a (ty1 :*: ty2) =
      mparens (p > 3 || p == 3 && a == LA) $
        prettyTyPrec 3 LA ty1 ++ " * " ++ prettyTyPrec 3 RA ty2
    prettyTyPrec _ _ (TVar x) = x
    prettyTyPrec p _ (Rec x ty) =
      mparens (p > 0) $ "rec " ++ x ++ ". " ++ prettyType ty
    prettyTyPrec p _ (Lazy ty) =
      "Lazy " ++ prettyType ty

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
mparens False = id

data Associativity = LA | RA
  deriving (Show, Eq)

pretty :: Expr -> String
pretty = prettyPrec 0 LA

prettyPrec :: Prec -> Associativity -> Expr -> String
prettyPrec _ _ (EVar x)   = x
prettyPrec p a (EInj L t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "inl " ++ prettyPrec 2 RA t
prettyPrec p a (EInj R t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "inr " ++ prettyPrec 2 RA t
prettyPrec _ _ (ECase t x1 t1 x2 t2)
  = "case " ++ pretty t ++ " { inl " ++ x1 ++ " -> " ++ pretty t1
  ++ " | inr " ++ x2 ++ " -> " ++ pretty t2 ++ " }"
prettyPrec _ _ EUnit = "unit"
prettyPrec _ _ (EPair t1 t2) = "<" ++ pretty t1 ++ ", " ++ pretty t2 ++ ">"
prettyPrec p a (EProj L t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "fst " ++ prettyPrec 2 RA t
prettyPrec p a (EProj R t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "snd " ++ prettyPrec 2 RA t
prettyPrec p _ (ELam x ty body) =
  mparens (p>0) $
    "λ " ++ x ++ " : " ++ prettyType ty ++ ". " ++ pretty body
prettyPrec p a (EApp e1 e2) =
  mparens (p>2 || (p==2 && a == RA)) $
    prettyPrec 2 LA e1 ++ " " ++ prettyPrec 2 RA e2
prettyPrec p a (EAscr ty t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "as (" ++ prettyType ty ++ ") " ++ prettyPrec 2 RA t
prettyPrec p a (EFold t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "fold " ++ prettyPrec 2 RA t
prettyPrec p a (EUnfold t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "unfold " ++ prettyPrec 2 RA t
prettyPrec p a (EDelay x e) =
  mparens (p>2 || (p==2 && a == RA)) $
    "delay " ++ x ++ ". " ++ pretty e
prettyPrec p a (EForce t) =
  mparens (p>2 || (p==2 && a == RA)) $
    "force " ++ prettyPrec 2 RA t
prettyPrec _ _ (ECell n) = show n

prettyValue :: Value -> String
prettyValue = pretty . valueToExpr

valueToExpr :: Value -> Expr
valueToExpr (VInj s v)      = EInj s (valueToExpr v)
valueToExpr VUnit           = EUnit
valueToExpr (VPair v1 v2)   = EPair (valueToExpr v1) (valueToExpr v2)
valueToExpr (VClo _ x ty t) = ELam x ty t
valueToExpr (VFold v)       = EFold (valueToExpr v)
valueToExpr (VCell n)       = ECell n

prettyCESK :: CESK -> String
prettyCESK (In c e m k) = unlines $
  [ "▶ " ++ pretty c ] ++
  [         prettyEnv  e | not (M.null e) ] ++
  [ "  " ++ prettyMem  m | not (IntMap.null (mu m)) ] ++
  [ "  " ++ prettyCont k ]
prettyCESK (Out v m k) = unlines $
  [ "◀ " ++ prettyValue v ] ++
  [ "  " ++ prettyMem m | not (IntMap.null (mu m)) ] ++
  [ "  " ++ prettyCont k ]

prettyEnv
  = unlines
  . map (("  "++) . prettyBinding)
  . M.assocs
  where
    prettyBinding (x, v) = x ++ " |-> " ++ prettyValue v

prettyMem (Mem _ m)
  = mparens True
  . intercalate ", "
  . map prettyBinding
  . IntMap.assocs
  $ m
  where
    prettyBinding (n, c) = show n ++ " |-> " ++ prettyCell c

prettyCell Blackhole = "⬤"
prettyCell (E _ t)   = "~ " ++ pretty t
prettyCell (V v)     = "! " ++ prettyValue v

prettyCont
  = (++"]")
  . ("["++)
  . intercalate " ; "
  . map prettyFrame

prettyFrame (FInj L)      = "inl _"
prettyFrame (FInj R)      = "inr _"
prettyFrame (FCase _ x1 t1 x2 t2)
                          = "case _ { inl " ++ x1 ++ " -> " ++ pretty t1 ++ " | inr " ++ x2 ++ " -> " ++ pretty t2 ++ " }"
prettyFrame (FPairL _ t2) = "<_, " ++ pretty t2 ++ ">"
prettyFrame (FPairR v)    = "<" ++ prettyValue v ++ ", _>"
prettyFrame (FProj L)     = "fst _"
prettyFrame (FProj R)     = "snd _"
prettyFrame (FArg _ t2)   = "_ " ++ prettyPrec 2 RA t2
prettyFrame (FApp v1)     = prettyPrec 2 LA (valueToExpr v1) ++ " _"
prettyFrame FFold         = "fold _"
prettyFrame FUnfold       = "unfold _"
prettyFrame FForce        = "force _"
prettyFrame (FUpdate n)   = "update " ++ show n

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

-- No type checker for now, just untyped.  I was mostly just trying to
-- figure out how the dynamics works.

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

-- type Ctx = Map String Type

-- infer :: Ctx -> Expr -> Either TypeError Type
-- infer ctx (EVar x) =
--   case M.lookup x ctx of
--     Just ty -> return ty
--     Nothing -> Left $ UnboundVar x
-- infer ctx (ECase t x1 t1 x2 t2) = do
--   (ty1, ty2) <- checkSum ctx t
--   ty  <- infer (M.insert x1 ty1 ctx) t1
--   ty' <- infer (M.insert x2 ty2 ctx) t2
--   checkEqual ty ty'
-- infer _ EUnit = return Unit
-- infer ctx (EPair t1 t2) = (:*:) <$> infer ctx t1 <*> infer ctx t2
-- infer ctx (EProj s t) = do
--   (ty1, ty2) <- checkProd ctx t
--   case s of
--     L -> return ty1
--     R -> return ty2
-- infer ctx (ELam x ty t) = (ty :->:) <$> infer (M.insert x ty ctx) t
-- infer ctx (EApp t1 t2) = do
--   (argTy, resTy) <- checkFun ctx t1
--   check ctx t2 argTy
--   return resTy
-- infer ctx (EAscr ty t) =
--   check ctx ty t
--   return ty
-- infer ctx (EUnfold t) =
--   (t, ty) <- checkRec ctx t
--   return (substTy t (Rec t ty) ty)
-- infer ctx (EDelay x t) =


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
eval = run . initCESK

initCESK :: Expr -> CESK
initCESK t = In t M.empty (Mem 0 IntMap.empty) []

run :: CESK -> Value
run cek = case isFinal cek of
  Just v  -> v
  Nothing -> run (step cek)

steps :: CESK -> [CESK]
steps = takeUntil (isJust . isFinal) . iterate step

takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

showSteps :: String -> IO ()
showSteps s = do
  let ss = steps . initCESK . tm $ s
  mapM_ (putStrLn . prettyCESK) ss
  putStrLn $ "Completed in " ++ show (length ss) ++ " steps."

showStepsFrom :: String -> IO ()
showStepsFrom fileName = readFile fileName >>= showSteps

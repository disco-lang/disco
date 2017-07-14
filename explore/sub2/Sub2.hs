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

import Debug.Trace

import Data.Coerce

import           Parsing2

import Prelude hiding (lookup)
import qualified Prelude as P

import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import Control.Lens (anyOf, toListOf, each, (^..), over, both)
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
import           Data.Tuple
import           Data.Maybe
import           Data.Void
import           Text.Printf

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS as G (condensation, topsort', components)

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
  EFst  :: Expr
  ESnd  :: Expr

  deriving (Show, Generic)

data Atom where
  AVar :: Name Type -> Atom
  ANat :: Atom
  AInt :: Atom
  deriving (Show, Eq, Ord, Generic)

isVar :: Atom -> Bool
isVar (AVar _) = True
isVar _        = False

isBase :: Atom -> Bool
isBase = not . isVar

isSub :: Atom -> Atom -> Bool
isSub a1 a2 | a1 == a2 = True
isSub ANat AInt = True
isSub _ _ = False

aglb2 :: Atom -> Atom -> Maybe Atom
aglb2 a1 a2     | a1 == a2 = Just a1
aglb2 AInt ANat = Just ANat
aglb2 ANat AInt = Just ANat
aglb2 _ _       = Nothing

aglb :: [Atom] -> Maybe Atom
aglb []  = Nothing
aglb [a] = Just a
aglb (a:as) = do
  g <- aglb as
  aglb2 a g

alub2 :: Atom -> Atom -> Maybe Atom
alub2 a1 a2     | a1 == a2 = Just a1
alub2 AInt ANat = Just AInt
alub2 ANat AInt = Just AInt
alub2 _ _       = Nothing

alub :: [Atom] -> Maybe Atom
alub []  = Nothing
alub [a] = Just a
alub (a:as) = do
  g <- alub as
  alub2 a g

data Cons where
  CArr  :: Cons
  CPair :: Cons
  -- F     :: Cons  -- XXX for testing
  -- G     :: Cons
  -- H     :: Cons
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

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

instance Subst Atom Atom where
  isvar (AVar x) = Just (SubstName (coerce x))
  isvar _        = Nothing

instance Subst Type Cons
instance Subst Type Atom
instance Subst Type Type where
  isvar (TyAtom (AVar x)) = Just (SubstName x)
  isvar _                 = Nothing

-- orphans
instance (Ord a, Subst t a) => Subst t (Set a) where
  subst x t = S.map (subst x t)
  substs s  = S.map (substs s)
instance (Ord k, Subst t a) => Subst t (Map k a) where
  subst x t = M.map (subst x t)
  substs s  = M.map (substs s)

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

compose :: Subst a a => [S' a] -> S' a
compose = foldr (@@) idS

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
unify = unify' (==)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations equal *up to* identifying all base
--   types.  So, for example, Int = Nat weakly unifies but Int = (Int
--   -> Int) does not.  This is used to check whether subtyping
--   constraints are structurally sound before doing constraint
--   simplification/solving.
weakUnify :: [Eqn] -> Maybe S
weakUnify = unify' (\_ _ -> True)

-- | Given a list of equations between types, return a substitution
--   which makes all the equations satisfied (or fail if it is not
--   possible), up to the given comparison on base types.
unify' :: (Atom -> Atom -> Bool) -> [Eqn] -> Maybe S
unify' _ [] = Just idS
unify' atomEq (e:es) = do
  u <- unifyOne atomEq e
  case u of
    Left sub    -> (@@ sub) <$> unify' atomEq (substs sub es)
    Right newEs -> unify' atomEq (newEs ++ es)

equate :: [Type] -> Maybe S
equate tys = unify eqns
  where
    eqns = zipWith (:=:) tys (tail tys)

occurs :: Name Type -> Type -> Bool
occurs x = anyOf fv (==x)

unifyOne :: (Atom -> Atom -> Bool) -> Eqn -> Maybe (Either S [Eqn])
unifyOne _ (ty1 :=: ty2)
  | ty1 == ty2 = return $ Left idS
unifyOne _ (TyVar x :=: ty2)
  | occurs x ty2 = Nothing
  | otherwise    = Just $ Left (x |-> ty2)
unifyOne atomEq (ty1 :=: x@(TyVar _))
  = unifyOne atomEq (x :=: ty1)
unifyOne _ (TyCons c1 tys1 :=: TyCons c2 tys2)
  | c1 == c2  = return $ Right (zipWith (:=:) tys1 tys2)
  | otherwise = Nothing
unifyOne atomEq (TyAtom a1 :=: TyAtom a2)
  | atomEq a1 a2 = return $ Left idS
  | otherwise    = Nothing
unifyOne _ _ = Nothing  -- Atom = Cons

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
  { reservedNames = ["let", "in", "fst", "snd"]
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

data Ineqn where
  (:<:) :: Type -> Type -> Ineqn
  deriving (Eq, Show, Generic)

toEqn :: Ineqn -> Eqn
toEqn (ty1 :<: ty2) = ty1 :=: ty2

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
infer EFst = do
  a <- freshTy
  b <- freshTy
  return (TyFun (TyPair a b) a)
infer ESnd = do
  a <- freshTy
  b <- freshTy
  return (TyFun (TyPair a b) b)
infer (EPair e1 e2) = TyPair <$> infer e1 <*> infer e2

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

type SimplifyM a = StateT ([Constraint], S) (ExceptT TypeError LFreshM) a



-- This is the next step after doing inference & constraint
-- generation.  After this step, the remaining constraints will all be
-- atomic constraints, of the form (v1 < v2), (v < b), or (b < v),
-- where v is a type variable and b is a base type.
--
-- Actually, there is one more step that should happen before this, to
-- check weak unification (to ensure this algorithm will terminate).
-- For example, (^x.x x) causes this to loop.
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
    simplifiable (Right (TyAtom a1 :<: TyAtom a2)) = isBase a1 && isBase a2

--------------------------------------------------
-- Graphs
--
-- Build a thin layer on top of Gr from fgl, which also allows
-- referring to nodes by their label.

data G a = G (Gr a ()) (Map a G.Node) (Map G.Node a)
  deriving Show

mkGraph :: Ord a => [a] -> [(a,a)] -> G a
mkGraph vs es = G (G.mkGraph vs' es') a2n n2a
  where
    vs' = zip [0..] vs
    n2a = M.fromList vs'
    a2n = M.fromList . map swap $ vs'
    es' = map mkEdge es
    mkEdge (a1,a2) = (a2n ! a1, a2n ! a2, ())

nodes :: G a -> [a]
nodes (G _ m _) = M.keys m

edges :: G a -> [(a,a)]
edges (G g _ m) = map (\(n1,n2,()) -> (m ! n1, m ! n2)) (G.labEdges g)

nmap :: Ord b => (a -> b) -> G a -> G b
nmap f (G g m1 m2) = G (G.nmap f g) (M.mapKeys f m1) (M.map f m2)

condensation :: Ord a => G a -> G [a]
condensation (G g _ n2a) = G g' asToNode n2as
  where
    g' = G.nmap (map (n2a !)) (G.condensation g)
    vs' = G.labNodes g'
    n2as = M.fromList vs'
    asToNode = M.fromList . map swap $ vs'

-- Weakly connected components
wcc :: Ord a => G a -> [[a]]
wcc (G g a2n n2a) = (map . map) (n2a!) (G.components g)

sequenceGraph :: Ord a => G (Maybe a) -> Maybe (G a)
sequenceGraph g = case sequence (nodes g) of
  Nothing -> Nothing
  Just _  -> Just $ nmap fromJust g

-- Successors
suc :: Ord a => G a -> a -> [a]
suc (G g a2n n2a) = map (n2a !) . G.suc g . (a2n !)

-- Predecessors
pre :: Ord a => G a -> a -> [a]
pre (G g a2n n2a) = map (n2a !) . G.pre g . (a2n !)

-- To build sets of successors and predecessors, do a topological
-- sort, then scan through in both directions.
-- XXX add more comments.
cessors :: Ord a => G a -> (Map a (Set a), Map a (Set a))
cessors g@(G gg a2n n2a) = (succs, preds)
  where
    as = G.topsort' gg
    succs = foldr collectSuccs M.empty as  -- build successors map
    collectSuccs a m = M.insert a succsSet m
      where
        ss       = suc g a
        succsSet = S.fromList ss `S.union` (S.unions $ map (m!) ss)

    preds = foldr collectPreds M.empty (reverse as)  -- build predecessors map
    collectPreds a m = M.insert a predsSet m
      where
        ss       = pre g a
        predsSet = S.fromList ss `S.union` (S.unions $ map (m!) ss)

--------------------------------------------------
-- Build the constraint graph

-- Given a list of atomic subtype constraints, build the corresponding
-- constraint graph.
mkConstraintGraph :: [(Atom, Atom)] -> G Atom
mkConstraintGraph cs = mkGraph (nubify nodes) (nubify cs)
  where
    nodes   = cs ^.. traverse . each
    nubify :: Ord a => [a] -> [a]
    nubify = S.toList . S.fromList

-- Next step: eliminate strongly connected components in the
-- constraint set by collapsing them (unifying all the types in the
-- SCC). Can fail if the types in a SCC are not unifiable.  Returns
-- the collapsed graph (which is now guaranteed to be DAG) and an
-- updated substitution.
--
-- What's an example term that leads to a cycle?
elimCycles :: G Atom -> Either TypeError (G Atom, S)
elimCycles g
  = maybe
      (Left NoUnify)
      (Right . (nmap fst &&& (compose . map snd . nodes)))
      g'
  where
    -- Condense each SCC into a node labeled by its list of atoms,
    -- which will be unified
    g' :: Maybe (G (Atom, S))
    g' = sequenceGraph $ nmap unifySCC (condensation g)

    unifySCC :: [Atom] -> Maybe (Atom, S)
    unifySCC [] = error "Impossible! unifySCC []"
    unifySCC as@(a:_) = (flip substs a &&& id) <$> equate tys
      where
        tys  = map TyAtom as

--------------------------------------------------
-- Constraint resolution

-- Build the set of successor and predecessor base types of each type
-- variable in the constraint graph.  For each type variable, make
-- sure the lub of its predecessors is <= the glb of its successors,
-- and assign it one of the two: one or the other if it has only
-- predecessors or only successors; if it has both default to the
-- lower bound.

-- XXX comment
solveConstraints :: G Atom -> Maybe S
solveConstraints g = (convertSubst . unifyWCC) <$> go ss ps
  where
    convertSubst :: S' Atom -> S
    convertSubst = map (coerce *** TyAtom)

    -- Final step: Unify-WCC. Take any remaining weakly connected
    -- components of variables (which means that the WCC contains no
    -- base types) and unify all the variables. An example where this
    -- makes a difference is ^x. (^y.y) x where there are two type
    -- variables generated, with one a subtype of the other, but no
    -- other constraints on them.  Without the unify-WCC step it would
    -- result in a1 -> a3 but really we should unify them to result in
    -- a -> a.  As another example, (^x. (^y.y) x, ^x. (^y.y) x)
    -- results in (a -> a, b -> b) since there are two disjoint WCCs
    -- (which should not be unified with each other).

    unifyWCC :: S' Atom -> S' Atom
    unifyWCC s = concatMap mkEquateSubst wccVarGroups
      where
        wccVarGroups = filter (all isVar) . substs s $ wcc g
        mkEquateSubst (a : as) = map (\(AVar v) -> (coerce v, a)) as

    -- Get the successor and predecessor sets for all the type variables.
    (ss, ps) = (onlyVars *** onlyVars) $ cessors g
    onlyVars = M.filterWithKey (\a _ -> isVar a)

    go :: Map Atom (Set Atom) -> Map Atom (Set Atom) -> Maybe (S' Atom)
    go succs preds = case as of
      -- No variables left that have base type constraints.
      []    -> Just idS

      -- Solve one variable at a time.  See below.
      (a:_) ->

        case solveVar a of
          Nothing       -> Nothing

          -- If we solved for a, delete it from the maps and apply the
          -- resulting substitution to the remainder.
          Just s -> traceShow s $
            (@@ s) <$> go (substs s (M.delete a succs)) (substs s (M.delete a preds))

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
        -- -> N as well.
        --
        -- This exact graph comes from (^x.x+1) which was erroneously
        -- being inferred to have type Z -> N.

        -- Get only the variables we can solve on this pass, which have
        -- base types in their predecessor or successor set.
        as = filter (\a -> any isBase (succs ! a) || any isBase (preds ! a)) (M.keys succs)

        -- Solve for a variable, failing if it has no solution, otherwise returning
        -- a substitution for it.
        solveVar :: Atom -> Maybe (S' Atom)
        solveVar a@(AVar v) =
          case (filter isBase (S.toList $ succs ! a), filter isBase (S.toList $ preds ! a)) of
            ([], []) ->
              error $ "Impossible! solveConstraints.solveVar called on variable "
                      ++ show a ++ " with no base type successors or predecessors"

            -- Only successors.  Just assign a to their glb, if one exists.
            (bsuccs, []) -> (coerce v |->) <$> aglb bsuccs

            -- Only predecessors.  Just assign a to their lub.
            ([], bpreds) -> (coerce v |->) <$> alub bpreds

            -- Both successors and predecessors.  Both must have a
            -- valid bound, and the bounds must not overlap.  Assign a
            -- to the upper bound of its predecessors.
            (bsuccs, bpreds) -> do
              ub <- aglb bsuccs
              lb <- alub bpreds
              case isSub lb ub of
                True  -> Just (coerce v |-> lb)
                False -> Nothing

        solveVar a = error $ "Impossible! solveConstraints.solveVar called on non-variable " ++ show a

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

instance Pretty Eqn where
  pretty (ty1 :=: ty2) = pretty ty1 ++ " = " ++ pretty ty2

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a)  = pretty a
  pretty (Right b) = pretty b

instance Pretty Ineqn where
  pretty (ty1 :<: ty2) = pretty ty1 ++ " < " ++ pretty ty2

instance Pretty a => Pretty (G a) where
  pretty = prettyList . map prettyEdge . edges
    where
      prettyEdge (a1,a2) = pretty a1 ++ " >> " ++ pretty a2

instance Pretty (Name Type) where
  pretty = show

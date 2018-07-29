{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ViewPatterns             #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Typecheck the Disco surface language and transform it into a
-- type-annotated AST.
--
-----------------------------------------------------------------------------

module Disco.Typecheck where

import           GHC.Generics                            (Generic)

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (makeLenses, use, (%=),
                                                          (%~), (&), _1)
import qualified Control.Lens                            as Lens
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Bifunctor                          (first, second)
import           Data.Coerce
import           Data.List                               (group, partition,
                                                          sort)
import qualified Data.Map                                as M
import qualified Data.Set                                as S
import           Prelude                                 hiding (lookup)

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Generic                       (selectSide)
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Syntax.Operators
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Solve
import           Disco.Types
import           Disco.Types.Rules

------------------------------------------------------------
-- Definitions and contexts
------------------------------------------------------------

-- | A definition is a group of clauses, each having a list of
--   patterns that bind names in a term, without the name of the
--   function being defined.  For example, given the concrete syntax
--   @f n (x,y) = n*x + y@, the corresponding 'Defn' would be
--   something like @[n, (x,y)] (n*x + y)@.
data Defn  = Defn (Name ATerm) [Type] Type [Clause]
  deriving (Show, Generic)

-- | A clause in a definition consists of a list of patterns (the LHS
--   of the =) and a term (the RHS).
type Clause = Bind [APattern] ATerm

instance Subst Type Defn

-- | A map from type names to their corresponding definitions.
type TyDefCtx = M.Map String Type

-- | A context of definitions: a map from names of terms to their
--   (typechecked) definitions, as well as a map of named types.
data DefnCtx = DefnCtx
  { _termDefns :: Ctx ATerm Defn
  , _tyDefns   :: TyDefCtx
  }

makeLenses ''DefnCtx

-- | The initial, empty @DefnCtx@.
emptyDefnCtx :: DefnCtx
emptyDefnCtx = DefnCtx
  { _termDefns = emptyCtx
  , _tyDefns   = M.empty
  }

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term Sigma

------------------------------------------------------------
-- Errors
------------------------------------------------------------

-- | Potential typechecking errors.
data TCError
  = Unbound (Name Term)    -- ^ Encountered an unbound variable
  | NotCon Con Term Type   -- ^ The term should have an outermost constructor matching
                           --   matching Con, but it has type 'Type' instead
  | NotFun   ATerm         -- ^ The term should be a function but has a non-arrow type
  | NotTuplePattern Pattern Type
  | Mismatch Type ATerm    -- ^ Simple type mismatch: expected, actual
  | CantInfer Term         -- ^ We were asked to infer the type of the
                           --   term, but its type cannot be inferred
  | NotNum ATerm           -- ^ The term is expected to have a numeric type but it doesn't
  | NotNumTy Type          -- ^ The type should be numeric, but is not.
  | IncompatibleTypes Type Type  -- ^ The types should have a lub
                                 -- (i.e. common supertype) but they
                                 -- don't.
  | Juxtaposition ATerm Term
                           -- ^ The first term is juxtaposed with the
                           --   second, but typechecks as neither
                           --   function application nor
                           --   multiplication
  | Undecidable Type       -- ^ The type should be decidable so we can
                           --   check equality, but it isn't.
  | Unordered Type         -- ^ The type should be totally ordered so
                           --   we can check less than, but it isn't.
  | Infinite Type
  | EmptyCase              -- ^ Case analyses cannot be empty.
  | NoLub Type Type        -- ^ The given types have no lub.
  | PatternType Pattern Type  -- ^ The given pattern should have the type, but it doesn't.
  | ModQ                   -- ^ Can't do mod on rationals.
  | ExpQ                   -- ^ Can't exponentiate by a rational.
  | DuplicateDecls (Name Term)  -- ^ Duplicate declarations.
  | DuplicateDefns (Name Term)  -- ^ Duplicate definitions.
  | DuplicateTyDefns String -- ^ Duplicate type definitions.
  | CyclicTyDef String     -- ^ Cyclic type definition.
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NotSubtractive Type
  | NotFractional Type
  | Unsolvable SolveError
  | NotTyDef String             -- ^ The type is an algebraic data type that was never defined.
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

------------------------------------------------------------
-- TCM monad
------------------------------------------------------------

-- | Type checking monad. Maintains a locally-scoped context of
--   variables and their types and a read-write context of term and
--   type definitions; collects constraints; can throw @TCError@s; and
--   can generate fresh names.
type TCM = RWST TyCtx Constraint DefnCtx (ExceptT TCError FreshM)

-- This is an orphan instance, but we can't very well add it to either
-- 'containers' or 'unbound-generic'.
instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

--------------------------------------------------
-- Running

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, DefnCtx, Constraint)
runTCM = runFreshM . runExceptT . (\m -> runRWST m emptyCtx emptyDefnCtx)

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap (\(a,_,_) -> a) . runTCM

--------------------------------------------------
-- Constraints

-- | Add a constraint.
constraint :: Constraint -> TCM ()
constraint = tell

-- | Add a list of constraints.
constraints :: [Constraint] -> TCM ()
constraints = constraint . cAnd

-- | Close over the current constraint with a forall.
forAll :: [Name Type] -> TCM a -> TCM a
forAll nms = censor (CAll . bind nms)

-- | Run a 'TCM' computation, returning the generated 'Constraint'
--   along with the output, and reset the 'Constraint' of the resulting
--   computation to 'mempty'.
withConstraint :: TCM a -> TCM (a, Constraint)
withConstraint = censor (const mempty) . listen

-- | Run a 'TCM' computation and solve its generated constraint,
--   returning the resulting substitution (or failing with an error).
--   The resulting TCM computation generates the empty constraint.
solve :: TCM a -> TCM (a, S)
solve m = do
  (a, c) <- withConstraint m
  tds <- use tyDefns
  case runSolveM . solveConstraint tds $ c of
    Left err -> throwError (Unsolvable err)
    Right s  -> return (a, s)

--------------------------------------------------
-- Contexts

-- | Add a definition to the set of current definitions.
addDefn :: Name Term -> Defn -> TCM ()
addDefn x b = termDefns %= M.insert (coerce x) b

-- | Add a type definition to the set of current type defintions.
addTyDefn :: String -> Type -> TCM ()
addTyDefn x b = tyDefns %= M.insert x b

-- | Extend the current TyDefCtx with a provided tydef context.
extendTyDefs :: TyDefCtx -> TCM a -> TCM a
extendTyDefs newtyctx
  = withRWST (curry (second (tyDefns %~ M.union newtyctx)))

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy :: Name Term -> TCM Sigma
lookupTy x = lookup x >>= maybe (throwError (Unbound x)) return

-- | Look up the definition of a named type.  Throw a 'NotTyDef' error
--   if it is not found.
lookupTyDefn :: String -> TCM Type
lookupTyDefn x = do
  d <- use tyDefns
  case M.lookup x d of
    Nothing -> throwError (NotTyDef x)
    Just ty -> return ty

--------------------------------------------------
-- Fresh name generation

-- | Generate a type variable with a fresh name.
freshTy :: TCM Type
freshTy = TyVar <$> fresh (string2Name "a")


------------------------------------------------------------
-- Container utilities
------------------------------------------------------------

containerTy :: Container -> Type -> Type
containerTy c ty = TyCon (containerToCon c) [ty]

containerToCon :: Container -> Con
containerToCon ListContainer = CList
containerToCon SetContainer  = CSet

------------------------------------------------------------
-- Telescopes
------------------------------------------------------------

-- | Infer the type of a telescope, given a way to infer the type of
--   each item along with a context of variables it binds; each such
--   context is then added to the overall context when inferring
--   subsequent items in the telescope.
inferTelescope
  :: (Alpha b, Alpha tyb)
  => (b -> TCM (tyb, TyCtx)) -> Telescope b -> TCM (Telescope tyb, TyCtx)
inferTelescope inferOne tel = do
  (tel1, ctx) <- go (fromTelescope tel)
  return $ (toTelescope tel1, ctx)
  where
    go []     = return ([], emptyCtx)
    go (b:bs) = do
      (tyb, ctx) <- inferOne b
      extends ctx $ do
      (tybs, ctx') <- go bs
      return (tyb:tybs, ctx `joinCtx` ctx')

------------------------------------------------------------
-- Type checking/inference
------------------------------------------------------------

-- | Typechecking can be in one of two modes: inference mode means we
--   are trying to generate a valid type for a term; checking mode
--   means we are trying to show that a term has a given type.
data Mode = Infer | Check Type
  deriving Show

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
--
--   This function is provided for convenience; it simply calls
--   'typecheck' with an appropriate 'Mode'.
check :: Term -> Type -> TCM ATerm
check t ty = typecheck (Check ty) t

-- | Infer the type of a term.  If it succeeds, it returns the term
--   with all subterms annotated.
--
--   This function is provided for convenience; it simply calls
--   'typecheck' with an appropriate 'Mode'.
infer :: Term -> TCM ATerm
infer t = typecheck Infer t

-- | Top-level type inference algorithm: infer a (polymorphic) type
--   for a term by running type inference, solving the resulting
--   constraints, and quantifying over any remaining type variables.
inferTop :: Term -> TCM (ATerm, Sigma)
inferTop t = do
  (at, theta) <- solve $ infer t
  traceShowM at
  let at' = substs theta at
  return (at', closeSigma (getType at'))

-- | The main workhorse of the typechecker.  Instead of having two
--   functions, one for inference and one for checking, 'typecheck'
--   takes a 'Mode'.  This cuts down on code duplication in many
--   cases, and allows all the checking and inference code related to
--   a given AST node to be placed together.
typecheck :: Mode -> Term -> TCM ATerm

-- To check at a defined type, expand its definition and recurse.
typecheck (Check (TyDef tyn)) t = lookupTyDefn tyn >>= check t

-- Recurse through parens; they are not represented explicitly in the
-- resulting ATerm.
typecheck mode (TParens t) = typecheck mode t

-- To infer the type of a variable, just look it up in the context.
-- We don't need a checking case; checking the type of a variable will
-- fall through to this case.
typecheck Infer (TVar x)      = do
  Forall sig <- lookupTy x
  (_, ty)    <- unbind sig
  return $ ATVar ty (coerce x)

-- A few trivial cases for base types.
typecheck Infer             TUnit     = return ATUnit
typecheck Infer             (TBool b) = return $ ATBool b
typecheck (Check (TyFin n)) (TNat x)  = return $ ATNat (TyFin n) x
typecheck Infer             (TNat n)  = return $ ATNat TyN n
typecheck Infer             (TRat r)  = return $ ATRat r

-- XXX the following two cases really ought to be unified, and the
-- checkArgs function done away with (or incorporated).

-- To check a lambda abstraction:
typecheck (Check ty) (TAbs lam) = do
  (args, t) <- unbind lam

  -- First check that the given type is of the form ty1 -> ty2 ->
  -- ... -> resTy, where the types ty1, ty2 ... match up with any
  -- types declared for the arguments to the lambda (e.g.  (x:tyA)
  -- (y:tyB) -> ...).
  (ctx, typedArgs, resTy) <- checkArgs args ty (TAbs lam)

  -- Then check the type of the body under a context extended with
  -- types for all the arguments.
  extends ctx $
    ATAbs ty <$> (bind (coerce typedArgs) <$> check t resTy)

-- To infer a lambda abstraction:
typecheck Infer (TAbs lam)    = do

  -- Open it and get the variables with any type annotations.
  (args, t) <- unbind lam
  let (xs, mtys) = unzip args

  -- Replace any missing type annotations with fresh type variables,
  -- and make a map from variable names to types.
  tys <- mapM (maybe freshTy return) (map unembed mtys)
  let tymap = M.fromList $ zip xs (map toSigma tys)

  -- Infer the type of the body under an extended context, and
  -- construct an ATAbs with the appropriate function type.
  extends tymap $ do
    at <- infer t
    return $ ATAbs (mkFunTy tys (getType at))
                   (bind (zip (map coerce xs) (map embed tys)) at)
  where
    -- mkFunTy [ty1, ..., tyn] out = ty1 -> (ty2 -> ... (tyn -> out))
    mkFunTy :: [Type] -> Type -> Type
    mkFunTy tys out = foldr TyArr out tys

-- Infer the type of a function application by inferring the function
-- type and then checking the argument type.  We don't need a checking
-- case because checking mode doesn't help.
typecheck Infer (TApp t t')   = do
  at <- infer t
  let ty = getType at
  [ty1, ty2] <- ensureConstr CArr ty (Left t)
  ATApp ty2 at <$> check t' ty1

-- Check/infer the type of a tuple.
typecheck mode1 (TTup ts) = uncurry ATTup <$> typecheckTuple mode1 ts
  where
    typecheckTuple :: Mode -> [Term] -> TCM (Type, [ATerm])
    typecheckTuple _    []     = error "Impossible! typecheckTuple []"
    typecheckTuple mode [t]    = (getType &&& (:[])) <$> typecheck mode t
    typecheckTuple mode (t:ts) = do
      [m, ms]   <- ensureConstrMode CPair mode (Left $ TTup (t:ts))
      at        <- typecheck      m  t
      (ty, ats) <- typecheckTuple ms ts
      return $ (TyPair (getType at) ty, at : ats)

-- Check/infer the type of an injection into a sum type.
typecheck mode lt@(TInj s t) = do
  [mL, mR] <- ensureConstrMode CSum mode (Left lt)
  at <- typecheck (selectSide s mL mR) t
  resTy <- case mode of
    Infer    ->
      selectSide s (TySum <$> pure (getType at) <*> freshTy          )
                   (TySum <$> freshTy           <*> pure (getType at))
    Check ty -> return ty
  return $ ATInj resTy s at

-- To check a cons, make sure the type is a list type, then
-- check the two arguments appropriately.
typecheck (Check ty) t@(TBin Cons x xs) = do
  [tyElt] <- ensureConstr CList ty (Left t)
  ATBin ty Cons <$> check x tyElt <*> check xs (TyList tyElt)

-- To infer the type of a cons:
typecheck Infer (TBin Cons t1 t2) = do

  -- First, infer the type of the first argument (a list element).
  at1 <- infer t1

  case t2 of
    -- If the second argument is the empty list, just assign it the
    -- type inferred from the first element.
    TList [] Nothing -> do
      let ty1 = getType at1
      return $ ATBin (TyList ty1) Cons at1 (ATList (TyList ty1) [] Nothing)

    -- Otherwise, infer the type of the second argument...
    _ -> do
      at2 <- infer t2
      case getType at2 of

        -- ...make sure it is a list, and find the lub of the element types.
        TyList ty2 -> do
          let ty1 = getType at1
          tyv <- freshTy
          constraints $ [CSub ty1 tyv, CSub ty2 tyv]
          return $ ATBin (TyList tyv) Cons at1 at2
        ty -> throwError (NotCon CList t2 ty)

-- A bunch of inference cases (& a few checking cases) for binary and
-- unary operators.

-- Once upon a time, when we only had types N, Z, Q+, and Q, we could
-- always infer the type of anything numeric, so we didn't need
-- checking cases for Add, Mul, etc.  But now we could have e.g.  (a +
-- b) : Z13, in which case we need to push the checking type (in the
-- example, Z13) through the operator to check the arguments.

-- Checking arithmetic binary operations involves checking both operands
-- and then adding a constraint qualifier over binary operation and the desired type.
typecheck (Check ty) (TBin op t1 t2) | op `elem` [Add, Mul, Div, Sub, SSub] = do
  constraint $ CQual (bopQual op) ty
  ATBin ty op <$> check t1 ty <*> check t2 ty

-- To infer the type of addition or multiplication, infer the types
-- of the subterms, check that they are numeric, and return their
-- lub.
typecheck Infer (TBin op t1 t2) | op `elem` [Add, Mul, Sub, Div, SSub] = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
      ty2 = getType at2
  tyv <- freshTy

  constraints $
    [ CSub ty1 tyv
    , CSub ty2 tyv
    , CQual (bopQual op) tyv
    ]
  return $ ATBin tyv op at1 at2

typecheck (Check ty) (TUn Neg t) = do
  constraint $ CQual (QSub) ty
  ATUn ty Neg <$> check t ty

typecheck Infer (TUn Neg t) = do
  at <- infer t
  let ty = getType at
  tyv <- freshTy
  constraints $ [CSub ty tyv, CQual QSub tyv]
  return $ ATUn tyv Neg at

typecheck Infer (TUn op t) | op `elem` [Sqrt, Lg]    = ATUn TyN op <$> check t TyN

typecheck Infer (TUn op t) | op `elem` [Floor, Ceil] = do
  at <- infer t
  let ty = getType at
  resTy <- cInt ty
  return $ ATUn resTy op at

typecheck Infer (TUn Abs t) = do
  at <- infer t
  let ty = getType at
  resTy <- cPos ty
  return $ ATUn resTy Abs at

typecheck Infer (TBin IDiv t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv1 <- freshTy
  resTy <- cInt tyv1
  constraints $ [CSub ty1 tyv1, CSub ty2 tyv1]
  return $ ATBin resTy IDiv at1 at2

typecheck (Check ty) (TBin Exp t1 t2) = do
  at1   <- check t1 ty
  at2   <- infer t2
  resTy <- cExp (getType at1) (getType at2)

  constraint $ CSub resTy ty
  return $ ATBin resTy Exp at1 at2

typecheck Infer (TBin Exp t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  resTy <- cExp ty1 ty2
  return $ ATBin resTy Exp at1 at2

-- Infer the type of a comparison. A comparison always has type Bool,
-- but we have to make sure the subterms have compatible types.
typecheck Infer (TBin op t1 t2) | op `elem` [Eq, Neq, Lt, Gt, Leq, Geq] = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
      ty2 = getType at2
  tyv <- freshTy
  constraints $ [CSub ty1 tyv, CSub ty2 tyv]
  return $ ATBin TyBool op at1 at2

-- &&, ||, and not always have type Bool, and the subterms must have type
-- Bool as well.
typecheck Infer (TBin op t1 t2) | op `elem` [And, Or, Impl] =
  ATBin TyBool op <$> check t1 TyBool <*> check t2 TyBool

typecheck Infer (TUn Not t) = ATUn TyBool Not <$> check t TyBool

typecheck Infer (TBin Mod t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  constraints $ [CSub ty1 tyv, CSub ty2 tyv, CSub tyv TyZ]
  return $ ATBin tyv Mod at1 at2

typecheck Infer (TBin Divides t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  constraints $ [CSub ty1 tyv, CSub ty2 tyv, CQual QNum tyv]
  return $ ATBin TyBool Divides at1 at2

-- For now, a simple typing rule for multinomial coefficients that
-- requires everything to be Nat.  However, they can be extended to
-- handle negative or fractional arguments.
typecheck Infer (TBin Choose t1 t2) = do
  at1 <- check t1 TyN

  -- t2 can be either a Nat (a binomial coefficient)
  -- or a list of Nat (a multinomial coefficient).
  at2 <- infer t2
  let ty2 = getType at2
  constraint $ COr [CEq ty2 TyN, CEq ty2 (TyList TyN)]
  return $ ATBin TyN Choose at1 at2

typecheck Infer (TUn Size t) = do
  at <- infer t
  _  <- ensureConstr CSet (getType at) (Left t)
  return $ ATUn TyN Size at

typecheck (Check ty) t@(TBin setOp t1 t2)
    | setOp `elem` [Union, Intersection, Difference] = do
  [tyElt] <- ensureConstr CSet ty (Left t)
  ATBin ty setOp <$> check t1 (TySet tyElt) <*> check t2 (TySet tyElt)

typecheck Infer (TBin setOp t1 t2)
    | setOp `elem` [Union, Intersection, Difference, Subset] = do
  at1 <- infer t1
  at2 <- infer t2
  tyelt <- freshTy
  let ty1 = getType at1
  let ty2 = getType at2
  let ty = case setOp of {Subset -> TyBool; _ -> TySet tyelt}
  constraints [CSub ty1 (TySet tyelt), CSub ty2 (TySet tyelt)]
  return $ ATBin ty setOp at1 at2

typecheck Infer (TUn Fact t) =
  ATUn TyN Fact <$> check t TyN

typecheck Infer (TChain t1 links) =
  ATChain TyBool <$> infer t1 <*> inferChain t1 links

  where
    inferChain :: Term -> [Link] -> TCM [ALink]
    inferChain _  [] = return []
    inferChain t1 (TLink op t2 : links) = do
      at2 <- infer t2
      _   <- check (TBin op t1 t2) TyBool
      atl <- inferChain t2 links
      return $ ATLink op at2 : atl

typecheck Infer (TTyOp Enumerate t) = return $ ATTyOp (TyList t) Enumerate t

typecheck Infer (TTyOp Count t)     = return $ ATTyOp (TySum TyUnit TyN) Count t

-- Literal containers
typecheck mode t@(TContainer c xs ell)  = do
  [eltMode] <- ensureConstrMode (containerToCon c) mode (Left t)
  axs  <- mapM (typecheck eltMode) xs
  aell <- typecheckEllipsis eltMode ell
  resTy <- case mode of
    Infer -> do
      let tys = [ getType at | Just (Until at) <- [aell] ] ++ (map getType) axs
      tyv  <- freshTy
      constraints $ map (flip CSub tyv) tys
      return $ containerTy c tyv
    Check ty -> return ty
  return $ ATContainer resTy c axs aell

  where
    typecheckEllipsis :: Mode -> Maybe (Ellipsis Term) -> TCM (Maybe (Ellipsis ATerm))
    typecheckEllipsis _    Nothing          = return Nothing
    typecheckEllipsis _    (Just Forever)   = return $ Just Forever
    typecheckEllipsis mode (Just (Until t)) = (Just . Until) <$> typecheck mode t

-- Container comprehensions
typecheck mode tcc@(TContainerComp c bqt) = do
  [eltMode] <- ensureConstrMode (containerToCon c) mode (Left tcc)
  (qs, t)   <- unbind bqt
  (aqs, cx) <- inferTelescope inferQual qs
  extends cx $ do
    at <- typecheck eltMode t
    let resTy = case mode of
          Infer    -> containerTy c (getType at)
          Check ty -> ty
    return $ ATContainerComp resTy c (bind aqs at)

  where
    inferQual :: Qual -> TCM (AQual, TyCtx)
    inferQual (QBind x (unembed -> t))  = do
      at <- infer t
      case (c, getType at) of
        (_, TyList ty)   -> return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty))
        (SetContainer, TySet ty) -> return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty))
        (_, wrongTy)   -> throwError $ NotCon (containerToCon c) t wrongTy

    inferQual (QGuard (unembed -> t))   = do
      at <- check t TyBool
      return (AQGuard (embed at), emptyCtx)


-- To check/infer a let expression.  Note let is non-recursive.
typecheck mode (TLet l) = do
  (bs, t2) <- unbind l

  -- Infer the types of all the variables bound by the let...
  (as, ctx) <- inferTelescope inferBinding bs

  -- ...then check/infer the body under an extended context.
  extends ctx $ do
    at2 <- typecheck mode t2
    return $ ATLet (getType at2) (bind as at2)

  where

    -- Infer the type of a binding (@x [: ty] = t@), returning a
    -- type-annotated binding along with a (singleton) context for the
    -- bound variable.  The optional type annotation on the variable
    -- determines whether we use inference or checking mode for the
    -- body.
    inferBinding :: Binding -> TCM (ABinding, TyCtx)
    inferBinding (Binding mty x (unembed -> t)) = do
      at <- case mty of
        Just (unembed -> ty) -> checkSigma t ty
        Nothing              -> infer t
      return $ (ABinding mty (coerce x) (embed at), singleCtx x (toSigma $ getType at))

-- Check/infer a case expression.
typecheck _    (TCase []) = throwError EmptyCase
typecheck mode (TCase bs) = do
  bs' <- mapM typecheckBranch bs
  resTy <- case mode of
    Check ty -> return ty
    Infer    -> do
      x <- freshTy
      constraints $ map (flip CSub x) (map getType bs')
      return x
  return $ ATCase resTy bs'

  where
    typecheckBranch :: Branch -> TCM ABranch
    typecheckBranch b = do
      (gs, t) <- unbind b
      (ags, ctx) <- inferTelescope inferGuard gs
      extends ctx $
        bind ags <$> typecheck mode t

    -- Infer the type of a guard, returning the type-annotated guard
    -- along with a context of types for any variables bound by the
    -- guard.
    inferGuard :: Guard -> TCM (AGuard, TyCtx)
    inferGuard (GBool (unembed -> t)) = do
      at <- check t TyBool
      return (AGBool (embed at), emptyCtx)
    inferGuard (GPat (unembed -> t) p) = do
      at <- infer t
      (ctx, apt) <- checkPattern p (getType at)
      return (AGPat (embed at) apt, ctx)


-- Ascriptions are what let us flip from inference mode into
-- checking mode.
typecheck Infer (TAscr t ty) = checkSigma t ty

-- Finally, to check anything else, we can fall back to inferring its
-- type and then check that the inferred type is a *subtype* of the
-- given type.  We have to be careful to call 'setType' to change the
-- type at the root of the term to the requested type.
typecheck (Check ty) t = do
  at <- infer t
  constraint $ CSub (getType at) ty
  return $ setType ty at

-- This should never happen.  We should at least be able to infer a
-- type for everything.
typecheck mode t = error $ "Impossible! No case for typecheck " ++ show mode ++ " " ++ show t

------------------------------------------------------------
-- XXX  Misc stuff that needs to be organized still
------------------------------------------------------------

-- | Check that a term has the given sigma type.
checkSigma :: Term -> Sigma -> TCM ATerm
checkSigma t (Forall sig) = do
  (as, tau) <- unbind sig
  (at, cst) <- withConstraint $ check t tau
  case as of
    [] -> constraint cst
    _  -> constraint $ CAll (bind as cst)
  return at

-- | Given the variables and their optional type annotations in the
--   head of a lambda (e.g.  @x (y:Z) (f : N -> N) -> ...@), and the
--   type at which we are checking the lambda, ensure that the type is
--   of the form @ty1 -> ty2 -> ... -> resTy@, and that there are
--   enough @ty1@, @ty2@, ... to match all the arguments.  Also check
--   that each binding with a type annotation matches the
--   corresponding ty_i component from the checking type: in
--   particular, the ty_i must be a subtype of the type annotation.
--   If it succeeds, return a context binding variables to their types
--   (taken either from their annotation or from the type to be
--   checked, as appropriate) which we can use to extend when checking
--   the body, along with the result type of the function.
checkArgs
  :: [(Name Term, Embed (Maybe Type))] -> Type -> Term
  ->  TCM (TyCtx, [(Name Term, Embed Type)], Type)

-- If we're all out of arguments, the remaining checking type is the
-- result, and there are no variables to bind in the context.
checkArgs [] ty _ = return (emptyCtx, [], ty)

-- Take the next variable and its annotation; the checking type must
-- be a function type ty1 -> ty2.
checkArgs ((x, unembed -> mty) : args) ty term = do

  -- Ensure that ty is a function type
  [ty1, ty2] <- ensureConstr CArr ty (Left term)

  -- Figure out the type of x:
  xTy <- case mty of

    -- If it has no annotation, just take the input type ty1.
    Nothing    -> return ty1

    -- If it does have an annotation, make sure the input type is a
    -- subtype of it.
    Just annTy -> do
      constraint $ CSub ty1 annTy
      return ty1

  -- Check the rest of the arguments under the type ty2, returning a
  -- context with the rest of the arguments and the final result type.
  (ctx, typedArgs, resTy) <- checkArgs args ty2 term

  -- Pass the result type through, and add x with its type to the
  -- generated context.
  return (singleCtx x (toSigma xTy) `joinCtx` ctx, (x, embed xTy) : typedArgs, resTy)

-- | Generates appropriate constraints for the type of an operand of
--   an absolute value function along with constraints for the type of the result.
cPos :: Type -> TCM Type
cPos ty@(TyAtom (ABase b)) = do
  constraint $ CQual QNum ty
  return $ TyAtom (ABase (pos b))
  where
    pos Z = N
    pos Q = F
    pos _ = b

cPos ty = do
  res <- freshTy
  constraints $
    [ CQual QNum ty
    , COr
      [ cAnd [CSub ty TyZ, CSub TyN res]
      , cAnd [CSub ty TyQ, CSub TyF res]
      , CEq ty res
      ]
    ]
  return res

-- | Generates appropriate constraints for the type of an operand of a
--   ceiling or floor function along with constraints for the type of the result.
cInt :: Type -> TCM Type
cInt ty@(TyAtom (ABase b)) = do
  constraint $ CQual QNum ty
  return $ TyAtom (ABase (int b))
  where
    int F = N
    int Q = Z
    int _ = b

cInt ty = do
  res <- freshTy
  constraints $
    [ CQual QNum ty
    , COr
      [ cAnd [CSub ty TyF, CSub TyN res]
      , cAnd [CSub ty TyQ, CSub TyZ res]
      , CEq ty res
      ]
    ]
  return res

-- | Generates appropriate constraints for the types of the operands of the
--   exponent function along with constraints for the type of the result.
cExp :: Type -> Type -> TCM Type
cExp ty1 ty2            = do
  traceM $ "cExp: " ++ show ty1 ++ " " ++ show ty2
  tyv1 <- freshTy

  -- if a^b :: fractional t, then a :: t, b :: Z
  -- else if a^b :: non-fractional t, then a :: t, b :: N
  constraint $ COr
    [ cAnd [CQual QNum tyv1, CEq ty2 TyN, CSub ty1 tyv1]
    , cAnd [CQual QDiv tyv1, CEq ty2 TyZ, CSub ty1 tyv1]
    ]
  return tyv1

-- | Check that a pattern has the given type, and return a context of
--   pattern variables bound in the pattern along with their types.
checkPattern :: Pattern -> Type -> TCM (TyCtx, APattern)

checkPattern p (TyDef tyn)                  = do
  lookupTyDefn tyn >>= checkPattern p

checkPattern (PVar x) ty                    = return (singleCtx x (toSigma ty), APVar ty (coerce x))

checkPattern PWild    ty                    = return (emptyCtx, APWild ty)

checkPattern PUnit tyv@(TyVar _)            = do
  constraint $ CEq tyv TyUnit
  return (emptyCtx, APUnit)

checkPattern PUnit TyUnit                   = return (emptyCtx, APUnit)

checkPattern (PBool b) tyv@(TyVar _)        = do
  constraint $ CEq tyv TyBool
  return (emptyCtx, APBool b)

checkPattern (PBool b) TyBool               = return (emptyCtx, APBool b)

checkPattern (PTup ps) ty                   = do
  listCtxtAps <- checkTuplePat ps ty
  let (ctxs, aps) = unzip listCtxtAps
  return (joinCtxs ctxs, APTup (foldr1 TyPair (map getType aps)) aps)
checkPattern p@(PInj L pat) ty       = do
  [ty1, ty2] <- ensureConstr CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty1
  return (ctx, APInj (TySum ty1 ty2) L apt)
checkPattern p@(PInj R pat) ty    = do
  [ty1, ty2] <- ensureConstr CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty2
  return (ctx, APInj (TySum ty1 ty2) R apt)

-- we can match any supertype of TyN against a Nat pattern, OR
-- any TyFin.

-- XXX this isn't quite right, what if we're checking at a type
-- variable but we need to solve it to be a TyFin?  Can this ever
-- happen?  We would need a COr, except we can't express the constraint "exists m. ty = TyFin m"
checkPattern (PNat n) (TyFin m) = return (emptyCtx, APNat (TyFin m) n)
checkPattern (PNat n) ty        = do
  constraint $ CSub TyN ty
  return (emptyCtx, APNat ty n)

checkPattern (PSucc p) tyv@(TyVar _)               = do
  (ctx, apt) <- checkPattern p TyN
  constraint $ CEq tyv TyN
  return (ctx, APSucc apt)

checkPattern (PSucc p) TyN                 = do
  (ctx, apt) <- checkPattern p TyN
  return (ctx, APSucc apt)

checkPattern p@(PCons p1 p2) ty      = do
  [tyl] <- ensureConstr CList ty (Right p)
  (ctx1, ap1) <- checkPattern p1 tyl
  (ctx2, ap2) <- checkPattern p2 (TyList tyl)
  return (joinCtx ctx1 ctx2, APCons (TyList tyl) ap1 ap2)

checkPattern p@(PList ps) ty         = do
  [tyl] <- ensureConstr CList ty (Right p)
  listCtxtAps <- mapM (flip checkPattern tyl) ps
  let (ctxs, aps) = unzip listCtxtAps
  return (joinCtxs ctxs, APList (TyList tyl) aps)

checkPattern p ty = throwError (PatternType p ty)

checkTuplePat :: [Pattern] -> Type -> TCM ([(TyCtx, APattern)])
checkTuplePat [] _   = error "Impossible! checkTuplePat []"
checkTuplePat [p] ty = do     -- (:[]) <$> check t ty
  (ctx, apt) <- checkPattern p ty
  return [(ctx, apt)]
checkTuplePat (p:ps) ty = do
  [ty1, ty2]  <- ensureConstr CPair ty (Right $ PTup (p:ps))
  (ctx, apt)  <- checkPattern p ty1
  rest <- checkTuplePat ps ty2
  return ((ctx, apt) : rest)

-- | Ensures that a type's outermost constructor matches the provided constructor,
--   returning the types within the matched constructor or throwing a type error.
--   If the type provided is a type variable, appropriate constraints are generated
--   to guarentee the type variable's outermost constructor matches the provided
--   constructor, and a list of type variables is returned whose count matches the
--   arity of the provided constructor.
ensureConstr :: Con -> Type -> Either Term Pattern -> TCM [Type]
ensureConstr c1 (TyCon c2 tys) _ | c1 == c2 = return tys

ensureConstr c tyv@(TyAtom (AVar (U _))) _ = do
  tyvs <- mapM (const freshTy) (arity c)
  constraint $ CEq tyv (TyCon c tyvs)
  return tyvs

ensureConstr c ty targ =
  case targ of
    Left term -> throwError (NotCon c term ty)
    Right pat -> throwError (PatternType pat ty)

-- XXX
ensureConstrMode :: Con -> Mode -> Either Term Pattern -> TCM [Mode]
ensureConstrMode c Infer      _  = return $ map (const Infer) (arity c)
ensureConstrMode c (Check ty) tp = map Check <$> ensureConstr c ty tp

-- | Check all the types in a module, returning a context of types for
--   top-level definitions.
checkModule :: Module -> TCM (Ctx Term Docs, Ctx ATerm [AProperty], TyCtx)
checkModule (Module m docs) = do
  let (tydefs, rest) = partition isTyDef m
  addTyDefns tydefs
  checkCyclicTys tydefs
  let (defns, typeDecls) = partition isDefn rest
  withTypeDecls typeDecls $ do
    mapM_ checkDefn defns
    aprops <- checkProperties docs
    ctx <- ask
    return (docs, aprops, ctx)

addTyDefns :: [Decl] -> TCM ()
addTyDefns tydefs = do
  let dups :: [(String, Int)]
      dups = filter ((>1) . snd) . map (head &&& length) . group . sort . map tyDefName $ tydefs
  case dups of
    ((x,_):_) -> throwError (DuplicateTyDefns x)
    []        -> mapM (\(x, ty) -> addTyDefn x ty) (map getTyDef tydefs) *> return ()
  where
    getTyDef :: Decl -> (String, Type)
    getTyDef (DTyDef x ty) = (x, ty)
    getTyDef d             = error $ "Impossible! addTyDefns.getTyDef called on non-DTyDef: " ++ show d

checkCyclicTys :: [Decl] -> TCM ()
checkCyclicTys decls = mapM (\decl -> unwrap decl) decls *> return ()
  where
    unwrap :: Decl -> TCM (S.Set String)
    unwrap (DTyDef x _) = checkCyclicTy (TyDef x) S.empty
    unwrap d = error $ "Impossible: checkCyclicTys.unwrap called on non-TyDef: " ++ show d

-- | Checks if a given type is cyclic. A type 'ty' is cyclic if:
-- 1.) 'ty' is a TyDef.
-- 2.) Repeated expansions of the TyDef yield nothing but other TyDefs.
-- 3.) An expansion of a TyDef yields another TyDef that has been previously encountered.
-- The function returns the set of TyDefs encountered during expansion if the TyDef is not cyclic.
checkCyclicTy :: Type -> S.Set String -> TCM (S.Set String)
checkCyclicTy (TyDef name) set = do
  case S.member name set of
    True -> throwError (CyclicTyDef name)
    False -> do
      ty <- lookupTyDefn name
      checkCyclicTy ty (S.insert name set)

checkCyclicTy _ set = return set


-- | Run a type checking computation in the context of some type
--   declarations. First check that there are no duplicate type
--   declarations; then run the computation in a context extended with
--   the declared types.
--
--   Precondition: only called on 'DType's.
withTypeDecls :: [Decl] -> TCM a -> TCM a
withTypeDecls decls k = do
  let dups :: [(Name Term, Int)]
      dups = filter ((>1) . snd) . map (head &&& length) . group . sort . map declName $ decls
  case dups of
    ((x,_):_) -> throwError (DuplicateDecls x)
    []        -> extends declCtx k
  where
    declCtx = M.fromList $ map getDType decls

    getDType (DType x ty) = (x,ty)
    getDType d            = error $ "Impossible! withTypeDecls.getDType called on non-DType: " ++ show d

-- | Type check a top-level definition. Precondition: only called on
--   'DDefn's.
checkDefn :: Decl -> TCM ()
checkDefn (DDefn x clauses) = do
  Forall sig <- lookupTy x
  prevDefn <- use (termDefns . Lens.at (coerce x))
  case prevDefn of
    Just _ -> throwError (DuplicateDefns x)
    Nothing -> do
      ((acs, ty'), theta) <- solve $ do
        checkNumPats clauses
        (nms, ty) <- unbind sig
        aclauses <- forAll nms $ mapM (checkClause ty) clauses
        return (aclauses, ty)

      let defnTy = substs theta ty'
          (patTys, bodyTy) = decomposeDefnTy (numPats (head clauses)) defnTy
      addDefn x (substs theta (Defn (coerce x) patTys bodyTy acs))
  where
    numPats = length . fst . unsafeUnbind

    checkNumPats []     = return ()   -- This can't happen, but meh
    checkNumPats [_]    = return ()
    checkNumPats (c:cs)
      | all ((==0) . numPats) (c:cs) = throwError (DuplicateDefns x)
      | not (all (== numPats c) (map numPats cs)) = throwError NumPatterns
               -- XXX more info, this error actually means # of
               -- patterns don't match across different clauses
      | otherwise = return ()

    checkClause :: Type -> Bind [Pattern] Term -> TCM Clause
    checkClause ty clause = do
      (pats, body) <- unbind clause
      (aps, at) <- go pats ty body
      return $ bind aps at

    go :: [Pattern] -> Type -> Term -> TCM ([APattern], ATerm)
    go [] ty body = do
     at <- check body ty
     return ([], at)
    go (p:ps) (TyArr ty1 ty2) body = do
      (ctx, apt) <- checkPattern p ty1
      (apts, at) <- extends ctx $ go ps ty2 body
      return (apt:apts, at)
    go _ _ _ = throwError NumPatterns   -- XXX include more info

    decomposeDefnTy 0 ty = ([], ty)
    decomposeDefnTy n (TyArr ty1 ty2) = first (ty1:) (decomposeDefnTy (n-1) ty2)
    decomposeDefnTy n ty = error $ "Impossible! decomposeDefnTy " ++ show n ++ " " ++ show ty

checkDefn d = error $ "Impossible! checkDefn called on non-Defn: " ++ show d

-- | Given a context mapping names to documentation, extract the
--   properties attached to each name and typecheck them.
checkProperties :: Ctx Term Docs -> TCM (Ctx ATerm [AProperty])
checkProperties docs =
  (M.mapKeys coerce . M.filter (not.null))
    <$> (traverse . traverse) checkProperty properties
  where
    properties :: Ctx Term [Property]
    properties = M.map (\ds -> [p | DocProperty p <- ds]) docs

-- | Check the types of the terms embedded in a property.
checkProperty :: Property -> TCM AProperty
checkProperty prop = do

  ((binds, at), theta) <- solve $ do
    -- A property looks like  forall (x1:ty1) ... (xn:tyn). term.
    (bs, t) <- unbind prop

    -- Extend the context with (x1:ty1) ... (xn:tyn) ...
    extends (M.fromList $ map (second toSigma) bs) $ do

    -- ... check that the term has type Bool ...
    a <- check t TyBool

    return (bs,a)

  -- Finally, apply the resulting substitution and fix up the types of
  -- the variables.
  return (bind (binds & traverse . _1 %~ coerce) (substs theta at))

{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

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

module Disco.Typecheck
       ( -- * Type checking monad
         TCM, TyCtx, runTCM, evalTCM, execTCM
         -- ** Definitions
       , Defn, DefnCtx
         -- ** Errors
       , TCError(..)

         -- * Type checking
       , check, checkPattern, checkDefn
       , checkProperties, checkProperty

         -- ** Whole modules
       , checkModule, withTypeDecls

         -- * Type inference
       , inferTop
       , infer
       , inferComp
         -- ** Case analysis
       , inferCase, inferBranch

         -- * Erasure
       , erase
       , eraseBinding, eraseBranch, eraseGuard
       , eraseLink, eraseQual, eraseProperty
       , erasePattern
       )
       where

import           Prelude                                 hiding (lookup)

import           Control.Applicative                     ((<|>))
import           Control.Arrow                           ((&&&))
import           Control.Lens                            ((%~), (&), _1)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                          (second)
import           Data.Coerce
import           Data.List                               (group, partition,
                                                          sort)
import qualified Data.Map                                as M

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Surface
import           Disco.AST.Typed

import           Disco.Context
import           Disco.Syntax.Operators
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Solve
import           Disco.Types
import           Disco.Types.Rules

-- | A definition is a group of clauses, each having a list of
--   patterns that bind names in a term, without the name of the
--   function being defined.  For example, given the concrete syntax
--   @f n (x,y) = n*x + y@, the corresponding 'Defn' would be
--   something like @[n, (x,y)] (n*x + y)@.
type Defn  = [Bind [APattern] ATerm]

-- | A map from names to definitions.
type DefnCtx = Ctx ATerm Defn

-- | A typing context is a mapping from term names to types.
type TyCtx = Ctx Term Sigma

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
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NotSubtractive Type
  | NotFractional Type
  | Unsolvable SolveError
  | NoError                -- ^ Not an error.  The identity of the
                           --   @Monoid TCError@ instance.
  deriving Show

-- | 'TCError' is a monoid where we simply discard the first error.
instance Monoid TCError where
  mempty = NoError
  mappend _ r = r

-- | Type checking monad. Maintains a context of variable types and a
--   set of definitions, and can throw @TCError@s and generate fresh
--   names.
type TCM = StateT DefnCtx (ReaderT TyCtx (ExceptT TCError FreshM))

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, DefnCtx)
runTCM = runFreshM . runExceptT . flip runReaderT emptyCtx . flip runStateT M.empty

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap fst . runTCM

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the resulting definitions.
execTCM :: TCM a -> Either TCError DefnCtx
execTCM = fmap snd . runTCM

-- | Solve a constraint set, generating a substitution (or failing
--   with an error).
solve :: Constraint -> TCM S
solve = either (throwError . Unsolvable) return . runSolveM . solveConstraint

-- | Add a definition to the set of current definitions.
addDefn :: Name Term -> Defn -> TCM ()
addDefn x b = modify (M.insert (coerce x) b)

-- | Look up the type of a variable in the context.  Throw an "unbound
--   variable" error if it is not found.
lookupTy :: Name Term -> TCM Sigma
lookupTy x = lookup x >>= maybe (throwError (Unbound x)) return

-- | Generates a type variable with a fresh name.
freshTy :: TCM Type
freshTy = TyVar <$> fresh (string2Name "a")

-- | Check that a term has the given sigma type.
checkSigma :: Term -> Sigma -> TCM (ATerm, Constraint)
checkSigma t (Forall sig) = do
  (as, tau) <- unbind sig
  (at, cst) <- check t tau
  return $ case as of
    [] -> (at, cst)
    _  -> (at, CAll (bind as cst))

-- Invariant:
--
-- IF
--   (at, _) <- check tm ty
-- THEN
--   getType at == ty

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
check :: Term -> Type -> TCM (ATerm, Constraint)

check (TParens t) ty = check t ty

check (TTup ts) ty = do
  (ats, csts) <- checkTuple ts ty
  return $ (ATTup ty ats, cAnd csts)

check t@(TBin Cons x xs) ty = do
  ([tyElt], cst1) <- ensureConstr CList ty (Left t)
  (ax, cst2) <- check x  tyElt
  (axs, cst3) <- check xs ty
  return $ (ATBin ty Cons ax axs, cAnd [cst1, cst2, cst3])

check t@(TContainer c xs ell) ty = do
  ([tyelt], cst1) <- ensureConstr (containerToCon c) ty (Left t)
  aclist <- mapM (flip check tyelt) xs
  let (axs, csts) = unzip aclist
  (aell, cst2) <- checkEllipsis ell tyelt
  return $ (ATContainer ty c axs aell, cAnd (cst1 : cst2 : csts))

check t@(TContainerComp c bqt) ty = do
  ([tyElt], cst1) <- ensureConstr (containerToCon c) ty (Left t)
  (qs, term) <- unbind bqt
  (aqs, cx, cst2) <- inferTelescope (inferQual c) qs
  extends cx $ do
  (at, cst3) <- check term tyElt
  return $ (ATContainerComp ty c (bind aqs at), cAnd [cst1, cst2, cst3])

check t@(TBin setOp t1 t2) ty | setOp `elem` [Union, Intersection, Difference, Subset] = do
  ([tyElt], cst) <- ensureConstr CSet ty (Left t)
  (at1, cst1) <- check t1 (TySet tyElt)
  (at2, cst2) <- check t2 (TySet tyElt)
  return (ATBin ty setOp at1 at2, cAnd [cst, cst1, cst2])

-- To check an abstraction:
check (TAbs lam) ty = do
  (args, t) <- unbind lam

  -- First check that the given type is of the form ty1 -> ty2 ->
  -- ... -> resTy, where the types ty1, ty2 ... match up with any
  -- types declared for the arguments to the lambda (e.g.  (x:tyA)
  -- (y:tyB) -> ...).
  (ctx, resTy, cst1) <- checkArgs args ty (TAbs lam)

  -- Then check the type of the body under a context extended with
  -- types for all the arguments.
  extends ctx $ do
  (at, cst2) <- check t resTy
  return (ATAbs ty (bind (coerce args) at), cAnd [cst1, cst2])

-- To check an injection has a sum type, recursively check the
-- relevant type.
check lt@(TInj L t) ty = do
  ([ty1, _], cst1) <- ensureConstr CSum ty (Left lt)
  (at, cst2) <- check t ty1
  return (ATInj ty L at, cAnd [cst1, cst2])

check rt@(TInj R t) ty = do
  ([_, ty2], cst1) <- ensureConstr CSum ty (Left rt)
  (at, cst2) <- check t ty2
  return (ATInj ty R at, cAnd [cst1, cst2])

-- Trying to check an injection under a non-sum type: error.
check t@(TInj _ _) ty = throwError (NotCon CSum t ty)

-- To check a let expression:
check (TLet l) ty = do
  (bs, t2) <- unbind l

  -- Infer the types of all the variables bound by the let...
  (as, ctx, cst1) <- inferTelescope inferBinding bs

  -- ...then check the body under an extended context.
  extends ctx $ do
    (at2, cst2) <- check t2 ty
    return $ (ATLet ty (bind as at2), cAnd [cst1, cst2])

check (TCase bs) ty = do
  aclist <- mapM (checkBranch ty) bs
  let (bs', clist) = unzip aclist
  return (ATCase ty bs', cAnd clist)

-- Once upon a time, when we only had types N, Z, Q+, and Q, we could
-- always infer the type of anything numeric, so we didn't need
-- checking cases for Add, Mul, etc.  But now we could have e.g.  (a +
-- b) : Z13, in which case we need to push the checking type (in the
-- example, Z13) through the operator to check the arguments.

-- Checking arithmetic binary operations involves checking both operands
-- and then adding a constraint qualifier over binary operation and the desired type.
check (TBin op t1 t2) ty | op `elem` [Add, Mul, Div, Sub, SSub] = do
  (at1, cst1) <- check t1 ty
  (at2, cst2) <- check t2 ty
  return (ATBin ty op at1 at2, cAnd [cst1, cst2, CQual (bopQual op) ty])


check (TBin Exp t1 t2) ty = do
  -- if a^b :: fractional t, then a :: t, b :: Z
  -- else if a^b :: non-fractional t, then a :: t, b :: N
  (at1, cst1) <- check t1 ty
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  (resTy, cst3) <- cExp ty1 ty2
  return $ (ATBin resTy Exp at1 at2, cAnd [cst1, cst2, cst3])

-- Note, we don't have the same special case for Neg as for Sub, since
-- unlike subtraction, which can sometimes make sense on N or F, it
-- never makes sense to negate a value of type N or F.
check (TUn Neg t) ty = do
  (at, cst) <- check t ty
  return $ (ATUn ty Neg at, cAnd [cst, CQual (QSub) ty])

check (TNat x) (TyFin n) =
  return $ (ATNat (TyFin n) x, CTrue)

-- Finally, to check anything else, we can fall back to inferring its
-- type and then check that the inferred type is a *subtype* of the
-- given type.  We have to be careful to call 'setType' to change the
-- type at the root of the term to the requested type.
check t ty = do
  (at, ct1) <- infer t
  let aty = getType at
  return $ (setType ty at, cAnd [ct1, CSub aty ty])

containerTy :: Container -> Type -> Type
containerTy c ty = TyCon (containerToCon c) [ty]

containerToCon :: Container -> Con
containerToCon ListContainer = CList
containerToCon SetContainer  = CSet
containerToCon MultisetContainer = CMultiset

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
checkArgs :: [(Name Term, Embed (Maybe Type))] -> Type -> Term ->  TCM (TyCtx, Type, Constraint)

-- If we're all out of arguments, the remaining checking type is the
-- result, and there are no variables to bind in the context.
checkArgs [] ty _ = return (emptyCtx, ty, CTrue)

-- Take the next variable and its annotation; the checking type must
-- be a function type ty1 -> ty2.
checkArgs ((x, unembed -> mty) : args) ty term = do

  -- Ensure that ty is a function type
  ([ty1, ty2], cst1) <- ensureConstr CArr ty (Left term)

  -- Figure out the type of x:
  (xTy, cst2) <- case mty of

    -- If it has no annotation, just take the input type ty1.
    Nothing    -> return (ty1, CTrue)

    -- If it does have an annotation, make sure the input type is a
    -- subtype of it.
    Just annTy -> return (ty1, CSub ty1 annTy)

  -- Check the rest of the arguments under the type ty2, returning a
  -- context with the rest of the arguments and the final result type.
  (ctx, resTy, cst3) <- checkArgs args ty2 term

  -- Pass the result type through, and add x with its type to the
  -- generated context.
  return (singleCtx x (toSigma xTy) `joinCtx` ctx, resTy, cAnd [cst1, cst2, cst3])


-- | Check the types of terms in a tuple against a nested
--   pair type.
checkTuple :: [Term] -> Type -> TCM ([ATerm], [Constraint])
checkTuple [] _   = error "Impossible! checkTuple []"
checkTuple [t] ty = do     -- (:[]) <$> check t ty
  (at, cst) <- check t ty
  return ([at], [cst])
checkTuple (t:ts) ty = do
  ([ty1, ty2], cst1) <- ensureConstr CPair ty (Left $ TTup  (t:ts))
  (at, cst2) <- check t ty1
  (ats, csts) <- checkTuple ts ty2
  return $ (at : ats, cst1 : cst2 : csts)

checkEllipsis :: Maybe (Ellipsis Term) -> Type -> TCM (Maybe (Ellipsis ATerm), Constraint)
checkEllipsis Nothing          _  = return (Nothing, CTrue)
checkEllipsis (Just Forever)   _  = return (Just Forever, CTrue)
checkEllipsis (Just (Until t)) ty = do
  (at, ct) <- check t ty
  return $ ((Just . Until) at, ct)

-- | Check the type of a branch, returning a type-annotated branch.
checkBranch :: Type -> Branch -> TCM (ABranch, Constraint)
checkBranch ty b = do
  (gs, t) <- unbind b
  (ags, ctx, cst1) <- inferTelescope inferGuard gs
  extends ctx $ do
  (at, cst2) <- check t ty
  return $ (bind ags at, cAnd [cst1, cst2])

-- | Generates appropriate constraints for the type of an operand of
--   an absolute value function along with constraints for the type of the result.
cPos :: Type -> TCM (Type, Constraint)
cPos ty@(TyAtom (ABase b)) = return (TyAtom (ABase (pos b)), CQual QNum ty)  -- Has to be QNum!!
  where
    pos Z = N
    pos Q = F
    pos _ = b

cPos ty                 = do
  res <- freshTy
  return (res, CAnd
               [ CQual QNum ty
               , COr
                 [ cAnd [CSub ty TyZ, CSub TyN res]
                 , cAnd [CSub ty TyQ, CSub TyF res]
                 , CEq ty res
                 ]
               ])

-- | Generates appropriate constraints for the type of an operand of a
--   ceiling or floor function along with constraints for the type of the result.
cInt :: Type -> TCM (Type, Constraint)
cInt ty@(TyAtom (ABase b)) = return (TyAtom (ABase (int b)), CQual QNum ty)
  where
    int F = N
    int Q = Z
    int _ = b

cInt ty                 = do
  res <- freshTy
  return (res, CAnd
               [ CQual QNum ty
               , COr
                 [ cAnd [CSub ty TyF, CSub TyN res]
                 , cAnd [CSub ty TyQ, CSub TyZ res]
                 , CEq ty res
                 ]
               ])

-- | Generates appropriate constraints for the types of the operands of the
--   exponent function along with constraints for the type of the result.
cExp :: Type -> Type -> TCM (Type, Constraint)
cExp ty1 ty2            = do
  tyv1 <- freshTy
  return (tyv1, COr
                 [ cAnd [CQual QNum tyv1, CEq ty2 TyN, CSub ty1 tyv1]
                 , cAnd [CQual QDiv tyv1, CEq ty2 TyZ, CSub ty1 tyv1]
                 ])

-- | Infer the (polymorphic) type of a term.
inferTop :: Term -> TCM (ATerm, Sigma)
inferTop t = do
  (at, constr) <- infer t
  traceShowM at
  theta <- solve constr
  let at' = substs theta at
  return (at', closeSigma (getType at'))

-- | Infer the type of a term, along with some constraints.  If it
--   succeeds, it returns the term with all subterms annotated.
infer :: Term -> TCM (ATerm, Constraint)

infer (TParens t)   = infer t

  -- To infer the type of a variable, just look it up in the context.
infer (TVar x)      = do
  sig <- lookupTy x
  ty <- inferSubsumption sig
  return $ (ATVar ty (coerce x), CTrue)

  -- A few trivial cases.
infer TUnit         = return (ATUnit, CTrue)
infer (TBool b)     = return $ (ATBool b, CTrue)
infer (TNat n)      = return $ (ATNat TyN n, CTrue)
infer (TRat r)      = return $ (ATRat r, CTrue)

-- To infer an injection has a sum type, recursively check the
-- relevant type, and generate a type
infer lt@(TInj L t) = do
  (at, cst) <- infer t
  let ty = getType at
  tyv <- freshTy
  return (ATInj (TySum ty tyv) L at, cst)

infer lt@(TInj R t) = do
  (at, cst) <- infer t
  let ty = getType at
  tyv <- freshTy
  return (ATInj (TySum tyv ty) R at, cst)

  -- We can infer the type of a lambda if the variable is annotated
  -- with a type.
infer (TAbs lam)    = do
  (args, t) <- unbind lam
  let (xs, mtys) = unzip args
  case sequence (map unembed mtys) of
    Nothing  -> throwError (CantInfer (TAbs lam))
    Just tys -> extends (M.fromList $ zip xs (map toSigma tys)) $ do
      (at, cst) <- infer t
      return (ATAbs (mkFunTy tys (getType at))
                     (bind (zip (map coerce xs) (map (embed . Just) tys)) at), cst)
  where
    -- mkFunTy [ty1, ..., tyn] out = ty1 -> (ty2 -> ... (tyn -> out))
    mkFunTy :: [Type] -> Type -> Type
    mkFunTy tys out = foldr TyArr out tys

infer (TPrim "mapSet") = do
  a <- freshTy
  b <- freshTy
  return $ (ATPrim (TyArr (TyArr a b) (TyArr (TySet a) (TySet b))) "mapSet", CTrue)

infer (TPrim "setToMultiset") = do
  a <- freshTy
  return  (ATPrim (TyArr (TySet a) (TyMultiset a)) "setToMultiset", CTrue)

infer (TPrim "multisetToSet") = do
  a <- freshTy
  return (ATPrim (TyArr (TyMultiset a) (TySet (TyPair a TyN))) "multisetToSet", CTrue)

infer (TPrim "foldSet") = do
  a <- freshTy
  b <- freshTy
  return $ (ATPrim (TyArr (TyArr a (TyArr b b)) (TyArr b (TyArr (TySet a) b))) "foldSet", CTrue)

infer (TPrim "foldMultiset") = do
  a <- freshTy
  b <- freshTy
  return $ (ATPrim (TyArr (TyArr a (TyArr b b)) (TyArr b (TyArr (TyMultiset a) b))) "foldMultiset", CTrue)
  -- Infer the type of a function application by inferring the
  -- function type and then checking the argument type.
infer (TApp t t')   = do
  (at, cst1) <- infer t
  let ty = getType at
  ([ty1, ty2], cst2) <- ensureConstr CArr ty (Left t)
  (at', cst3) <- check t' ty1
  return (ATApp ty2 at at', cAnd [cst1, cst2, cst3])

  -- To infer the type of a pair, just infer the types of both components.
infer (TTup ts) = do
  (ty, ats, csts) <- inferTuple ts
  return (ATTup ty ats, cAnd csts)

  -- To infer the type of addition or multiplication, infer the types
  -- of the subterms, check that they are numeric, and return their
  -- lub.
infer (TBin op t1 t2) | op `elem` [Add, Mul, Sub, Div, SSub] = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  return (ATBin tyv op at1 at2, cAnd [cst1, cst2, CSub ty1 tyv, CSub ty2 tyv, CQual (bopQual op) tyv])

infer (TUn Neg t) = do
  (at, cst) <- infer t
  let ty = getType at
  tyv <- freshTy
  return $ (ATUn tyv Neg at, cAnd [cst, CSub ty tyv, CQual QSub tyv])

infer (TUn op t) | op `elem` [Sqrt, Lg] = do
  (at, cst) <- check t TyN
  return (ATUn TyN op at, cst)

infer (TUn op t) | op `elem` [Floor, Ceil] = do
  (at, cst) <- infer t
  let ty = getType at
  (resTy, cst2) <- cInt ty
  return (ATUn resTy op at, cAnd [cst, cst2])

infer (TUn Abs t) = do
  (at, cst) <- infer t
  let ty = getType at
  (resTy, cst2) <- cPos ty
  return (ATUn resTy Abs at, cAnd [cst, cst2])

 -- Very similar to division
infer (TBin IDiv t1 t2) = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv1 <- freshTy
  (resTy, cst3) <- cInt tyv1
  return (ATBin resTy IDiv at1 at2, cAnd [cst1, cst2, cst3, CSub ty1 tyv1, CSub ty2 tyv1])

infer (TBin Exp t1 t2) = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  (resTy, cst3) <- cExp ty1 ty2
  return (ATBin resTy Exp at1 at2, cAnd [cst1, cst2, cst3])

  -- An equality or inequality test always has type Bool, but we need
  -- to check that they have a common supertype.
infer (TBin eqOp t1 t2) | eqOp `elem` [Eq, Neq] = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  return (ATBin TyBool eqOp at1 at2, cAnd [cst1, cst2, CSub ty1 tyv, CSub ty2 tyv])

infer (TBin op t1 t2)
  | op `elem` [Lt, Gt, Leq, Geq] = inferComp op t1 t2

  -- &&, ||, and not always have type Bool, and the subterms must have type
  -- Bool as well.
infer (TBin op t1 t2) | op `elem` [And, Or, Impl] = do
  (at1, cst1) <- check t1 TyBool
  (at2, cst2) <- check t2 TyBool
  return (ATBin TyBool op at1 at2, cAnd [cst1, cst2])

infer (TUn Not t) = do
  (at, cst) <- check t TyBool
  return (ATUn TyBool Not at, cst)

infer (TBin Mod t1 t2) = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  return (ATBin tyv Mod at1 at2, cAnd [cst1, cst2, CSub ty1 tyv, CSub ty2 tyv, CSub tyv TyZ])


infer (TBin Divides t1 t2) = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  return (ATBin TyBool Divides at1 at2, cAnd [cst1, cst2, CSub ty1 tyv, CSub ty2 tyv, CQual QNum tyv])

-- For now, a simple typing rule for multinomial coefficients that
-- requires everything to be Nat.  However, they can be extended to
-- handle negative or fractional arguments.
infer (TBin Choose t1 t2) = do
  (at1, cst1) <- check t1 TyN

  -- t2 can be either a Nat (a binomial coefficient)
  -- or a list of Nat (a multinomial coefficient).
  (at2, cst2) <- check t2 TyN <|> check t2 (TyList TyN)
  return (ATBin TyN Choose at1 at2, cAnd [cst1, cst2])

-- To infer the type of a cons:
infer (TBin Cons t1 t2) = do

  -- First, infer the type of the first argument (a list element).
  (at1, cst1) <- infer t1

  case t2 of
    -- If the second argument is the empty list, just assign it the
    -- type inferred from the first element.
    TList [] Nothing -> do
      let ty1 = getType at1
      return (ATBin (TyList ty1) Cons at1 (ATList (TyList ty1) [] Nothing), cst1)

    -- Otherwise, infer the type of the second argument...
    _ -> do
      (at2, cst2) <- infer t2
      case (getType at2) of

        -- ...make sure it is a list, and find the lub of the element types.
        TyList ty2 -> do
          let ty1 = getType at1
          tyv <- freshTy
          return (ATBin (TyList tyv) Cons at1 at2, cAnd [cst1, cst2, CSub ty1 tyv, CSub ty2 tyv])
        ty -> throwError (NotCon CList t2 ty)

--To infer the type of the size of a list:
infer (TUn Size t) = do
  (at, cst0) <- infer t
  ([tyElt], cst1) <- ensureConstr CSet (getType at) (Left t)
  return (ATUn TyN Size at, cAnd [cst0, cst1])

infer (TUn PowerSet t) = do
  (at, cst1) <- infer t
  ([tyElt], cst2) <- ensureConstr CSet (getType at) (Left t)
  return (ATUn (TySet (TySet tyElt)) PowerSet at, cAnd [cst1, cst2])

infer (TBin setOp t1 t2) | setOp `elem` [Union, Intersection, Difference, Subset] = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  tyelt <- freshTy
  let ty1 = getType at1
  let ty2 = getType at2
  let ty = case setOp of {Subset -> TyBool; _ -> TySet tyelt}
  return (ATBin ty setOp at1 at2, cAnd [cst1, cst2, CSub ty1 (TySet tyelt), CSub ty2 (TySet tyelt)])

infer (TUn Fact t) = do
  (at, cst) <- check t TyN
  return (ATUn TyN Fact at, cst)

infer (TChain t1 links) = do
  (at1, cst1) <- infer t1
  (alinks, cst2) <- inferChain t1 links
  return (ATChain TyBool at1 alinks, cAnd [cst1, cAnd cst2])

infer (TContainer c [] ell) = do
  tyv <- freshTy
  (aell, cstell) <- inferEllipsis ell
  return (ATContainer (containerTy c tyv) c [] aell, cstell)

infer (TContainer c es@(_:_) ell)  = do
  list1 <- mapM infer es
  let (ates, cstes) = unzip list1
  (aell, cstell) <- inferEllipsis ell
  let tys = [ getType at | Just (Until at) <- [aell] ] ++ (map getType) ates
  tyv  <- freshTy
  return ( ATContainer (containerTy c tyv) c ates aell
         , cAnd [cAnd cstes, cstell, cAnd (subs tys tyv)]
         )
    where subs tylist ty = map (flip CSub ty) tylist

infer (TContainerComp c bqt) = do
  (qs, t) <- unbind bqt
  (aqs, cx, cst1) <- inferTelescope (inferQual c) qs
  extends cx $ do
  (at, cst2) <- infer t
  let ty = getType at
  return (ATContainerComp (containerTy c ty) c (bind aqs at)
         , cAnd [cst1, cst2]
         )

infer (TTyOp Enumerate t) = do
  return (ATTyOp (TyList t) Enumerate t, CTrue)

infer (TTyOp Count t) = do
  return (ATTyOp (TySum TyUnit TyN) Count t, CTrue)

  -- To infer the type of (let x = t1 in t2), assuming it is
  -- NON-RECURSIVE, infer the type of t1, and then infer the type of
  -- t2 in an extended context.
infer (TLet l) = do
  (bs, t2) <- unbind l
  (as, ctx, cst1) <- inferTelescope inferBinding bs
  extends ctx $ do
  (at2, cst2) <- infer t2
  return (ATLet (getType at2) (bind as at2), cAnd [cst1, cst2])

  -- Ascriptions are what let us flip from inference mode into
  -- checking mode.
  -- XXX: WHAT should I do since there will no longer be an ATAsrc
infer (TAscr t ty) = do
  (at, cst) <- checkSigma t ty
  return (at, cst)

infer (TCase []) = throwError EmptyCase
infer (TCase bs) = inferCase bs

  -- Catch-all case at the end: if we made it here, we can't infer it.
infer t = throwError (CantInfer t)

-- | Recover a Type type from a Sigma type by pulling out the type within
--   the Forall constructor
inferSubsumption :: Sigma -> TCM Type
inferSubsumption (Forall sig) = snd <$> unbind sig

-- | Infer the type of a binding (@x [: ty] = t@), returning a
--   type-annotated binding along with a (singleton) context for the
--   bound variable.  The optional type annotation on the variable
--   determines whether we use inference or checking mode for the
--   body.
inferBinding :: Binding -> TCM (ABinding, TyCtx, Constraint)
inferBinding (Binding mty x (unembed -> t)) = do
  (at, cst) <- case mty of
    Just (unembed -> ty) -> checkSigma t ty
    Nothing              -> infer t
  return $ (ABinding mty (coerce x) (embed at), singleCtx x (toSigma $ getType at), cst)

-- | Infer the type of a comparison. A comparison always has type
--   Bool, but we have to make sure the subterms are OK. We must check
--   that their types are compatible and have a total order.
inferComp :: BOp -> Term -> Term -> TCM (ATerm, Constraint)
inferComp comp t1 t2 = do
  (at1, cst1) <- infer t1
  (at2, cst2) <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyv <- freshTy
  return (ATBin TyBool comp at1 at2, cAnd [CSub ty1 tyv, CSub ty2 tyv, cst1, cst2])

inferChain :: Term -> [Link] -> TCM ([ALink], [Constraint])
inferChain _  [] = return ([], [CTrue])
inferChain t1 (TLink op t2 : links) = do
  (at2, cst1) <- infer t2
  (_, cst2)   <- check (TBin op t1 t2) TyBool
  (atl, cst3) <- inferChain t2 links
  return (ATLink op at2 : atl, cst1 : cst2 : cst3)

inferEllipsis :: Maybe (Ellipsis Term) -> TCM (Maybe (Ellipsis ATerm), Constraint)
inferEllipsis (Just (Until t)) = do
  (at, cst) <- infer t
  return ((Just . Until) at, cst)

inferEllipsis (Just Forever)   = return (Just Forever, CTrue)
inferEllipsis Nothing          = return (Nothing, CTrue)

inferTuple :: [Term] -> TCM (Type, [ATerm], [Constraint])
inferTuple []     = error "Impossible! inferTuple []"
inferTuple [t]    = do
  (at, cst) <- infer t
  return (getType at, [at], [cst])
inferTuple (t:ts) = do
  (at, cst) <- infer t
  (ty, ats, csts) <- inferTuple ts
  return (TyPair (getType at) ty, at : ats, cst : csts)

-- | Infer the type of a case expression.  The result type is the
--   least upper bound (if it exists) of all the branches.
inferCase :: [Branch] -> TCM (ATerm, Constraint)
inferCase bs = do
  bs' <- mapM inferBranch bs
  let (branchTys, abs', csts) = unzip3 bs'
  resTy <- freshTy
  return (ATCase resTy abs', cAnd [cAnd csts, cAnd $ sub branchTys resTy])
    where sub tylist ty = map (flip CSub ty) tylist

-- | Infer the type of a case branch, returning the type along with a
--   type-annotated branch.
inferBranch :: Branch -> TCM (Type, ABranch, Constraint)
inferBranch b = do
  (gs, t) <- unbind b
  (ags, ctx, cst1) <- inferTelescope inferGuard gs
  extends ctx $ do
  (at, cst2) <- infer t
  return $ (getType at, bind ags at, cAnd [cst1, cst2])

-- | Infer the type of a telescope, given a way to infer the type of
--   each item along with a context of variables it binds; each such
--   context is then added to the overall context when inferring
--   subsequent items in the telescope.
inferTelescope
  :: (Alpha b, Alpha tyb)
  => (b -> TCM (tyb, TyCtx, Constraint)) -> Telescope b -> TCM (Telescope tyb, TyCtx, Constraint)
inferTelescope inferOne tel = do
  (tel1, ctx, cst) <- go (fromTelescope tel)
  return $ (toTelescope tel1, ctx, cst)
  where
    go []     = return ([], emptyCtx, CTrue)
    go (b:bs) = do
      (tyb, ctx, cst1) <- inferOne b
      extends ctx $ do
      (tybs, ctx', cst2) <- go bs
      return (tyb:tybs, ctx `joinCtx` ctx', cAnd [cst1, cst2])

-- | Infer the type of a guard, returning the type-annotated guard
--   along with a context of types for any variables bound by the guard.
inferGuard :: Guard -> TCM (AGuard, TyCtx, Constraint)
inferGuard (GBool (unembed -> t)) = do
  (at, cst) <- check t TyBool
  return (AGBool (embed at), emptyCtx, cst)
inferGuard (GPat (unembed -> t) p) = do
  (at, cst1) <- infer t
  (ctx, apt, cst2) <- checkPattern p (getType at)
  return (AGPat (embed at) apt, ctx, cAnd [cst1, cst2])

inferQual :: Container -> Qual -> TCM (AQual, TyCtx, Constraint)
inferQual c (QBind x (unembed -> t))  = do
  (at, cst) <- infer t
  case (c, getType at) of
    (_, TyList ty)   -> return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty), cst)
    (SetContainer, TySet ty) -> return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty), cst)
    (MultisetContainer, TyMultiset ty) -> return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty), cst)
    (_, wrongTy)   -> throwError $ NotCon (containerToCon c) t wrongTy

inferQual _ (QGuard (unembed -> t))   = do
  (at, cst) <- check t TyBool
  return (AQGuard (embed at), emptyCtx, cst)

-- | Check that a pattern has the given type, and return a context of
--   pattern variables bound in the pattern along with their types.
checkPattern :: Pattern -> Type -> TCM (TyCtx, APattern, Constraint)
checkPattern (PVar x) ty                    = return (singleCtx x (toSigma ty), APVar (coerce x), CTrue)

checkPattern PWild    _                     = return (emptyCtx, APWild, CTrue)

checkPattern PUnit tyv@(TyVar _)            = return (emptyCtx, APUnit, CEq tyv TyUnit)
checkPattern PUnit TyUnit                   = return (emptyCtx, APUnit, CTrue)

checkPattern PUnit tyv@(TyVar _)            = return (emptyCtx, APUnit, CEq tyv TyBool)
checkPattern (PBool b) TyBool               = return (emptyCtx, APBool b, CTrue)

checkPattern (PTup ps) ty                   = do
  listCtxtAps <- checkTuplePat ps ty
  let (ctxs, aps, csts) = unzip3 listCtxtAps
  return (joinCtxs ctxs, APTup aps, cAnd csts)
checkPattern p@(PInj L pat) ty       = do
  ([ty1, _], cst1) <- ensureConstr CSum ty (Right p)
  (ctx, apt, cst2) <- checkPattern pat ty1
  return (ctx, APInj L apt, cAnd [cst1, cst2])
checkPattern p@(PInj R pat) ty    = do
  ([_, ty2], cst1) <- ensureConstr CSum ty (Right p)
  (ctx, apt, cst2) <- checkPattern pat ty2
  return (ctx, APInj R apt, cAnd [cst1, cst2])

-- we can match any supertype of TyN against a Nat pattern, OR
-- any TyFin.
checkPattern (PNat n) (TyFin _) = return (emptyCtx, APNat n, CTrue)
checkPattern (PNat n) ty = return (emptyCtx, APNat n, CSub TyN ty)

checkPattern (PSucc p) tyv@(TyVar _)               = do
  (ctx, apt, cst) <- checkPattern p TyN
  return (ctx, APSucc apt, cAnd [cst, CEq tyv TyN])

checkPattern (PSucc p) TyN                 = do
  (ctx, apt, cst) <- checkPattern p TyN
  return (ctx, APSucc apt, cst)

checkPattern p@(PCons p1 p2) ty      = do
  ([tyl], cst1) <- ensureConstr CList ty (Right p)
  (ctx1, ap1, cst2) <- checkPattern p1 tyl
  (ctx2, ap2, cst3) <- checkPattern p2 (TyList tyl)
  return (joinCtx ctx1 ctx2, APCons ap1 ap2, cAnd [cst1, cst2, cst3])

checkPattern p@(PList ps) ty         = do
  ([tyl], cst) <- ensureConstr CList ty (Right p)
  listCtxtAps <- mapM (flip checkPattern tyl) ps
  let (ctxs, aps, csts) = unzip3 listCtxtAps
  return (joinCtxs ctxs, APList aps, cAnd (cst:csts))

checkPattern p ty = throwError (PatternType p ty)

checkTuplePat :: [Pattern] -> Type -> TCM ([(TyCtx, APattern, Constraint)])
checkTuplePat [] _   = error "Impossible! checkTuplePat []"
checkTuplePat [p] ty = do     -- (:[]) <$> check t ty
  (ctx, apt, cst) <- checkPattern p ty
  return [(ctx, apt, cst)]
checkTuplePat (p:ps) ty = do
  ([ty1, ty2], cst1)  <- ensureConstr CPair ty (Right $ PTup (p:ps))
  (ctx, apt, cst2)  <- checkPattern p ty1
  rest <- checkTuplePat ps ty2
  return ((ctx, apt, cAnd [cst1, cst2]) : rest)

-- | Check all the types in a module, returning a context of types for
--   top-level definitions.
checkModule :: Module -> TCM (Ctx Term Docs, Ctx ATerm [AProperty], TyCtx)
checkModule (Module m docs) = do
  let (defns, typeDecls) = partition isDefn m
  withTypeDecls typeDecls $ do
    mapM_ checkDefn defns
    aprops <- checkProperties docs
    ctx <- ask
    return (docs, aprops, ctx)

-- | Ensures that a type's outermost constructor matches the provided constructor,
--   returning the types within the matched constructor or throwing a type error.
--   If the type provided is a type variable, appropriate constraints are generated
--   to guarentee the type variable's outermost constructor matches the provided
--   constructor, and a list of type variables is returned whose count matches the
--   arity of the provided constructor.
ensureConstr :: Con -> Type -> Either Term Pattern -> TCM ([Type], Constraint)
ensureConstr c1 (TyCon c2 tys) _ | c1 == c2 = return (tys, CTrue)

ensureConstr c tyv@(TyVar _) _ = do
  tyvs <- mapM (const freshTy) (arity c)
  return (tyvs, CEq tyv (TyCon c tyvs))

ensureConstr c ty targ = case targ of
                           Left term -> throwError (NotCon c term ty)
                           Right pat -> throwError (PatternType pat ty)

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
  prevDefn <- gets (M.lookup (coerce x))
  case prevDefn of
    Just _ -> throwError (DuplicateDefns x)
    Nothing -> do
      checkNumPats clauses
      (nms, ty) <- unbind sig
      aclist <- mapM (checkClause ty) clauses
      let (aclauses, csts) = unzip aclist
          cst = cAnd csts
      theta <- solve $ CAll (bind nms cst)
      addDefn x (substs theta aclauses)
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

    checkClause :: Type -> Bind [Pattern] Term -> TCM (Bind [APattern] ATerm, Constraint)
    checkClause ty clause = do
      (pats, body) <- unbind clause
      (aps, at, cst) <- go pats ty body
      return (bind aps at, cst)


    go :: [Pattern] -> Type -> Term -> TCM ([APattern], ATerm, Constraint)
    go [] ty body = do
     (at, cst) <- check body ty
     return ([], at, cst)
    go (p:ps) (TyArr ty1 ty2) body = do
      (ctx, apt, cst1) <- checkPattern p ty1
      (apts, at, cst2) <- extends ctx $ go ps ty2 body
      return (apt:apts, at, cAnd [cst1, cst2])
    go _ _ _ = throwError NumPatterns   -- XXX include more info

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

  -- A property looks like  forall (x1:ty1) ... (xn:tyn). term.
  (binds, t) <- unbind prop

  -- Extend the context with (x1:ty1) ... (xn:tyn) ...
  extends (M.fromList $ map (second toSigma) binds) $ do

  -- ... check that the term has type Bool ...
  (at, cst) <- check t TyBool

  -- ... and solve the resulting constraints.
  theta <- solve cst

  -- Finally, apply the resulting substitution and fix up the types of
  -- the variables.
  return (bind (binds & traverse . _1 %~ coerce) (substs theta at))

------------------------------------------------------------
-- Erasure
------------------------------------------------------------

-- | Erase all the type annotations from a term.
erase :: ATerm -> Term
erase (ATVar _ x)           = TVar (coerce x)
erase (ATLet _ bs)          = TLet $ bind (mapTelescope eraseBinding tel) (erase at)
  where (tel,at) = unsafeUnbind bs
erase ATUnit                = TUnit
erase (ATBool b)            = TBool b
erase (ATNat _ i)           = TNat i
erase (ATRat r)             = TRat r
erase (ATAbs _ b)           = TAbs $ bind (coerce x) (erase at)
  where (x,at) = unsafeUnbind b
erase (ATApp _ t1 t2)       = TApp (erase t1) (erase t2)
erase (ATTup _ ats)         = TTup (map erase ats)
erase (ATInj _ s at)        = TInj s (erase at)
erase (ATCase _ brs)        = TCase (map eraseBranch brs)
erase (ATUn _ uop at)       = TUn uop (erase at)
erase (ATBin _ bop at1 at2) = TBin bop (erase at1) (erase at2)
erase (ATChain _ at lnks)   = TChain (erase at) (map eraseLink lnks)
erase (ATTyOp _ op ty)      = TTyOp op ty
erase (ATList _ ats aell)   = TList (map erase ats) ((fmap . fmap) erase aell)
erase (ATListComp _ b)      = TListComp $ bind (mapTelescope eraseQual tel) (erase at)
  where (tel,at) = unsafeUnbind b

eraseBinding :: ABinding -> Binding
eraseBinding (ABinding mty x (unembed -> at)) = Binding mty (coerce x) (embed (erase at))

erasePattern :: APattern -> Pattern
erasePattern (APVar n)        = PVar (coerce n)
erasePattern APWild           = PWild
erasePattern APUnit           = PUnit
erasePattern (APBool b)       = PBool b
erasePattern (APTup alp)      = PTup $ map erasePattern alp
erasePattern (APInj s apt)    = PInj s (erasePattern apt)
erasePattern (APNat n)        = PNat n
erasePattern (APSucc apt)     = PSucc $ erasePattern apt
erasePattern (APCons ap1 ap2) = PCons (erasePattern ap1) (erasePattern ap2)
erasePattern (APList alp)     = PList $ map erasePattern alp

eraseBranch :: ABranch -> Branch
eraseBranch b = bind (mapTelescope eraseGuard tel) (erase at)
  where (tel,at) = unsafeUnbind b

eraseGuard :: AGuard -> Guard
eraseGuard (AGBool (unembed -> at))  = GBool (embed (erase at))
eraseGuard (AGPat (unembed -> at) p) = GPat (embed (erase at)) (erasePattern p)

eraseLink :: ALink -> Link
eraseLink (ATLink bop at) = TLink bop (erase at)

eraseQual :: AQual -> Qual
eraseQual (AQBind x (unembed -> at)) = QBind (coerce x) (embed (erase at))
eraseQual (AQGuard (unembed -> at))  = QGuard (embed (erase at))

eraseProperty :: AProperty -> Property
eraseProperty b = bind (coerce xs) (erase at)
  where (xs, at) = unsafeUnbind b

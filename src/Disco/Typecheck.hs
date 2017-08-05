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
         TCM, runTCM, evalTCM, execTCM
         -- ** Definitions
       , Defn, Defns
         -- ** Contexts
       , Ctx, emptyCtx, singleCtx, joinCtx, joinCtxs
       , lookup, extend, extends, addDefn
         -- ** Errors
       , TCError(..)

         -- * Type predicates
       , isSubtractive, isFractional

         -- * Type checking
       , check, checkPattern, ok, checkDefn
       , checkPropertyTypes

         -- ** Whole modules
       , checkModule, withTypeDecls
         -- ** Subtyping
       , checkSub, isSub, lub, numLub
         -- ** Decidability
       , isFinite
       , isDecidable, checkDecidable
       , isOrdered, checkOrdered

       , requireSameTy
       , getFunTy
       , checkNumTy

         -- * Type inference
       , infer
       , inferComp
         -- ** Case analysis
       , inferCase, inferBranch
       )
       where

-- import Debug.Trace

import           Prelude                                 hiding (lookup)

import           Control.Applicative                     ((<|>))
import           Control.Arrow                           ((&&&))
import           Control.Lens                            ((%~), (&), _1, _2)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                          (first, second)
import           Data.Coerce
import           Data.List                               (group, partition,
                                                          sort)
import qualified Data.Map                                as M

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Syntax.Operators
import           Disco.Types

import           Math.NumberTheory.Primes.Testing        (isPrime)

-- | A definition is a group of clauses, each having a list of
--   patterns that bind names in a term, without the name of the
--   function being defined.  For example, given the concrete syntax
--   @f n (x,y) = n*x + y@, the corresponding 'Defn' would be
--   something like @[n, (x,y)] (n*x + y)@.
type Defn  = [Bind [Pattern] ATerm]

-- | A map from names to definitions.
type Defns = M.Map (Name ATerm) Defn

-- | A typing context, /i.e./ a map from names to types.
type Ctx = M.Map (Name Term) Type

-- | Potential typechecking errors.
data TCError
  = Unbound (Name Term)    -- ^ Encountered an unbound variable
  | NotArrow Term Type     -- ^ The type of a lambda should be an arrow type but isn't
  | NotFun   ATerm         -- ^ The term should be a function but has a non-arrow type
  | NotSum Term Type       -- ^ The term is an injection but is
                           --   expected to have some type other than
                           --   a sum type
  | NotTuple Term Type     -- ^ The term is a tuple but has a type
                           --   which is not an appropriate product type
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
  | RelPmQ                 -- ^ Can't ask about relative primality of rationals.
  | DuplicateDecls (Name Term)  -- ^ Duplicate declarations.
  | DuplicateDefns (Name Term)  -- ^ Duplicate definitions.
  | NumPatterns            -- ^ # of patterns does not match type in definition
  | NotList Term Type      -- ^ Should have a list type, but expected to have some other type
  | NotSubtractive Type
  | NotFractional Type
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
type TCM = StateT Defns (ReaderT Ctx (ExceptT TCError LFreshM))

-- | Run a 'TCM' computation starting in the empty context.
runTCM :: TCM a -> Either TCError (a, Defns)
runTCM = runLFreshM . runExceptT . flip runReaderT emptyCtx . flip runStateT M.empty

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the result of the computation.
evalTCM :: TCM a -> Either TCError a
evalTCM = fmap fst . runTCM

-- | Run a 'TCM' computation starting in the empty context, returning
--   only the resulting definitions.
execTCM :: TCM a -> Either TCError Defns
execTCM = fmap snd . runTCM

-- | The empty context.
emptyCtx :: Ctx
emptyCtx = M.empty

-- | A singleton context, mapping a name to a type.
singleCtx :: Name Term -> Type -> Ctx
singleCtx = M.singleton

-- | Join two contexts (left-biased).
joinCtx :: Ctx -> Ctx -> Ctx
joinCtx = M.union

-- | Join a list of contexts (left-biased).
joinCtxs :: [Ctx] -> Ctx
joinCtxs = M.unions

-- | Look up a name in the context, either returning its type or
--   throwing an unbound variable exception if it is not found.
lookup :: Name Term -> TCM Type
lookup x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError (Unbound x)
    Just ty -> return ty

-- | Run a @TCM@ computation in a context extended with a new binding.
--   The new binding shadows any old binding for the same name.
extend :: Name Term -> Type -> TCM a -> TCM a
extend x ty = local (M.insert x ty)

-- | Run a @TCM@ computation in a context extended with an additional
--   context.  Bindings in the additional context shadow any bindings
--   with the same names in the existing context.
extends :: Ctx -> TCM a -> TCM a
extends ctx = local (joinCtx ctx)

-- | Add a definition to the set of current definitions.
addDefn :: Name Term -> [Bind [Pattern] ATerm] -> TCM ()
addDefn x b = modify (M.insert (coerce x) b)

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
check :: Term -> Type -> TCM ATerm

check (TParens t) ty = check t ty

check (TTup ts) ty = do
  ats <- checkTuple ts ty
  return $ ATTup ty ats

check (TList xs ell) ty@(TyList eltTy) = do
  axs  <- mapM (flip check eltTy) xs
  aell <- checkEllipsis ell eltTy
  return $ ATList ty axs aell

check l@(TList _ _) ty            = throwError (NotList l ty)

check (TBin Cons x xs) ty@(TyList eltTy) = do
  ax  <- check x  eltTy
  axs <- check xs ty
  return $ ATBin ty Cons ax axs

check t@(TBin Cons _ _) ty     = throwError (NotList t ty)

check (TListComp bqt) ty@(TyList eltTy) = do
  lunbind bqt $ \(qs,t) -> do
  (aqs, cx) <- inferTelescope inferQual qs
  extends cx $ do
  at <- check t eltTy
  return $ ATListComp ty (bind aqs at)

check (TListComp bqt) ty    = throwError (NotList (TListComp bqt) ty)

  -- To check that an abstraction has an arrow type, check that the
  -- body has the return type under an extended context.
check (TAbs lam) ty@(TyArr ty1 ty2) = do
  lunbind lam $ \(x,t) -> do
  extend x ty1 $ do
  at <- check t ty2
  return $ ATAbs ty (bind (coerce x) at)
  -- We are trying to check an abstraction under a non-arrow type: error.
check t@(TAbs _) ty = throwError (NotArrow t ty)

  -- To check an injection has a sum type, recursively check the
  -- relevant type.
check (TInj L t) ty@(TySum ty1 _) = do
  at <- check t ty1
  return $ ATInj ty L at
check (TInj R t) ty@(TySum _ ty2) = do
  at <- check t ty2
  return $ ATInj ty R at
  -- Trying to check an injection under a non-sum type: error.
check t@(TInj _ _) ty = throwError (NotSum t ty)

check (TLet l) ty =
  lunbind l $ \(bs, t2) -> do
    (as, ctx) <- inferBindings bs
    extends ctx $ do
      at2 <- check t2 ty
      return $ ATLet ty (bind as at2)

check (TCase bs) ty = do
  bs' <- mapM (checkBranch ty) bs
  return (ATCase ty bs')

-- Need to add new cases due to finite types
check (TBin Add t1 t2) ty =
  if (isNumTy ty)
    then do
      at1 <- check t1 ty
      at2 <- check t2 ty
      return $ ATBin ty Add at1 at2
    else throwError (NotNumTy ty)

-- Checking multiplication is the same as addition
-- (since it is repeated addition)
check (TBin Mul t1 t2) ty =
  if (isNumTy ty)
    then do
      at1 <- check t1 ty
      at2 <- check t2 ty
      return $ ATBin ty Mul at1 at2
    else throwError (NotNumTy ty)

check (TBin Div t1 t2) ty = do
  _ <- checkFractional ty
  at1 <- check t1 ty
  at2 <- check t2 ty
  return $ ATBin ty Div at1 at2

check (TBin IDiv t1 t2) ty = do
  if (isNumTy ty)
    then do
      at1 <- check t1 ty   -- should this be lub of ty and TyQP?
      at2 <- check t2 ty   -- lub of ty and TyQP?
      return $ ATBin ty IDiv at1 at2
    else throwError (NotNumTy ty)

check (TBin Exp t1 t2) ty =
  if (isNumTy ty)
    then do
      -- if a^b :: fractional t, then a :: t, b :: Z
      -- else if a^b :: non-fractional t, then a :: t, b :: N
      at1 <- check t1 ty
      at2 <- check t2 (if isFractional ty then TyZ else TyN)
      return $ ATBin ty Exp at1 at2
    else throwError (NotNumTy ty)

-- XXX comment me: special case for N, Q+ with runtime check
-- XXX should probably make this an opt-in feature
check (TBin Sub t1 t2) ty = do
  when (not (isNumTy ty)) $ throwError (NotNumTy ty)
  -- when (not (isSubtractive ty)) $ do
  --   traceM $ "Warning: checking subtraction at type " ++ show ty
  --   traceShowM $ (TBin Sub t1 t2)
    -- XXX emit warning re: subtraction on N or Q+
  at1 <- check t1 ty
  at2 <- check t2 ty
  return $ ATBin ty Sub at1 at2

-- XXX comment me: not the same special case for Neg as for Sub
check (TUn Neg t) ty = do
  _ <- checkSubtractive ty
  at <- check t ty
  return $ ATUn ty Neg at

check (TNat x) (TyFin n) =
  return $ ATNat (TyFin n) x

  -- Finally, to check anything else, we can infer its type and then
  -- check that the inferred type is a subtype of the given type.
check t ty = do
  at <- infer t
  checkSub at ty

-- | Check the types of terms in a tuple against a nested
--   pair type.
checkTuple :: [Term] -> Type -> TCM [ATerm]
checkTuple [] _   = error "Impossible! checkTuple []"
checkTuple [t] ty = do     -- (:[]) <$> check t ty
  at <- check t ty
  return [at]
checkTuple (t:ts) (TyPair ty1 ty2) = do
  at  <- check t ty1
  ats <- checkTuple ts ty2
  return (at:ats)
checkTuple ts ty = throwError $ NotTuple (TTup ts) ty

checkEllipsis :: Maybe (Ellipsis Term) -> Type -> TCM (Maybe (Ellipsis ATerm))
checkEllipsis Nothing          _  = return Nothing
checkEllipsis (Just Forever)   _  = return (Just Forever)
checkEllipsis (Just (Until t)) ty = (Just . Until) <$> check t ty

-- | Check the type of a branch, returning a type-annotated branch.
checkBranch :: Type -> Branch -> TCM ABranch
checkBranch ty b =
  lunbind b $ \(gs, t) -> do
  (ags, ctx) <- inferTelescope inferGuard gs
  extends ctx $ do
  at <- check t ty
  return $ bind ags at

-- | Check that the given annotated term has a type which is a subtype
--   of the given type.  The returned annotated term may be the same
--   as the input, or it may be wrapped in 'ATSub' if we made a
--   nontrivial use of subtyping.
checkSub :: ATerm -> Type -> TCM ATerm
checkSub at ty = do
  let ty' = getType at
  if (isSub ty' ty)
   then
     if (ty' == ty)
       then return at
       else return (ATSub ty at)
   else
     throwError (Mismatch ty at)

-- | Check whether one type is a subtype of another (we have decidable
--   subtyping).
isSub :: Type -> Type -> Bool
isSub ty1 ty2 | ty1 == ty2 = True
isSub TyVoid _ = True
isSub TyN TyZ  = True
isSub TyN TyQP = True
isSub TyN TyQ  = True
isSub TyZ TyQ  = True
isSub TyQP TyQ = True
isSub (TyArr t1 t2) (TyArr t1' t2')   = isSub t1' t1 && isSub t2 t2'
isSub (TyPair t1 t2) (TyPair t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub (TySum  t1 t2) (TySum  t1' t2') = isSub t1 t1' && isSub t2 t2'
isSub _ _ = False

-- | Compute the least upper bound (least common supertype) of two
--   types.  Return the LUB, or throw an error if there isn't one.
lub :: Type -> Type -> TCM Type
lub ty1 ty2
  | isSub ty1 ty2 = return ty2
  | isSub ty2 ty1 = return ty1
lub TyQP TyZ = return TyQ
lub TyZ TyQP = return TyQ
lub (TyArr t1 t2) (TyArr t1' t2') = do
  requireSameTy t1 t1'
  t2'' <- lub t2 t2'
  return $ TyArr t1 t2''
lub (TyPair t1 t2) (TyPair t1' t2') = do
  t1'' <- lub t1 t1'
  t2'' <- lub t2 t2'
  return $ TyPair t1'' t2''
lub (TySum t1 t2) (TySum t1' t2') = do
  t1'' <- lub t1 t1'
  t2'' <- lub t2 t2'
  return $ TySum t1'' t2''
lub (TyList t1) (TyList t2) = do
  t' <- lub t1 t2
  return $ TyList t'
lub ty1 ty2 = throwError $ NoLub ty1 ty2

-- | Recursively computes the least upper bound of a list of Types.
lubs :: [Type] -> TCM Type
lubs [ty]     = return $ ty
lubs (ty:tys) = do
  lubstys  <- lubs tys
  lubty    <- lub ty lubstys
  return $ lubty
lubs []       = error "Impossible! Called lubs on an empty list"

-- | Convenience function that ensures the given annotated terms have
--   numeric types, AND computes their LUB.
numLub :: ATerm -> ATerm -> TCM Type
numLub at1 at2 = do
  checkNumTy at1
  checkNumTy at2
  lub (getType at1) (getType at2)

-- | Decide whether a type supports division
isFractional :: Type -> Bool
isFractional TyQ        = True
isFractional TyQP       = True
isFractional (TyFin n)  = isPrime n
isFractional _          = False

checkFractional :: Type -> TCM Type
checkFractional ty =
  if (isNumTy ty)
    then case isFractional ty of
      True  -> return ty
      False -> throwError $ NotFractional ty
    else throwError $ NotNumTy ty

-- | Decide whether a type supports subtraction
isSubtractive :: Type -> Bool
isSubtractive TyZ       = True
isSubtractive TyQ       = True
isSubtractive (TyFin _) = True
isSubtractive _         = False

checkSubtractive :: Type -> TCM Type
checkSubtractive ty =
  if (isNumTy ty)
    then case isSubtractive ty of
      True  -> return ty
      False -> throwError $ NotSubtractive ty
  else throwError $ NotNumTy ty

-- | Decide whether a type is finite.
isFinite :: Type -> Bool
isFinite TyVoid     = True
isFinite TyUnit     = True
isFinite TyBool     = True
isFinite (TyFin _)  = True
isFinite (TyPair ty1 ty2) = isFinite ty1 && isFinite ty2
isFinite (TySum ty1 ty2)  = isFinite ty1 && isFinite ty2
isFinite (TyArr ty1 ty2)  = isFinite ty1 && isFinite ty2
isFinite _ = False

-- | Check whether the given type is finite, and throw an error if not.
checkFinite :: Type -> TCM ()
checkFinite ty
  | isFinite ty = return ()
  | otherwise   = throwError $ Infinite ty

-- | Decide whether a type has decidable equality.
isDecidable :: Type -> Bool
isDecidable (TyVar _) = error "isDecidable TyVar"
isDecidable TyVoid    = True
isDecidable TyUnit    = True
isDecidable TyBool    = True
isDecidable TyN       = True
isDecidable TyZ       = True
isDecidable TyQP      = True
isDecidable TyQ       = True
isDecidable (TyFin _) = True
isDecidable (TyPair ty1 ty2) = isDecidable ty1 && isDecidable ty2
isDecidable (TySum  ty1 ty2) = isDecidable ty1 && isDecidable ty2
isDecidable (TyArr  ty1 ty2) = isFinite    ty1 && isDecidable ty2
isDecidable (TyList ty) = isDecidable ty

-- | Check whether the given type has decidable equality, and throw an
--   error if not.
checkDecidable :: Type -> TCM ()
checkDecidable ty
  | isDecidable ty = return ()
  | otherwise      = throwError $ Undecidable ty

-- | Check whether the given type has a total order.
isOrdered :: Type -> Bool
isOrdered (TyVar _) = error "isOrdered TyVar"
isOrdered TyVoid    = True
isOrdered TyUnit    = True
isOrdered TyBool    = True
isOrdered TyN       = True
isOrdered TyZ       = True
isOrdered TyQP      = True
isOrdered TyQ       = True
isOrdered (TyFin _) = True
isOrdered (TyPair ty1 ty2) = isOrdered ty1 && isOrdered ty2
isOrdered (TySum  ty1 ty2) = isOrdered ty1 && isOrdered ty2
isOrdered (TyArr  ty1 ty2) = isFinite ty1 && isOrdered ty1 && isOrdered ty2
isOrdered (TyList ty) = isOrdered ty

-- | Check whether the given type has a total order, and throw an
--   error if not.
checkOrdered :: Type -> TCM ()
checkOrdered ty
  | isOrdered ty = return ()
  | otherwise    = throwError $ Unordered ty

-- | Require two types to be equal.
requireSameTy :: Type -> Type -> TCM ()
requireSameTy ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise  = throwError $ IncompatibleTypes ty1 ty2

-- | Require a term to have a function type, returning the decomposed
--   type if it does, throwing an error if not.
getFunTy :: ATerm -> TCM (Type, Type)
getFunTy (getType -> TyArr ty1 ty2) = return (ty1, ty2)
getFunTy at = throwError (NotFun at)

-- | Check that an annotated term has a numeric type.  Throw an error
--   if not.
checkNumTy :: ATerm -> TCM ()
checkNumTy at =
  if (isNumTy $ getType at)
     then return ()
     else throwError (NotNum at)

-- | Convert a numeric type to its greatest subtype that does not
--   support division.  In particular this is used for the typing rule
--   of the floor and ceiling functions.
integralizeTy :: Type -> Type
integralizeTy TyQ   = TyZ
integralizeTy TyQP  = TyN
integralizeTy t     = t

-- | Convert a numeric type to its greatest subtype that does not
--   support subtraction.  In particular this is used for the typing
--   rule of the absolute value function.
positivizeTy :: Type -> Type
positivizeTy TyZ  = TyN
positivizeTy TyQ  = TyQP
positivizeTy t    = t

-- | Infer the type of a term.  If it succeeds, it returns the term
--   with all subterms annotated.
infer :: Term -> TCM ATerm

infer (TParens t)   = infer t

  -- To infer the type of a variable, just look it up in the context.
infer (TVar x)      = do
  ty <- lookup x
  return $ ATVar ty (coerce x)

  -- A few trivial cases.
infer TUnit         = return ATUnit
infer (TBool b)     = return $ ATBool b
infer (TNat n)      = return $ ATNat TyN n
infer (TRat r)      = return $ ATRat r

  -- Infer the type of a function application by inferring the
  -- function type and then checking the argument type.
infer (TApp t t')   = do
  at <- infer t
  (ty1, ty2) <- getFunTy at
  at' <- check t' ty1
  return $ ATApp ty2 at at'

  -- To infer the type of a pair, just infer the types of both components.
infer (TTup ts) = do
  (ty, ats) <- inferTuple ts
  return $ ATTup ty ats

  -- To infer the type of addition or multiplication, infer the types
  -- of the subterms, check that they are numeric, and return their
  -- lub.
infer (TBin Add t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  return $ ATBin num3 Add at1 at2
infer (TBin Mul t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  return $ ATBin num3 Mul at1 at2

  -- Subtraction is similar, except that we must also lub with Z (a
  -- Nat minus a Nat is not a Nat, it is an Int).
infer (TBin Sub t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  num4 <- lub num3 TyZ <|> checkSubtractive num3
  return $ ATBin num4 Sub at1 at2

  -- Negation is similar to subtraction.
infer (TUn Neg t) = do
  at <- infer t
  checkNumTy at
  let ty = getType at
  num2 <- lub ty TyZ <|> checkSubtractive ty
  return $ ATUn num2 Neg at

infer (TUn Sqrt t) = do
  at <- check t TyN
  return $ ATUn TyN Sqrt at

infer (TUn Lg t)  = do
  at <- check t TyN
  return $ ATUn TyN Lg at

infer (TUn Floor t) = do
  at <- infer t
  checkNumTy at
  let num2 = getType at
  return $ ATUn (integralizeTy num2) Floor at

infer (TUn Ceil t) = do
  at <- infer t
  checkNumTy at
  let num2 = getType at
  return $ ATUn (integralizeTy num2) Ceil at

infer (TUn Abs t) = do
  at <- infer t
  checkNumTy at
  return $ ATUn (positivizeTy (getType at)) Abs at

  -- Division is similar to subtraction; we must take the lub with Q+.
infer (TBin Div t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  num4 <- lub num3 TyQP <|> checkFractional num3
  return $ ATBin num4 Div at1 at2

 -- Very similar to division
infer (TBin IDiv t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  num3 <- numLub at1 at2
  let num4 = integralizeTy num3
  return $ ATBin num4 IDiv at1 at2

infer (TBin Exp t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  checkNumTy at1
  checkNumTy at2
  case getType at2 of

    -- t1^n has the same type as t1 when n : Nat.
    TyN -> return $ ATBin (getType at1) Exp at1 at2

    -- t1^z has type (lub ty1 Q+) when t1 : ty1 and z : Z.
    -- For example, (-3)^(-5) has type Q (= lub Z Q+)
    -- but 3^(-5) has type Q+.
    TyZ -> do
      res <- lub (getType at1) TyQP <|> checkFractional (getType at1)
      return $ ATBin res Exp at1 at2
    TyQ -> throwError ExpQ
    _   -> error "Impossible! getType at2 is not num type after checkNumTy"

  -- An equality or inequality test always has type Bool, but we need
  -- to check a few things first. We infer the types of both subterms
  -- and check that (1) they have a common supertype which (2) has
  -- decidable equality.
infer (TBin eqOp t1 t2) | eqOp `elem` [Eq, Neq] = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkDecidable ty3
  return $ ATBin TyBool eqOp at1 at2

infer (TBin op t1 t2)
  | op `elem` [Lt, Gt, Leq, Geq] = inferComp op t1 t2

  -- &&, ||, and not always have type Bool, and the subterms must have type
  -- Bool as well.
infer (TBin And t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool And at1 at2
infer (TBin Or t1 t2) = do
  at1 <- check t1 TyBool
  at2 <- check t2 TyBool
  return $ ATBin TyBool Or at1 at2
infer (TUn Not t) = do
  at <- check t TyBool
  return $ ATUn TyBool Not at

infer (TBin Mod t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty <- numLub at1 at2
  if (isSub ty TyZ)
    then return (ATBin ty Mod at1 at2)
    else throwError ModQ

infer (TBin Divides t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  _ <- numLub at1 at2
  return (ATBin TyBool Divides at1 at2)

infer (TBin RelPm t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  ty <- numLub at1 at2
  if (isSub ty TyZ)
    then return (ATBin TyBool RelPm at1 at2)
    else throwError RelPmQ

-- For now, a simple typing rule for multinomial coefficients that
-- requires everything to be Nat.  However, they can be extended to
-- handle negative or fractional arguments.
infer (TBin Choose t1 t2) = do
  at1 <- check t1 TyN

  -- t2 can be either a Nat (a binomial coefficient)
  -- or a list of Nat (a multinomial coefficient).
  at2 <- check t2 TyN <|> check t2 (TyList TyN)
  return $ ATBin TyN Choose at1 at2

infer (TBin Cons t1 t2) = do
  at1 <- infer t1
  at2 <- check t2 (TyList (getType at1))
  return $ ATBin (TyList (getType at1)) Cons at1 at2

infer (TUn Fact t) = do
  at <- check t TyN
  return $ ATUn TyN Fact at

infer (TChain t1 links) = do
  at1 <- infer t1
  alinks <- inferChain t1 links
  return $ ATChain TyBool at1 alinks

infer (TList es@(_:_) ell)  = do
  ates <- (mapM infer) es
  aell <- inferEllipsis ell
  let tys = [ getType at | Just (Until at) <- [aell] ] ++ (map getType) ates
  ty  <- lubs tys
  return $ ATList (TyList ty) ates aell

infer (TListComp bqt) = do
  lunbind bqt $ \(qs,t) -> do
  (aqs, cx) <- inferTelescope inferQual qs
  extends cx $ do
  at <- infer t
  let ty = getType at
  return $ ATListComp (TyList ty) (bind aqs at)

infer (TTyOp Enumerate t) = do
  checkFinite t
  return $ ATTyOp (TyList t) Enumerate t

infer (TTyOp Count t) = do
  checkFinite t
  return $ ATTyOp TyN Count t

  -- To infer the type of (let x = t1 in t2), assuming it is
  -- NON-RECURSIVE, infer the type of t1, and then infer the type of
  -- t2 in an extended context.
infer (TLet l) = do
  lunbind l $ \(bs, t2) -> do
  (as, ctx) <- inferBindings bs
  extends ctx $ do
  at2 <- infer t2
  return $ ATLet (getType at2) (bind as at2)

  -- Ascriptions are what let us flip from inference mode into
  -- checking mode.
infer (TAscr t ty) = do
  at <- check t ty
  return $ ATAscr at ty

infer (TCase []) = throwError EmptyCase
infer (TCase bs) = inferCase bs

  -- Catch-all case at the end: if we made it here, we can't infer it.
infer t = throwError (CantInfer t)


inferBindings :: [(Name Term, Embed Term)] -> TCM ([(Name ATerm, Embed ATerm)], Ctx)
inferBindings bs = do
  as <- mapM inferBinding bs
  return ((map . first) coerce as, M.fromList $ map (second (getType . unembed)) as)

inferBinding :: (Name Term, Embed Term) -> TCM (Name Term, Embed ATerm)
inferBinding (x, unembed -> t) = do
  at <- infer t
  return (x, embed at)

-- | Infer the type of a comparison. A comparison always has type
--   Bool, but we have to make sure the subterms are OK. We must check
--   that their types are compatible and have a total order.
inferComp :: BOp -> Term -> Term -> TCM ATerm
inferComp comp t1 t2 = do
  at1 <- infer t1
  at2 <- infer t2
  ty3 <- lub (getType at1) (getType at2)
  checkOrdered ty3
  return $ ATBin TyBool comp at1 at2

inferChain :: Term -> [Link] -> TCM [ALink]
inferChain _  [] = return []
inferChain t1 (TLink op t2 : links) = do
  at2 <- infer t2
  _   <- check (TBin op t1 t2) TyBool
  (ATLink op at2 :) <$> inferChain t2 links

inferEllipsis :: Maybe (Ellipsis Term) -> TCM (Maybe (Ellipsis ATerm))
inferEllipsis (Just (Until t)) = (Just . Until) <$> infer t
inferEllipsis (Just Forever)   = return $ Just Forever
inferEllipsis Nothing          = return Nothing

inferTuple :: [Term] -> TCM (Type, [ATerm])
inferTuple []     = error "Impossible! inferTuple []"
inferTuple [t]    = do
  at <- infer t
  return (getType at, [at])
inferTuple (t:ts) = do
  at <- infer t
  (ty, ats) <- inferTuple ts
  return (TyPair (getType at) ty, at:ats)

-- | Infer the type of a case expression.  The result type is the
--   least upper bound (if it exists) of all the branches.
inferCase :: [Branch] -> TCM ATerm
inferCase bs = do
  bs' <- mapM inferBranch bs
  let (branchTys, abs') = unzip bs'
  resTy <- foldM lub TyVoid branchTys
  return $ ATCase resTy abs'

-- | Infer the type of a case branch, returning the type along with a
--   type-annotated branch.
inferBranch :: Branch -> TCM (Type, ABranch)
inferBranch b =
  lunbind b $ \(gs, t) -> do
  (ags, ctx) <- inferTelescope inferGuard gs
  extends ctx $ do
  at <- infer t
  return $ (getType at, bind ags at)

-- | Infer the type of a telescope, given a way to infer the type of
--   each item along with a context of variables it binds; each such
--   context is then added to the overall context when inferring
--   subsequent items in the telescope.
inferTelescope
  :: (Alpha b, Alpha tyb)
  => (b -> TCM (tyb, Ctx)) -> Telescope b -> TCM (Telescope tyb, Ctx)
inferTelescope inferOne tel = first toTelescope <$> go (fromTelescope tel)
  where
    go []     = return ([], emptyCtx)
    go (b:bs) = do
      (tyb, ctx) <- inferOne b
      extends ctx $ do
      (tybs, ctx') <- go bs
      return (tyb:tybs, ctx `joinCtx` ctx')

-- | Infer the type of a guard, returning the type-annotated guard
--   along with a context of types for any variables bound by the guard.
inferGuard :: Guard -> TCM (AGuard, Ctx)
inferGuard (GBool (unembed -> t)) = do
  at <- check t TyBool
  return (AGBool (embed at), emptyCtx)
inferGuard (GPat (unembed -> t) p) = do
  at <- infer t
  ctx <- checkPattern p (getType at)
  return (AGPat (embed at) p, ctx)

inferQual :: Qual -> TCM (AQual, Ctx)
inferQual (QBind x (unembed -> t))  = do
  at <- infer t
  case getType at of
    TyList ty -> return (AQBind (coerce x) (embed at), singleCtx x ty)
    wrongTy   -> throwError $ NotList t wrongTy
inferQual (QGuard (unembed -> t))   = do
  at <- check t TyBool
  return (AQGuard (embed at), emptyCtx)

-- | Check that a pattern has the given type, and return a context of
--   pattern variables bound in the pattern along with their types.
checkPattern :: Pattern -> Type -> TCM Ctx
checkPattern (PVar x) ty                    = return $ singleCtx x ty
checkPattern PWild    _                     = ok
checkPattern PUnit TyUnit                   = ok
checkPattern (PBool _) TyBool               = ok
checkPattern (PTup ps) ty                   =
  joinCtxs <$> checkTuplePat ps ty
checkPattern (PInj L p) (TySum ty1 _)       = checkPattern p ty1
checkPattern (PInj R p) (TySum _ ty2)       = checkPattern p ty2
checkPattern (PNat _)   ty | isSub TyN ty   = ok
  -- we can match any supertype of TyN against a Nat pattern
checkPattern (PSucc p)  TyN                 = checkPattern p TyN
checkPattern (PCons p1 p2) (TyList ty)      =
  joinCtx <$> checkPattern p1 ty <*> checkPattern p2 (TyList ty)
checkPattern (PList ps) (TyList ty) =
  joinCtxs <$> mapM (flip checkPattern ty) ps

checkPattern p ty = throwError (PatternType p ty)

checkTuplePat :: [Pattern] -> Type -> TCM [Ctx]
checkTuplePat [] _   = error "Impossible! checkTuplePat []"
checkTuplePat [p] ty = do     -- (:[]) <$> check t ty
  ctx <- checkPattern p ty
  return [ctx]
checkTuplePat (p:ps) (TyPair ty1 ty2) = do
  ctx  <- checkPattern p ty1
  ctxs <- checkTuplePat ps ty2
  return (ctx:ctxs)
checkTuplePat ps ty = throwError $ NotTuplePattern (PTup ps) ty

-- | Successfully return the empty context.  A convenience method for
--   checking patterns that bind no variables.
ok :: TCM Ctx
ok = return emptyCtx

-- | Check all the types in a module, returning a context of types for
--   top-level definitions.
checkModule :: Module -> TCM (DocMap, M.Map (Name ATerm) [AProperty], Ctx)
checkModule (Module m docs) = do
  let (defns, typeDecls) = partition isDefn m
  withTypeDecls typeDecls $ do
    mapM_ checkDefn defns
    aprops <- checkPropertyTypes docs
    (docs,aprops,) <$> ask

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
    declCtx = M.fromList (map (\(DType x ty) -> (x,ty)) decls)

-- | Type check a top-level definition. Precondition: only called on
--   'DDefn's.
checkDefn :: Decl -> TCM ()
checkDefn (DDefn x clauses) = do
  ty <- lookup x
  prevDefn <- gets (M.lookup (coerce x))
  case prevDefn of
    Just _ -> throwError (DuplicateDefns x)
    Nothing -> do
      checkNumPats clauses
      aclauses <- mapM (checkClause ty) clauses
      addDefn x aclauses
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

    checkClause ty clause =
      lunbind clause $ \(pats, body) -> do
      at <- go pats ty body
      return $ bind pats at

    go [] ty body = check body ty
    go (p:ps) (TyArr ty1 ty2) body = do
      ctx <- checkPattern p ty1
      extends ctx $ go ps ty2 body
    go _ _ _ = throwError NumPatterns   -- XXX include more info

checkDefn d = error $ "Impossible! checkDefn called on non-Defn: " ++ show d

-- | XXX
checkPropertyTypes :: DocMap -> TCM (M.Map (Name ATerm) [AProperty])
checkPropertyTypes docs
  = (M.fromList . (traverse . _1 %~ coerce))
    <$> ((traverse . _2 . traverse) checkPropertyType) properties
  -- mapM_ (\(n, ps) -> mapM_ checkPropertyType ps) properties
  where
    properties = [ (n, ps) | (n, DocProperties ps) <- concatMap sequence . M.assocs $ docs ]
    checkPropertyType :: Property -> TCM AProperty
    checkPropertyType prop = do
      lunbind prop $ \(binds, t) -> do
      extends (M.fromList binds) $ do
      at <- check t TyBool
      return $ bind (binds & traverse . _1 %~ coerce) at

{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ViewPatterns             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Typecheck the Disco surface language and transform it into a
-- type-annotated AST.
--
-----------------------------------------------------------------------------

module Disco.Typecheck where

import           Control.Arrow                           ((&&&))
import           Control.Lens                            ((%~), (&), _1)
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Bifunctor                          (first, second)
import           Data.Coerce
import           Data.List                               (group, sort)
import qualified Data.Map                                as M
import           Data.Maybe                              (isJust)
import qualified Data.Set                                as S
import           Prelude                                 hiding (lookup)

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Disco.AST.Generic                       (selectSide)
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Context
import           Disco.Module
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Monad
import           Disco.Typecheck.Solve
import           Disco.Types
import           Disco.Types.Rules

------------------------------------------------------------
-- Container utilities
------------------------------------------------------------

containerTy :: Container -> Type -> Type
containerTy c ty = TyCon (containerToCon c) [ty]

containerToCon :: Container -> Con
containerToCon ListContainer = CList
containerToCon BagContainer  = CBag
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
-- Modules
------------------------------------------------------------

-- | Check all the types and extract all relevant info (docs,
--   properties, types) from a module, returning a 'ModuleInfo' record
--   on success.  This function does not handle imports at all; see
--   'recCheckMod'.
checkModule :: Module -> TCM ModuleInfo
checkModule (Module _ _ m docs) = do
  let (typeDecls, defns, tydefs) = partitionDecls m
  tyCtx <- makeTyCtx typeDecls
  tyDefnCtx <- makeTyDefnCtx tydefs
  withTyDefns tyDefnCtx $ extends tyCtx $ do
    checkCyclicTys tydefs
    adefns <- mapM checkDefn defns
    let defnCtx = M.fromList (map (getDefnName &&& id) adefns)
    let dups = filterDups . map getDefnName $ adefns
    case dups of
      (x:_) -> throwError $ DuplicateDefns (coerce x)
      [] -> do
        aprops <- checkProperties docs
        return $ ModuleInfo docs aprops tyCtx tyDefnCtx defnCtx
  where getDefnName :: Defn -> Name ATerm
        getDefnName (Defn n _ _ _) = n

--------------------------------------------------
-- Type definitions

-- | Turn a list of type definitions (which *must* all consist of
--   'DTyDef', /i.e./ @type name = ...@) into a 'TyDefCtx', checking
--   for duplicate names among the definitions and also any type
--   definitions already in the context.
makeTyDefnCtx :: [TypeDefn] -> TCM TyDefCtx
makeTyDefnCtx tydefs = do
  oldTyDefs <- get
  let oldNames = M.keys oldTyDefs
      newNames = map (\(TypeDefn x _) -> x) tydefs
      dups = filterDups $ newNames ++ oldNames
  case dups of
    (x:_) -> throwError (DuplicateTyDefns x)
    []    -> return $ M.fromList (map (\(TypeDefn x ty) -> (x,ty)) tydefs)

-- | Make sure there are no directly cyclic type definitions.
checkCyclicTys :: [TypeDefn] -> TCM ()
checkCyclicTys = mapM_ unwrap
  where
    unwrap :: TypeDefn -> TCM (S.Set String)
    unwrap (TypeDefn x _) = checkCyclicTy (TyDef x) S.empty

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

filterDups :: Ord a => [a] -> [a]
filterDups = map head . filter ((>1) . length) . group . sort
--------------------------------------------------
-- Type declarations

-- | Run a type checking computation in the context of some type
--   declarations. First check that there are no duplicate type
--   declarations; then run the computation in a context extended with
--   the declared types.

makeTyCtx :: [TypeDecl] -> TCM TyCtx
makeTyCtx decls = do
  let dups = filterDups . map (\(TypeDecl x _) -> x) $ decls
  case dups of
    (x:_) -> throwError (DuplicateDecls x)
    []    -> return declCtx
  where
    declCtx = M.fromList $ map (\(TypeDecl x ty) -> (x,ty)) decls

--------------------------------------------------
-- Top-level definitions

-- | Type check a top-level definition.
checkDefn :: TermDefn -> TCM Defn
checkDefn (TermDefn x clauses) = do
  Forall sig <- lookupTy x
  ((acs, ty'), theta) <- solve $ do
    checkNumPats clauses
    (nms, ty) <- unbind sig
    aclauses <- forAll nms $ mapM (checkClause ty) clauses
    return (aclauses, ty)

  let defnTy = substs theta ty'
      (patTys, bodyTy) = decomposeDefnTy (numPats (head clauses)) defnTy
  return $ substs theta (Defn (coerce x) patTys bodyTy acs)
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

--------------------------------------------------
-- Properties

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

-- | Check that a term has the given polymorphic type.
checkSigma :: Term -> Sigma -> TCM ATerm
checkSigma t (Forall sig) = do
  (as, tau) <- unbind sig
  (at, cst) <- withConstraint $ check t tau
  case as of
    [] -> constraint cst
    _  -> constraint $ CAll (bind as cst)
  return at

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

  -- Run inference on the term and try to solve the resulting
  -- constraints.
  (at, theta) <- solve $ infer t
  traceShowM at

      -- Apply the resulting substitution.
  let at' = substs theta at

      -- Find any remaining container variables.
      cvs = containerVars (getType at')

      -- Replace them all with List.
      at'' = substs (zip (S.toList cvs) (repeat (TyAtom (ABase CtrList)))) at'

  -- Finally, quantify over any remaining type variables and return
  -- the term along with the resulting polymorphic type.
  return (at'', closeSigma (getType at''))

--------------------------------------------------
-- The typecheck function
--------------------------------------------------

-- | The main workhorse of the typechecker.  Instead of having two
--   functions, one for inference and one for checking, 'typecheck'
--   takes a 'Mode'.  This cuts down on code duplication in many
--   cases, and allows all the checking and inference code related to
--   a given AST node to be placed together.
typecheck :: Mode -> Term -> TCM ATerm

-- ~~~~ Note [Pattern coverage]
-- In several places we have clauses like
--
--   typecheck Infer (TBin op t1 t2) | op `elem` [ ... ]
--
-- since the typing rules for all the given operators are the same.
-- The only problem is that the pattern coverage checker (sensibly)
-- doesn't look at guards in general, so it thinks that there are TBin
-- cases still uncovered.
--
-- However, we *don't* just want to add a catch-all case at the end,
-- because the coverage checker is super helpful in alerting us when
-- there's a missing typechecking case after modifying the language in
-- some way. The (not ideal) solution for now is to add some
-- additional explicit cases that simply call 'error', which will
-- never be reached but which assure the coverage checker that we have
-- handled those cases.
--
-- The ideal solution would be to use or-patterns, if Haskell had them
-- (see https://github.com/ghc-proposals/ghc-proposals/pull/43).

--------------------------------------------------
-- Defined types

-- To check at a defined type, expand its definition and recurse.
-- This case has to be first, so in all other cases we know the type
-- will not be a TyDef.
typecheck (Check (TyDef tyn)) t = lookupTyDefn tyn >>= check t

--------------------------------------------------
-- Parens

-- Recurse through parens; they are not represented explicitly in the
-- resulting ATerm.
typecheck mode (TParens t) = typecheck mode t

--------------------------------------------------
-- Variables

-- To infer the type of a variable, just look it up in the context.
-- We don't need a checking case; checking the type of a variable will
-- fall through to this case.
typecheck Infer (TVar x) = checkVar `catchError` checkPrim
  where
    checkVar = do
      Forall sig <- lookupTy x
      (_, ty)    <- unbind sig
      return $ ATVar ty (coerce x)

    -- If the variable is not bound, check if it is a primitive name.
    checkPrim (Unbound _) =
      case [ p | PrimInfo p syn True <- primTable, syn == name2String x ] of
        -- If so, infer the type of the prim instead.
        (prim:_) -> typecheck Infer (TPrim prim)
        _        -> throwError (Unbound x)

    -- For any other error, just rethrow.
    checkPrim e = throwError e

--------------------------------------------------
-- Primitives

-- The 'list', 'bag', and 'set' primitives convert containers into
-- other containers.
typecheck Infer (TPrim conv) | conv `elem` [PrimList, PrimBag, PrimSet] = do
  c <- freshAtom   -- make a unification variable for the container type
  a <- freshTy     -- make a unification variable for the element type

  -- converting to a set or bag requires being able to sort the elements
  when (conv /= PrimList) $ constraint $ CQual QCmp a

  return $ ATPrim (TyContainer c a :->: primCtrCon conv a) conv

  where
    primCtrCon PrimList = TyList
    primCtrCon PrimBag  = TyBag
    primCtrCon _        = TySet

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TPrim PrimList) = error "typecheck Infer PrimList should be unreachable"
typecheck Infer (TPrim PrimBag)  = error "typecheck Infer PrimBag should be unreachable"
typecheck Infer (TPrim PrimSet)  = error "typecheck Infer PrimSet should be unreachable"
------------------------------------------------------------

typecheck Infer (TPrim PrimB2C) = do
  a <- freshTy
  return $ ATPrim (TyBag a :->: TySet (TyPair a TyN)) PrimB2C

typecheck Infer (TPrim PrimC2B) = do
  a <- freshTy
  return $ ATPrim (TySet (TyPair a TyN) :->: TyBag a) PrimC2B

-- XXX see https://github.com/disco-lang/disco/issues/160

-- map : (a -> b) -> (c a -> c b)
typecheck Infer (TPrim PrimMap) = do
  c <- freshAtom
  a <- freshTy
  b <- freshTy
  return $ ATPrim ((a :->: b) :->: TyContainer c a :->: TyContainer c b) PrimMap

-- reduce : (a -> a -> a) -> a -> c a -> a
typecheck Infer (TPrim PrimReduce) = do
  c <- freshAtom
  a <- freshTy
  return $ ATPrim ((a :->: a :->: a) :->: a :->: TyContainer c a :->: a) PrimReduce

-- filter : (a -> Bool) -> c a -> c a
typecheck Infer (TPrim PrimFilter) = do
  c <- freshAtom
  a <- freshTy
  return $ ATPrim ((a :->: TyBool) :->: TyContainer c a :->: TyContainer c a) PrimFilter

-- join : c (c a) -> c a
typecheck Infer (TPrim PrimJoin) = do
  c <- freshAtom
  a <- freshTy
  return $ ATPrim (TyContainer c (TyContainer c a) :->: TyContainer c a) PrimJoin

-- merge : (N -> N -> N) -> c a -> c a -> c a
typecheck Infer (TPrim PrimMerge) = do
  c <- freshAtom
  a <- freshTy
  let ca = TyContainer c a
  return $ ATPrim ((TyN :->: TyN :->: TyN) :->: ca :->: ca :->: ca) PrimMerge

-- isPrime : N -> Bool
typecheck Infer (TPrim PrimIsPrime) = return $ ATPrim (TyN :->: TyBool) PrimIsPrime
-- factor : N -> Bag N
typecheck Infer (TPrim PrimFactor)  = return $ ATPrim (TyN :->: TyBag TyN) PrimFactor

-- crash : String -> a
typecheck Infer (TPrim PrimCrash)   = do
  a <- freshTy
  return $ ATPrim (TyList TyC :->: a) PrimCrash

-- Actually 'forever' and 'until' support more types than this, e.g. Q
-- instead of N, but this is good enough.  These cases are here just
-- for completeness---in case someone enables primitives and uses them
-- directly---but typically they are generated only during desugaring
-- of a container with ellipsis, after typechecking, in which case
-- they can be assigned a more appropriate type directly.

-- forever : List N -> List N
typecheck Infer (TPrim PrimForever)
  = return $ ATPrim (TyList TyN :->: TyList TyN) PrimForever

-- until : N -> List N -> List N
typecheck Infer (TPrim PrimUntil)
  = return $ ATPrim (TyN :->: TyList TyN :->: TyList TyN) PrimUntil

--------------------------------------------------
-- Operators

typecheck Infer (TPrim (PrimBOp And))
  = return $ ATPrim (TyBool :->: TyBool :->: TyBool) (PrimBOp And)

typecheck Infer (TPrim (PrimUOp Fact))
  = return $ ATPrim (TyN :->: TyN) (PrimUOp Fact)

typecheck Infer (TPrim (PrimBOp Lt)) = do
  ty <- freshTy
  constraint $ CQual QCmp ty
  return $ ATPrim (ty :->: ty :->: TyBool) (PrimBOp Lt)

--------------------------------------------------
-- Base types

-- A few trivial cases for base types.
typecheck Infer             TUnit        = return ATUnit
typecheck Infer             (TBool b)    = return $ ATBool b
typecheck Infer             (TChar c)    = return $ ATChar c
typecheck Infer             (TString cs) = return $ ATString cs
typecheck (Check (TyFin n)) (TNat x)     = return $ ATNat (TyFin n) x
typecheck Infer             (TNat n)     = return $ ATNat TyN n
typecheck Infer             (TRat r)     = return $ ATRat r

typecheck _                 TWild        = throwError $ NoTWild

--------------------------------------------------
-- Lambdas

-- To check a lambda abstraction:
typecheck (Check checkTy) (TAbs lam) = do
  (args, t) <- unbind lam

  -- First check that the given type is of the form ty1 -> ty2 ->
  -- ... -> resTy, where the types ty1, ty2 ... match up with any
  -- types declared for the arguments to the lambda (e.g.  (x:tyA)
  -- (y:tyB) -> ...).
  (ctx, typedArgs, resTy) <- checkArgs args checkTy (TAbs lam)

  -- Then check the type of the body under a context extended with
  -- types for all the arguments.
  extends ctx $
    ATAbs checkTy <$> (bind (coerce typedArgs) <$> check t resTy)

  where

    -- Given the variables and their optional type annotations in the
    -- head of a lambda (e.g.  @x (y:Z) (f : N -> N) -> ...@), and the
    -- type at which we are checking the lambda, ensure that the type
    -- is of the form @ty1 -> ty2 -> ... -> resTy@, and that there are
    -- enough @ty1@, @ty2@, ... to match all the arguments.  Also
    -- check that each binding with a type annotation matches the
    -- corresponding ty_i component from the checking type: in
    -- particular, the ty_i must be a subtype of the type annotation.
    -- If it succeeds, return a context binding variables to their
    -- types (taken either from their annotation or from the type to
    -- be checked, as appropriate) which we can use to extend when
    -- checking the body, along with the result type of the function.
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
      (ty1, ty2) <- ensureConstr2 CArr ty (Left term)

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

--------------------------------------------------
-- Application

-- Infer the type of a function application by inferring the function
-- type and then checking the argument type.  We don't need a checking
-- case because checking mode doesn't help.
typecheck Infer (TApp t t')   = do
  at <- infer t
  let ty = getType at
  (ty1, ty2) <- ensureConstr2 CArr ty (Left t)
  ATApp ty2 at <$> check t' ty1

--------------------------------------------------
-- Tuples

-- Check/infer the type of a tuple.
typecheck mode1 (TTup tup) = uncurry ATTup <$> typecheckTuple mode1 tup
  where
    typecheckTuple :: Mode -> [Term] -> TCM (Type, [ATerm])
    typecheckTuple _    []     = error "Impossible! typecheckTuple []"
    typecheckTuple mode [t]    = (getType &&& (:[])) <$> typecheck mode t
    typecheckTuple mode (t:ts) = do
      (m,ms)    <- ensureConstrMode2 CPair mode (Left $ TTup (t:ts))
      at        <- typecheck      m  t
      (ty, ats) <- typecheckTuple ms ts
      return $ (TyPair (getType at) ty, at : ats)

--------------------------------------------------
-- Sum types

-- Check/infer the type of an injection into a sum type.
typecheck mode lt@(TInj s t) = do
  (mL, mR) <- ensureConstrMode2 CSum mode (Left lt)
  at <- typecheck (selectSide s mL mR) t
  resTy <- case mode of
    Infer    ->
      selectSide s (TySum <$> pure (getType at) <*> freshTy          )
                   (TySum <$> freshTy           <*> pure (getType at))
    Check ty -> return ty
  return $ ATInj resTy s at

--------------------------------------------------
-- List cons

-- To check a cons, make sure the type is a list type, then
-- check the two arguments appropriately.
typecheck (Check ty) t@(TBin Cons x xs) = do
  tyElt <- ensureConstr1 CList ty (Left t)
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
        -- ...make sure it is a list, and find the lub of the element types.
      ty2 <- ensureConstr1 CList (getType at2) (Left t2)
      eltTy <- lub (getType at1) ty2
      return $ ATBin (TyList eltTy) Cons at1 at2

--------------------------------------------------
-- Binary & unary operators

----------------------------------------
-- Arithmetic

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

  -- Note, it looks like we would want to use 'lub' here, but that's
  -- not quite right because we don't want the *least* upper bound, we
  -- want the least upper bound which satisfies (bopQual op).  For
  -- example, 2 - 3 has type Z even though 2 and 3 are both inferred
  -- to have type N.  In the future we could make a variant of lub
  -- that takes sorts into account, or we could use lub and then
  -- another function to handle the sort, etc.
  constraints $
    [ CSub ty1 tyv
    , CSub ty2 tyv
    , CQual (bopQual op) tyv
    ]
  return $ ATBin tyv op at1 at2

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TBin Add  _ _) = error "typecheck Infer Add should be unreachable"
typecheck Infer (TBin Mul  _ _) = error "typecheck Infer Mul should be unreachable"
typecheck Infer (TBin Sub  _ _) = error "typecheck Infer Sub should be unreachable"
typecheck Infer (TBin Div  _ _) = error "typecheck Infer Div should be unreachable"
typecheck Infer (TBin SSub _ _) = error "typecheck Infer SSub should be unreachable"
------------------------------------------------------------

typecheck (Check ty) (TUn Neg t) = do
  constraint $ CQual QSub ty
  ATUn ty Neg <$> check t ty

typecheck Infer (TUn Neg t) = do
  at <- infer t
  negTy <- cNeg (getType at)
  return $ ATUn negTy Neg at

----------------------------------------
-- sqrt, lg, fact, floor, ceil, abs, idiv

typecheck Infer (TUn op t) | op `elem` [Sqrt, Lg]    = ATUn TyN op <$> check t TyN

typecheck Infer (TUn Fact t) = typecheck Infer $ expandUOp Fact t

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
  tyLub <- lub (getType at1) (getType at2)
  resTy <- cInt tyLub
  return $ ATBin resTy IDiv at1 at2

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TUn Sqrt  _) = error "typecheck Infer Sqrt should be unreachable"
typecheck Infer (TUn Lg    _) = error "typecheck Infer Lg should be unreachable"
typecheck Infer (TUn Floor _) = error "typecheck Infer Floor should be unreachable"
typecheck Infer (TUn Ceil  _) = error "typecheck Infer Ceil should be unreachable"
------------------------------------------------------------

----------------------------------------
-- exp

typecheck (Check ty) (TBin Exp t1 t2) = do
  at1   <- check t1 ty
  at2   <- infer t2
  resTy <- cExp (getType at1) (getType at2)

  constraint $ CSub resTy ty
  return $ ATBin resTy Exp at1 at2

typecheck Infer (TBin Exp t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  resTy <- cExp (getType at1) (getType at2)
  return $ ATBin resTy Exp at1 at2

----------------------------------------
-- Comparisons

-- Infer the type of a comparison. A comparison always has type Bool,
-- but we have to make sure the subterms have compatible types.  We
-- also generate a QCmp qualifier --- even though every type in disco
-- has semi-decidable equality and ordering, we need to know whether
-- e.g. a comparison was done at a certain type, so we can decide
-- whether the type is allowed to be completely polymorphic or not.
typecheck Infer (TBin op t1 t2) | op `elem` [Eq, Neq, Lt, Gt, Leq, Geq] = do
  at1 <- infer t1
  at2 <- infer t2
  ty <- lub (getType at1) (getType at2)
  constraint $ CQual QCmp ty
  return $ ATBin TyBool op at1 at2

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TBin Eq  _ _) = error "typecheck Infer Eq should be unreachable"
typecheck Infer (TBin Neq _ _) = error "typecheck Infer Neq should be unreachable"
typecheck Infer (TBin Lt  _ _) = error "typecheck Infer Lt should be unreachable"
typecheck Infer (TBin Gt  _ _) = error "typecheck Infer Gt should be unreachable"
typecheck Infer (TBin Leq _ _) = error "typecheck Infer Leq should be unreachable"
typecheck Infer (TBin Geq _ _) = error "typecheck Infer Geq should be unreachable"
------------------------------------------------------------

----------------------------------------
-- Comparison chain

typecheck Infer (TChain t ls) =
  ATChain TyBool <$> infer t <*> inferChain t ls

  where
    inferChain :: Term -> [Link] -> TCM [ALink]
    inferChain _  [] = return []
    inferChain t1 (TLink op t2 : links) = do
      at2 <- infer t2
      _   <- check (TBin op t1 t2) TyBool
      atl <- inferChain t2 links
      return $ ATLink op at2 : atl

----------------------------------------
-- Logic

typecheck mode (TBin And t1 t2) = typecheck mode (expandBOp And t1 t2)

-- &&, ||, and not always have type Bool, and the subterms must have type
-- Bool as well.
typecheck Infer (TBin op t1 t2) | op `elem` [Or, Impl] =
  ATBin TyBool op <$> check t1 TyBool <*> check t2 TyBool

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TBin And  _ _) = error "typecheck Infer And should be unreachable"
typecheck Infer (TBin Or   _ _) = error "typecheck Infer Or should be unreachable"
typecheck Infer (TBin Impl _ _) = error "typecheck Infer Impl should be unreachable"
------------------------------------------------------------

typecheck Infer (TUn Not t) = ATUn TyBool Not <$> check t TyBool

----------------------------------------
-- Mod

typecheck Infer (TBin Mod t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyLub <- lub ty1 ty2
  constraint $ CSub tyLub TyZ
  return $ ATBin tyLub Mod at1 at2

----------------------------------------
-- Divisibility

typecheck Infer (TBin Divides t1 t2) = do
  at1 <- infer t1
  at2 <- infer t2
  let ty1 = getType at1
  let ty2 = getType at2
  tyLub <- lub ty1 ty2
  constraint $ CQual QNum tyLub
  return $ ATBin TyBool Divides at1 at2

----------------------------------------
-- Choose

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

----------------------------------------
-- Set & bag operations

typecheck Infer (TUn Size t) = do
  at <- infer t
  _  <- ensureConstr CSet (getType at) (Left t)
  return $ ATUn TyN Size at

typecheck (Check ty) t@(TBin setOp t1 t2)
    | setOp `elem` [Union, Inter, Diff] = do
  tyElt <- ensureConstr1 CSet ty (Left t)
  ATBin ty setOp <$> check t1 (TySet tyElt) <*> check t2 (TySet tyElt)

typecheck Infer (TBin setOp t1 t2)
    | setOp `elem` [Union, Inter, Diff, Subset] = do
  at1 <- infer t1
  at2 <- infer t2
  tyelt <- freshTy
  let ty1 = getType at1
  let ty2 = getType at2
  let ty = case setOp of {Subset -> TyBool; _ -> TySet tyelt}
  constraints [CSub ty1 (TySet tyelt), CSub ty2 (TySet tyelt)]
  return $ ATBin ty setOp at1 at2

-- See Note [Pattern coverage] -----------------------------
typecheck Infer (TBin Union  _ _) = error "typecheck Infer Union should be unreachable"
typecheck Infer (TBin Inter  _ _) = error "typecheck Infer Inter should be unreachable"
typecheck Infer (TBin Diff   _ _) = error "typecheck Infer Diff should be unreachable"
typecheck Infer (TBin Subset _ _) = error "typecheck Infer Subset should be unreachable"
------------------------------------------------------------

typecheck Infer (TBin Rep t1 t2) = do
  at1 <- infer t1
  at2 <- check t2 TyN
  return $ ATBin (TyBag (getType at1)) Rep at1 at2

typecheck Infer (TUn PowerSet t) = do
  at <- infer t
  tyElt <- ensureConstr1 CSet (getType at) (Left t)
  return $ ATUn (TySet (TySet tyElt)) PowerSet at

----------------------------------------
-- Type operations

typecheck Infer (TTyOp Enumerate t) = return $ ATTyOp (TyList t) Enumerate t

typecheck Infer (TTyOp Count t)     = return $ ATTyOp (TySum TyUnit TyN) Count t

--------------------------------------------------
-- Containers

-- Literal containers, including ellipses
typecheck mode t@(TContainer c xs ell)  = do
  eltMode <- ensureConstrMode1 (containerToCon c) mode (Left t)
  axs  <- mapM (typecheck eltMode) xs
  aell <- typecheckEllipsis eltMode ell
  resTy <- case mode of
    Infer -> do
      let tys = [ getType at | Just (Until at) <- [aell] ] ++ (map getType) axs
      tyv  <- freshTy
      constraints $ map (flip CSub tyv) tys
      return $ containerTy c tyv
    Check ty -> return ty
  when (isJust ell) $
    constraint $ CQual QEnum (getEltTy resTy)
  return $ ATContainer resTy c axs aell

  where
    typecheckEllipsis :: Mode -> Maybe (Ellipsis Term) -> TCM (Maybe (Ellipsis ATerm))
    typecheckEllipsis _ Nothing           = return Nothing
    typecheckEllipsis _ (Just Forever)    = return $ Just Forever
    typecheckEllipsis m (Just (Until tm)) = (Just . Until) <$> typecheck m tm

-- Container comprehensions
typecheck mode tcc@(TContainerComp c bqt) = do
  eltMode <- ensureConstrMode1 (containerToCon c) mode (Left tcc)
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
      ty <- ensureConstr1 (containerToCon c) (getType at) (Left t)
      return (AQBind (coerce x) (embed at), singleCtx x (toSigma ty))

    inferQual (QGuard (unembed -> t))   = do
      at <- check t TyBool
      return (AQGuard (embed at), emptyCtx)

--------------------------------------------------
-- Let

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

--------------------------------------------------
-- Case

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
    inferGuard (GLet (Binding mty x (unembed -> t))) = do
      at <- case mty of
        Just (unembed -> ty) -> checkSigma t ty
        Nothing              -> infer t
      return (AGLet (ABinding mty (coerce x) (embed at)), singleCtx x (toSigma (getType at)))

--------------------------------------------------
-- Type ascription

-- Ascriptions are what let us flip from inference mode into
-- checking mode.
typecheck Infer (TAscr t ty) = checkSigma t ty

--------------------------------------------------
-- Inference fallback

-- Finally, to check anything else, we can fall back to inferring its
-- type and then check that the inferred type is a *subtype* of the
-- given type.  We have to be careful to call 'setType' to change the
-- type at the root of the term to the requested type.
typecheck (Check ty) t = do
  at <- infer t
  constraint $ CSub (getType at) ty
  return $ setType ty at

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

-- | Check that a pattern has the given type, and return a context of
--   pattern variables bound in the pattern along with their types.
checkPattern :: Pattern -> Type -> TCM (TyCtx, APattern)

checkPattern p (TyDef tyn) = lookupTyDefn tyn >>= checkPattern p

checkPattern (PVar x) ty = return (singleCtx x (toSigma ty), APVar ty (coerce x))

checkPattern PWild    ty = return (emptyCtx, APWild ty)

checkPattern PUnit ty = do
  ensureEq ty TyUnit
  return (emptyCtx, APUnit)

checkPattern (PBool b) ty = do
  ensureEq ty TyBool
  return (emptyCtx, APBool b)

checkPattern (PChar c) ty = do
  ensureEq ty TyC
  return (emptyCtx, APChar c)

checkPattern (PString s) ty = do
  ensureEq ty (TyList TyC)
  return (emptyCtx, APString s)

checkPattern (PTup tup) tupTy = do
  listCtxtAps <- checkTuplePat tup tupTy
  let (ctxs, aps) = unzip listCtxtAps
  return (joinCtxs ctxs, APTup (foldr1 TyPair (map getType aps)) aps)

  where
    checkTuplePat :: [Pattern] -> Type -> TCM ([(TyCtx, APattern)])
    checkTuplePat [] _   = error "Impossible! checkTuplePat []"
    checkTuplePat [p] ty = do     -- (:[]) <$> check t ty
      (ctx, apt) <- checkPattern p ty
      return [(ctx, apt)]
    checkTuplePat (p:ps) ty = do
      (ty1, ty2) <- ensureConstr2 CPair ty (Right $ PTup (p:ps))
      (ctx, apt) <- checkPattern p ty1
      rest <- checkTuplePat ps ty2
      return ((ctx, apt) : rest)

checkPattern p@(PInj L pat) ty       = do
  (ty1, ty2) <- ensureConstr2 CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty1
  return (ctx, APInj (TySum ty1 ty2) L apt)
checkPattern p@(PInj R pat) ty    = do
  (ty1, ty2) <- ensureConstr2 CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty2
  return (ctx, APInj (TySum ty1 ty2) R apt)

-- we can match any supertype of TyN against a Nat pattern, OR
-- any TyFin.

-- XXX this isn't quite right, what if we're checking at a type
-- variable but we need to solve it to be a TyFin?  Can this ever
-- happen?  We would need a COr, except we can't express the
-- constraint "exists m. ty = TyFin m"
--
-- Yes, this can happen, and here's an example:
--
--   > (\x. {? true when x is 3, false otherwise ?}) (2 : Z5)
--   Unsolvable NoUnify
--   > (\(x : Z5). {? true when x is 3, false otherwise ?}) (2 : Z5)
--   false

checkPattern (PNat n) (TyFin m) = return (emptyCtx, APNat (TyFin m) n)
checkPattern (PNat n) ty        = do
  constraint $ CSub TyN ty
  return (emptyCtx, APNat ty n)

checkPattern p@(PCons p1 p2) ty = do
  tyl <- ensureConstr1 CList ty (Right p)
  (ctx1, ap1) <- checkPattern p1 tyl
  (ctx2, ap2) <- checkPattern p2 (TyList tyl)
  return (joinCtx ctx1 ctx2, APCons (TyList tyl) ap1 ap2)

checkPattern p@(PList ps) ty = do
  tyl <- ensureConstr1 CList ty (Right p)
  listCtxtAps <- mapM (flip checkPattern tyl) ps
  let (ctxs, aps) = unzip listCtxtAps
  return (joinCtxs ctxs, APList (TyList tyl) aps)

checkPattern (PAdd s p t) ty = do
  constraint $ CQual QNum ty
  (ctx, apt) <- checkPattern p ty
  at <- check t ty
  return (ctx, APAdd ty s apt at)

checkPattern (PMul s p t) ty = do
  constraint $ CQual QNum ty
  (ctx, apt) <- checkPattern p ty
  at <- check t ty
  return (ctx, APMul ty s apt at)

checkPattern (PSub p t) ty = do
  constraint $ CQual QNum ty
  (ctx, apt) <- checkPattern p ty
  at <- check t ty
  return (ctx, APSub ty apt at)

checkPattern (PNeg p) ty = do
  constraint $ CQual QSub ty
  tyInner <- cPos ty
  (ctx, apt) <- checkPattern p tyInner
  return (ctx, APNeg ty apt)

checkPattern (PFrac p q) ty = do
  constraint $ CQual QDiv ty
  tyP <- cInt ty
  tyQ <- cPos tyP
  (ctx1, ap1) <- checkPattern p tyP
  (ctx2, ap2) <- checkPattern q tyQ
  return (joinCtx ctx1 ctx2, APFrac ty ap1 ap2)

checkPattern p ty = throwError (PatternType p ty)

------------------------------------------------------------
-- Constraints for abs, floor/ceiling/idiv, and exp
------------------------------------------------------------

-- | Given an input type @ty@, return a type which represents the
--   output type of the absolute value function, and generate
--   appropriate constraints.
cPos :: Type -> TCM Type
cPos ty = do
  constraint $ CQual QNum ty   -- The input type has to be numeric.
  case ty of
    -- If the input type is a concrete base type, we can just
    -- compute the correct output type.
    TyAtom (ABase b) -> return $ TyAtom (ABase (pos b))

    -- Otherwise, generate a fresh type variable for the output type
    -- along with some constraints.
    _ -> do
      res <- freshTy

      -- Valid types for absolute value are Z -> N, Q -> F, or T -> T
      -- (e.g. Z5 -> Z5).
      constraint $ COr
        [ cAnd [CSub ty TyZ, CSub TyN res]
        , cAnd [CSub ty TyQ, CSub TyF res]
        , CEq ty res
        ]
      return res
  where
    pos Z = N
    pos Q = F
    pos t = t

-- | Given an input type @ty@, return a type which represents the
--   output type of the floor or ceiling functions, and generate
--   appropriate constraints.
cInt :: Type -> TCM Type
cInt ty = do
  constraint $ CQual QNum ty
  case ty of
    -- If the input type is a concrete base type, we can just
    -- compute the correct output type.
    TyAtom (ABase b) -> return $ TyAtom (ABase (int b))

    -- Otherwise, generate a fresh type variable for the output type
    -- along with some constraints.
    _ -> do
      res <- freshTy

      -- Valid types for absolute value are F -> N, Q -> Z, or T -> T
      -- (e.g. Z5 -> Z5).
      constraint $ COr
        [ cAnd [CSub ty TyF, CSub TyN res]
        , cAnd [CSub ty TyQ, CSub TyZ res]
        , CEq ty res
        ]
      return res

  where
    int F = N
    int Q = Z
    int t = t

-- | Given input types to the exponentiation operator, return a type
--   which represents the output type, and generate appropriate
--   constraints.
cExp :: Type -> Type -> TCM Type
cExp ty1 TyN = do
  constraint $ CQual QNum ty1
  return ty1

-- We could include a special case for TyZ, but for that we would need
-- a function to find a supertype of a given type that satisfies QDiv.

cExp ty1 ty2 = do

  -- Create a fresh type variable to represent the result type.  The
  -- base type has to be a subtype.
  resTy <- freshTy
  constraint $ CSub ty1 resTy

  -- Either the exponent type is N, in which case the result type has
  -- to support multiplication, or else the exponent is Z, in which
  -- case the result type also has to support division.
  constraint $ COr
    [ cAnd [CQual QNum resTy, CEq ty2 TyN]
    , cAnd [CQual QDiv resTy, CEq ty2 TyZ]
    ]
  return resTy

cNeg :: Type -> TCM Type
cNeg (TyAtom (ABase b)) = (TyAtom . ABase) <$> negBase
  where
    negBase = case b of
      N     -> return Z
      F     -> return Q
      Z     -> return Z
      Q     -> return Q
      Fin n -> return $ Fin n
      _     -> throwError $ NoNeg (TyAtom (ABase b))
cNeg ty = do
  negTy <- freshTy
  constraints $ [CSub ty negTy, CQual QSub negTy]
  return negTy

------------------------------------------------------------
-- Subtyping and least upper bounds
------------------------------------------------------------

-- | Decide whether one type is /known/ to be a subtype of another.
--   Returns False if either one is a type variable (and they are not
--   equal).
isKnownSub :: Type -> Type -> Bool
isKnownSub ty1 ty2 | ty1 == ty2 = True
isKnownSub (TyAtom (ABase b1)) (TyAtom (ABase b2)) = isKnownSubBase b1 b2
isKnownSub (TyCon c1 ts1) (TyCon c2 ts2)
  = c1 == c2 && and (zipWith3 checkKnownSub (arity c1) ts1 ts2)
  where
    checkKnownSub Co t1 t2     = isKnownSub t1 t2
    checkKnownSub Contra t1 t2 = isKnownSub t2 t1
isKnownSub _ _ = False

-- | Check whether one base type is known to be a subtype of another.
isKnownSubBase :: BaseTy -> BaseTy -> Bool
isKnownSubBase b1 b2 = (b1,b2) `elem` basePairs
  where
    basePairs = [ (N,Z), (N,F), (N,Q)
                , (Z,Q), (F,Q)
                ]

-- | Compute the least upper bound of two types.  If they are concrete
--   types, either compute their lub directly or throw an error if
--   they are incompatible.  Otherwise, generate a new type variable
--   and appropriate constraints.
--
--   This saves work in the constraint solver, makes it easier to
--   generate good error messages, and even sometimes saves us the
--   indignity of exponential time complexity in the constraint solver.
lub :: Type -> Type -> TCM Type
lub ty1 ty2
  | isKnownSub ty1 ty2 = return ty2
  | isKnownSub ty2 ty1 = return ty1
lub TyF TyZ = return TyQ
lub TyZ TyF = return TyQ
lub ty1@(TyAtom (ABase _)) ty2@(TyAtom (ABase _)) = throwError $ NoLub ty1 ty2

-- Would make sense eventually to add this case, but we would need a glb function too
-- lub ty1@(TyCon c1 ts) ty2@(TyCon c2 t2)
--   | c1 == c2  = ...  -- need glb here for contravariant arguments
--   | otherwise = throwError $ NoLub ty1 ty2

-- Fallback case: generate a fresh type variable and constrain both input types
-- to be subtypes of it
lub ty1 ty2 = do
  tyLub <- freshTy
  constraints $ [CSub ty1 tyLub, CSub ty2 tyLub]
  return tyLub

------------------------------------------------------------
-- Operator expansion
------------------------------------------------------------

-- Expand operators into applications of primitives right before
-- type checking them.

expandUOp :: UOp -> Term -> Term
expandUOp uop = TApp (TPrim (PrimUOp uop))

expandBOp :: BOp -> Term -> Term -> Term
expandBOp bop = TApp . TApp (TPrim (PrimBOp bop))

------------------------------------------------------------
-- Decomposing type constructors
------------------------------------------------------------

-- | Ensure that a type's outermost constructor matches the provided
--   constructor, returning the types within the matched constructor
--   or throwing a type error.  If the type provided is a type
--   variable, appropriate constraints are generated to guarantee the
--   type variable's outermost constructor matches the provided
--   constructor, and a list of fresh type variables is returned whose
--   count matches the arity of the provided constructor.
ensureConstr :: Con -> Type -> Either Term Pattern -> TCM [Type]
ensureConstr c ty targ = matchConTy c ty
  where
    matchConTy :: Con -> Type -> TCM [Type]
    matchConTy c1 (TyCon c2 tys) = do
      matchCon c1 c2
      return tys

    matchConTy c1 tyv@(TyAtom (AVar (U _))) = do
      tyvs <- mapM (const freshTy) (arity c1)
      constraint $ CEq tyv (TyCon c1 tyvs)
      return tyvs

    matchConTy _ _ = matchError

    -- | Check whether two constructors match, which could include
    --   unifying container variables if we are matching two container
    --   types; otherwise, simply ensure that the constructors are
    --   equal.  Throw a 'matchError' if they do not match.
    matchCon :: Con -> Con -> TCM ()
    matchCon c1 c2                            | c1 == c2 = return ()
    matchCon (CContainer v@(AVar (U _))) (CContainer ctr2) =
      constraint $ CEq (TyAtom v) (TyAtom ctr2)
    matchCon (CContainer ctr1) (CContainer v@(AVar (U _))) =
      constraint $ CEq (TyAtom ctr1) (TyAtom v)
    matchCon _ _                              = matchError

    matchError :: TCM a
    matchError = case targ of
      Left term -> throwError (NotCon c term ty)
      Right pat -> throwError (PatternType pat ty)

-- | A variant of ensureConstr that expects to get exactly one
--   argument type out, and throws an error if we get any other
--   number.
ensureConstr1 :: Con -> Type -> Either Term Pattern -> TCM Type
ensureConstr1 c ty targ = do
  tys <- ensureConstr c ty targ
  case tys of
    [ty1] -> return ty1
    _     -> error $
      "Impossible! Wrong number of arg types in ensureConstr1 " ++ show c ++ " "
        ++ show ty ++ ": " ++ show tys

-- | A variant of ensureConstr that expects to get exactly two
--   argument types out, and throws an error if we get any other
--   number.
ensureConstr2 :: Con -> Type -> Either Term Pattern -> TCM (Type, Type)
ensureConstr2 c ty targ  = do
  tys <- ensureConstr c ty targ
  case tys of
    [ty1, ty2] -> return (ty1, ty2)
    _          -> error $
      "Impossible! Wrong number of arg types in ensureConstr2 " ++ show c ++ " "
        ++ show ty ++ ": " ++ show tys


-- | A variant of 'ensureConstr' that works on 'Mode's instead of
--   'Type's.  Behaves similarly to 'ensureConstr' if the 'Mode' is
--   'Check'; otherwise it generates an appropriate number of copies
--   of 'Infer'.
ensureConstrMode :: Con -> Mode -> Either Term Pattern -> TCM [Mode]
ensureConstrMode c Infer      _  = return $ map (const Infer) (arity c)
ensureConstrMode c (Check ty) tp = map Check <$> ensureConstr c ty tp

-- | A variant of 'ensureConstrMode' that expects to get a single
--   'Mode' and throws an error if it encounters any other number.
ensureConstrMode1 :: Con -> Mode -> Either Term Pattern -> TCM Mode
ensureConstrMode1 c m targ = do
  ms <- ensureConstrMode c m targ
  case ms of
    [m1] -> return m1
    _    -> error $
      "Impossible! Wrong number of arg types in ensureConstrMode1 " ++ show c ++ " "
        ++ show m ++ ": " ++ show ms

-- | A variant of 'ensureConstrMode' that expects to get two 'Mode's
--   and throws an error if it encounters any other number.
ensureConstrMode2 :: Con -> Mode -> Either Term Pattern -> TCM (Mode, Mode)
ensureConstrMode2 c m targ = do
  ms <- ensureConstrMode c m targ
  case ms of
    [m1, m2] -> return (m1, m2)
    _        -> error $
      "Impossible! Wrong number of arg types in ensureConstrMode2 " ++ show c ++ " "
        ++ show m ++ ": " ++ show ms

-- | Ensure that two types are equal:
--     1. Do nothing if they are literally equal
--     2. Generate an equality constraint otherwise
ensureEq :: Type -> Type -> TCM ()
ensureEq ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise  = constraint $ CEq ty1 ty2

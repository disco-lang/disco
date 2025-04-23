{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Disco.Typecheck
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Typecheck the Disco surface language and transform it into a
-- type-annotated AST.
module Disco.Typecheck where

import Control.Arrow ((&&&))
import Control.Lens ((^..))
import Control.Monad (filterM, forM_, replicateM, unless, when, zipWithM)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Coerce
import qualified Data.Foldable as F
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Disco.AST.Surface
import Disco.AST.Typed
import Disco.Context hiding (filter)
import qualified Disco.Context as Ctx
import Disco.Effects.Fresh
import Disco.Messages
import Disco.Module
import Disco.Names
import Disco.Subst (applySubst)
import qualified Disco.Subst as Subst
import Disco.Syntax.Operators
import Disco.Syntax.Prims
import Disco.Typecheck.Constraints
import Disco.Typecheck.Solve (SolutionLimit (..), solveConstraint)
import Disco.Typecheck.Util
import Disco.Types
import Disco.Types.Rules
import Polysemy hiding (embed)
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State (evalState)
import Polysemy.Writer
import Text.EditDistance (defaultEditCosts, restrictedDamerauLevenshteinDistance)
import Unbound.Generics.LocallyNameless (
  Alpha,
  Bind,
  Name,
  bind,
  embed,
  name2String,
  string2Name,
  substs,
  unembed,
 )
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)
import Prelude as P hiding (lookup)

------------------------------------------------------------
-- Container utilities
------------------------------------------------------------

containerTy :: Container -> Type -> Type
containerTy c ty = TyCon (containerToCon c) [ty]

containerToCon :: Container -> Con
containerToCon ListContainer = CList
containerToCon BagContainer = CBag
containerToCon SetContainer = CSet

------------------------------------------------------------
-- Telescopes
------------------------------------------------------------

-- | Infer the type of a telescope, given a way to infer the type of
--   each item along with a context of variables it binds; each such
--   context is then added to the overall context when inferring
--   subsequent items in the telescope.
inferTelescope ::
  (Alpha b, Alpha tyb, Member (Reader TyCtx) r) =>
  (b -> Sem r (tyb, TyCtx)) ->
  Telescope b ->
  Sem r (Telescope tyb, TyCtx)
inferTelescope inferOne tel = do
  (tel1, ctx) <- go (fromTelescope tel)
  return (toTelescope tel1, ctx)
 where
  go [] = return ([], emptyCtx)
  go (b : bs) = do
    (tyb, ctx) <- inferOne b
    extends ctx $ do
      (tybs, ctx') <- go bs
      return (tyb : tybs, ctx <> ctx')

------------------------------------------------------------
-- Variable name utilities
------------------------------------------------------------

suggestionsFrom :: String -> [String] -> [String]
suggestionsFrom x = filter ((<= 1) . restrictedDamerauLevenshteinDistance defaultEditCosts x)

------------------------------------------------------------
-- Modules
------------------------------------------------------------

-- | Check all the types and extract all relevant info (docs,
--   properties, types) from a module, returning a 'ModuleInfo' record
--   on success.  This function does not handle imports at all; any
--   imports should already be checked and passed in as the second
--   argument.
checkModule ::
  Members '[Output (Message ann), Reader TyCtx, Reader TyDefCtx, Error LocTCError, Fresh] r =>
  ModuleName ->
  Map ModuleName ModuleInfo ->
  Module ->
  Sem r ModuleInfo
checkModule name imports (Module es _ m docs terms) = do
  let (typeDecls, defns, tydefs) = partitionDecls m
      importTyCtx = mconcat (imports ^.. traverse . miTys)
      -- XXX this isn't right, if multiple modules define the same type synonyms.
      -- Need to use a normal Ctx for tydefs too.
      importTyDefnCtx = M.unions (imports ^.. traverse . miTydefs)
  tyDefnCtx <- mapError noLoc $ makeTyDefnCtx tydefs
  withTyDefns (tyDefnCtx `M.union` importTyDefnCtx) $ do
    tyCtx <- mapError noLoc $ makeTyCtx name typeDecls
    extends importTyCtx $ extends tyCtx $ do
      mapM_ (checkTyDefn name) tydefs
      adefns <- mapM (checkDefn name) defns
      let defnCtx = ctxForModule name (map (getDefnName &&& id) adefns)
          docCtx = ctxForModule name docs
          dups = filterDups . map getDefnName $ adefns
      case dups of
        (x : _) -> throw $ noLoc $ DuplicateDefns (coerce x)
        [] -> do
          aprops <- mapError noLoc $ checkProperties docCtx -- XXX location?
          aterms <- mapError noLoc $ mapM inferTop1 terms -- XXX location?
          return $ ModuleInfo name imports (map ((name .-) . getDeclName) typeDecls) docCtx aprops tyCtx tyDefnCtx defnCtx aterms es
 where
  getDefnName :: Defn -> Name ATerm
  getDefnName (Defn n _ _ _) = n

  getDeclName :: TypeDecl -> Name Term
  getDeclName (TypeDecl n _) = n

--------------------------------------------------
-- Type definitions

-- | Turn a list of type definitions into a 'TyDefCtx', checking
--   for duplicate names among the definitions and also any type
--   definitions already in the context.
makeTyDefnCtx :: Members '[Reader TyDefCtx, Error TCError] r => [TypeDefn] -> Sem r TyDefCtx
makeTyDefnCtx tydefs = do
  oldTyDefs <- ask @TyDefCtx
  let oldNames = M.keys oldTyDefs
      newNames = map (\(TypeDefn x _ _) -> x) tydefs
      dups = filterDups $ newNames ++ oldNames

  let convert (TypeDefn x args body) =
        (x, TyDefBody args (flip substs body . zip (map string2Name args)))

  case dups of
    (x : _) -> throw (DuplicateTyDefns x)
    [] -> return . M.fromList $ map convert tydefs

-- | Check the validity of a type definition.
checkTyDefn :: Members '[Reader TyDefCtx, Error LocTCError] r => ModuleName -> TypeDefn -> Sem r ()
checkTyDefn name defn@(TypeDefn x args body) = mapError (LocTCError (Just (name .- string2Name x))) $ do
  -- First, make sure the body is a valid type, i.e. everything inside
  -- it is well-kinded.
  checkTypeValid body

  -- Now make sure it is not directly cyclic (i.e. ensure it is a
  -- "productive" definition).
  _ <- checkCyclicTy (TyUser x (map (TyVar . string2Name) args)) S.empty

  -- Make sure it does not use any unbound type variables or undefined
  -- types.
  checkUnboundVars defn

  -- Make sure it does not use any polymorphic recursion (polymorphic
  -- recursion isn't allowed at the moment since it can make the
  -- subtyping checker diverge).
  checkPolyRec defn

-- | Check if a given type is cyclic. A type 'ty' is cyclic if:
--
--   1. 'ty' is the name of a user-defined type.
--   2. Repeated expansions of the type yield nothing but other user-defined types.
--   3. An expansion of one of those types yields another type that has
--      been previously encountered.
--
--   In other words, repeatedly expanding the definition can get us
--   back to exactly where we started.
--
--   The function returns the set of TyDefs encountered during
--   expansion if the TyDef is not cyclic.
checkCyclicTy :: Members '[Reader TyDefCtx, Error TCError] r => Type -> Set String -> Sem r (Set String)
checkCyclicTy (TyUser name args) set = do
  case S.member name set of
    True -> throw $ CyclicTyDef name
    False -> do
      ty <- lookupTyDefn name args
      checkCyclicTy ty (S.insert name set)
checkCyclicTy _ set = return set

-- | Ensure that a type definition does not use any unbound type
--   variables or undefined types.
checkUnboundVars :: Members '[Reader TyDefCtx, Error TCError] r => TypeDefn -> Sem r ()
checkUnboundVars (TypeDefn _ args body) = go body
 where
  go (TyAtom (AVar (U x)))
    | xn `elem` args = return ()
    | otherwise = throw $ UnboundTyVar x suggestions
   where
    xn = name2String x
    suggestions = suggestionsFrom xn args
  go (TyAtom _) = return ()
  go (TyUser name tys) = lookupTyDefn name tys >> mapM_ go tys
  go (TyCon _ tys) = mapM_ go tys

-- | Check for polymorphic recursion: starting from a user-defined
--   type, keep expanding its definition recursively, ensuring that
--   any recursive references to the defined type have only type variables
--   as arguments.
checkPolyRec :: Member (Error TCError) r => TypeDefn -> Sem r ()
checkPolyRec (TypeDefn name args body) = go body
 where
  go (TyCon (CUser x) tys)
    | x == name && not (all isTyVar tys) =
        throw $ NoPolyRec name args tys
    | otherwise = return ()
  go (TyCon _ tys) = mapM_ go tys
  go _ = return ()

-- | Keep only the duplicate elements from a list.
--
--   >>> filterDups [1,3,2,1,1,4,2]
--   [1,2]
filterDups :: Ord a => [a] -> [a]
filterDups = map NE.head . filter ((> 1) . NE.length) . NE.group . sort

--------------------------------------------------
-- Type declarations

-- | Given a list of type declarations from a module, first check that
--   there are no duplicate type declarations, and that the types are
--   well-formed; then create a type context containing the given
--   declarations.
makeTyCtx :: Members '[Reader TyDefCtx, Error TCError] r => ModuleName -> [TypeDecl] -> Sem r TyCtx
makeTyCtx name decls = do
  let dups = filterDups . map (\(TypeDecl x _) -> x) $ decls
  case dups of
    (x : _) -> throw (DuplicateDecls x)
    [] -> do
      checkCtx declCtx
      return declCtx
 where
  declCtx = ctxForModule name $ map (\(TypeDecl x ty) -> (x, ty)) decls

-- | Check that all the types in a context are valid.
checkCtx :: Members '[Reader TyDefCtx, Error TCError] r => TyCtx -> Sem r ()
checkCtx = mapM_ checkPolyTyValid . Ctx.elems

--------------------------------------------------
-- Top-level definitions

-- | Type check a top-level definition in the given module.
checkDefn ::
  Members '[Reader TyCtx, Reader TyDefCtx, Error LocTCError, Fresh, Output (Message ann)] r =>
  ModuleName ->
  TermDefn ->
  Sem r Defn
checkDefn name (TermDefn x clauses) = mapError (LocTCError (Just (name .- x))) $ do
  debug "======================================================================"
  debug "Checking definition:"
  -- Check that all clauses have the same number of patterns
  checkNumPats clauses

  -- Get the declared type signature of x
  Forall sig <- lookup (name .- x) >>= maybe (throw $ NoType x) return
  -- If x isn't in the context, it's because no type was declared for it, so
  -- throw an error.
  (nms, ty) <- unbind sig

  -- Try to decompose the type into a chain of arrows like pty1 ->
  -- pty2 -> pty3 -> ... -> bodyTy, according to the number of
  -- patterns, and lazily unrolling type definitions along the way.
  (patTys, bodyTy) <- decomposeDefnTy (numPats (NE.head clauses)) ty

  ((acs, _), thetas) <- solve 1 $ do
    aclauses <- forAll nms $ mapM (checkClause patTys bodyTy) clauses
    return (aclauses, ty)

  return $ applySubst (NE.head thetas) (Defn (coerce x) patTys bodyTy acs)
 where
  numPats = length . fst . unsafeUnbind

  checkNumPats (_ :| []) = return ()
  checkNumPats (c :| cs)
    | all ((== 0) . numPats) (c : cs) = throw (DuplicateDefns x)
    | not (all ((== numPats c) . numPats) cs) = throw NumPatterns
    -- XXX more info, this error actually means # of
    -- patterns don't match across different clauses
    | otherwise = return ()

  -- Check a clause of a definition against a list of pattern types and a body type.
  checkClause ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    [Type] ->
    Type ->
    Bind [Pattern] Term ->
    Sem r Clause
  checkClause patTys bodyTy clause = do
    (pats, body) <- unbind clause

    -- At this point we know that every clause has the same number of patterns,
    -- which is the same as the length of the list patTys.  So we can just use
    -- zipWithM to check all the patterns.
    (ctxs, aps) <- unzip <$> zipWithM checkPattern pats patTys
    at <- extends (mconcat ctxs) $ check body bodyTy
    return $ bind aps at

  -- Decompose a type that must be of the form t1 -> t2 -> ... -> tn -> t{n+1}.
  decomposeDefnTy :: Members '[Reader TyDefCtx, Error TCError] r => Int -> Type -> Sem r ([Type], Type)
  decomposeDefnTy 0 ty = return ([], ty)
  decomposeDefnTy n (TyUser tyName args) = lookupTyDefn tyName args >>= decomposeDefnTy n
  decomposeDefnTy n (ty1 :->: ty2) = first (ty1 :) <$> decomposeDefnTy (n - 1) ty2
  decomposeDefnTy _n _ty = throw NumPatterns

-- XXX include more info. More argument patterns than arrows in the type.

--------------------------------------------------
-- Properties

-- | Given a context mapping names to documentation, extract the
--   properties attached to each name and typecheck them.
checkProperties ::
  Members '[Reader TyCtx, Reader TyDefCtx, Error TCError, Fresh, Output (Message ann)] r =>
  Ctx Term Docs ->
  Sem r (Ctx ATerm [AProperty])
checkProperties docs =
  Ctx.coerceKeys . Ctx.filter (not . P.null)
    <$> (traverse . traverse) checkProperty properties
 where
  properties :: Ctx Term [Property]
  properties = fmap (\ds -> [p | DocProperty p <- ds]) docs

-- | Check the types of the terms embedded in a property.
checkProperty ::
  Members '[Reader TyCtx, Reader TyDefCtx, Error TCError, Fresh, Output (Message ann)] r =>
  Property ->
  Sem r AProperty
checkProperty prop = do
  debug "======================================================================"
  debug "Checking property:"
  debugPretty prop
  (at, thetas) <- solve 1 $ check prop TyProp
  -- XXX do we need to default container variables here?
  return $ applySubst (NE.head thetas) at

------------------------------------------------------------
-- Type checking/inference
------------------------------------------------------------

--------------------------------------------------
-- Checking types/kinds
--------------------------------------------------

-- | Check that a sigma type is a valid type.  See 'checkTypeValid'.
checkPolyTyValid :: Members '[Reader TyDefCtx, Error TCError] r => PolyType -> Sem r ()
checkPolyTyValid (Forall b) = do
  let (_, ty) = unsafeUnbind b
  checkTypeValid ty

-- | Disco doesn't need kinds per se, since all types must be fully
--   applied.  But we do need to check that every type is applied to
--   the correct number of arguments.
checkTypeValid :: Members '[Reader TyDefCtx, Error TCError] r => Type -> Sem r ()
checkTypeValid (TyAtom _) = return ()
checkTypeValid (TyCon c tys) = do
  k <- conArity c
  if
    | n < k -> throw (NotEnoughArgs c)
    | n > k -> throw (TooManyArgs c)
    | otherwise -> mapM_ checkTypeValid tys
 where
  n = length tys

conArity :: Members '[Reader TyDefCtx, Error TCError] r => Con -> Sem r Int
conArity (CContainer _) = return 1
conArity CGraph = return 1
conArity (CUser name) = do
  d <- ask @TyDefCtx
  case M.lookup name d of
    Nothing -> throw (NotTyDef name)
    Just (TyDefBody as _) -> return (length as)
conArity _ = return 2 -- (->, *, +, map)

--------------------------------------------------
-- Checking modes
--------------------------------------------------

-- | Typechecking can be in one of two modes: inference mode means we
--   are trying to synthesize a valid type for a term; checking mode
--   means we are trying to show that a term has a given type.
data Mode = Infer | Check Type
  deriving (Show)

-- | Check that a term has the given type.  Either throws an error, or
--   returns the term annotated with types for all subterms.
--
--   This function is provided for convenience; it simply calls
--   'typecheck' with an appropriate 'Mode'.
check ::
  Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Term ->
  Type ->
  Sem r ATerm
check t ty = typecheck (Check ty) t

-- | Check that a term has the given polymorphic type.
checkPolyTy ::
  Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Term ->
  PolyType ->
  Sem r ATerm
checkPolyTy t (Forall sig) = do
  (as, tau) <- unbind sig
  (at, cst) <- withConstraint $ check t tau
  case as of
    [] -> constraint cst
    _ -> constraint $ CAll (bind as cst)
  return at

-- | Infer the type of a term.  If it succeeds, it returns the term
--   with all subterms annotated.
--
--   This function is provided for convenience; it simply calls
--   'typecheck' with an appropriate 'Mode'.
infer ::
  Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Term ->
  Sem r ATerm
infer = typecheck Infer

-- | Top-level type inference algorithm, returning only the first
--   possible result.
inferTop1 ::
  Members '[Output (Message ann), Reader TyCtx, Reader TyDefCtx, Error TCError, Fresh] r =>
  Term ->
  Sem r (ATerm, PolyType)
inferTop1 t = NE.head <$> inferTop 1 t

-- | Top-level type inference algorithm: infer up to the requested max
--   number of possible (polymorphic) types for a term by running type
--   inference, solving the resulting constraints, and quantifying
--   over any remaining type variables.
inferTop ::
  Members '[Output (Message ann), Reader TyCtx, Reader TyDefCtx, Error TCError, Fresh] r =>
  Int ->
  Term ->
  Sem r (NonEmpty (ATerm, PolyType))
inferTop lim t = do
  -- Run inference on the term and try to solve the resulting
  -- constraints.
  debug "======================================================================"
  debug "Inferring the type of:"
  debugPretty t
  (at, thetas) <- solve lim $ infer t

  debug "Final annotated term (before substitution and container monomorphizing):"
  debugPretty at

  -- Currently the following code generates *every possible*
  -- combination of substitutions for container variables, which can
  -- lead to exponential blowup in some cases.  For example, inferring
  -- the type of
  --
  --   \x. \y. \z. (set(x), set(y), set(z))
  --
  -- takes a Very Long Time.  Potential solutions include:
  --
  --   1. Do something similar as in the 'solve' function, using a State SolutionLimit
  --      effect to stop early once we've generated enough variety.
  --
  --   2. Use a proper backtracking search monad like LogicT to scope
  --      over both the generation of solution substitutions *and*
  --      choosing container variable monomorphizations, then just
  --      take a limited number of solutions.  Unfortunately,
  --      polysemy's NonDet effect seems to be somewhat broken
  --      (https://stackoverflow.com/questions/62627695/running-the-nondet-effect-once-in-polysemy
  --      ; https://github.com/polysemy-research/polysemy/issues/246 )
  --      and using LogicT on top of Sem is going to be tedious since
  --      it would require calling 'lift' on almost everything.
  --
  --   3. Also, it is probably (?) the case that no matter which of
  --      the generated substitutions is used, the exact same
  --      container variables are still unconstrained in all of them.
  --      So we should be able to pick container variable
  --      monomorphizations independently of the substitutions from
  --      the solver.  Doing this would help though it would not
  --      address the fundamental issue.

  -- Quantify over any remaining type variables and return
  -- the term along with the resulting polymorphic type.
  return $ do
    -- Monad NonEmpty

    -- Iterate over all possible solutions...
    theta <- thetas

    let -- Apply each one...
        at' = applySubst theta at

        -- Find any remaining container variables...
        cvs = containerVars (getType at')

    -- Build all possible substitutions for those container variables...
    ctrs <- replicateM (S.size cvs) (NE.map (TyAtom . ABase) (CtrList :| [CtrBag, CtrSet]))

    -- Substitute for the container variables...
    let at'' = applySubst (Subst.fromList $ zip (S.toList cvs) ctrs) at'

    -- Return the term along with its type, with all substitutions applied.
    return (at'', closeType (getType at''))

-- | Top-level type checking algorithm: check that a term has a given
--   polymorphic type by running type checking and solving the
--   resulting constraints.
checkTop ::
  Members '[Output (Message ann), Reader TyCtx, Reader TyDefCtx, Error TCError, Fresh] r =>
  Term ->
  PolyType ->
  Sem r ATerm
checkTop t ty = do
  debug "======================================================================"
  debug "Checking the type of:"
  debugPretty t
  debugPretty ty
  (at, theta) <- solve 1 $ checkPolyTy t ty
  return $ applySubst (NE.head theta) at

--------------------------------------------------
-- The typecheck function
--------------------------------------------------

-- | The main workhorse of the typechecker.  Instead of having two
--   functions, one for inference and one for checking, 'typecheck'
--   takes a 'Mode'.  This cuts down on code duplication in many
--   cases, and allows all the checking and inference code related to
--   a given AST node to be placed together.
typecheck ::
  Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Mode ->
  Term ->
  Sem r ATerm
-- ~~~~ Note [Pattern coverage]
-- In several places we have clauses like
--
--   inferPrim (PrimBOp op) | op `elem` [And, Or, Impl, Iff]
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

-- To check at a user-defined type, expand its definition and recurse.
-- This case has to be first, so in all other cases we know the type
-- will not be a TyUser.
typecheck (Check (TyUser name args)) t = lookupTyDefn name args >>= check t
--------------------------------------------------
-- Parens

-- Recurse through parens; they are not represented explicitly in the
-- resulting ATerm.
typecheck mode (TParens t) = typecheck mode t
--------------------------------------------------
-- Variables

-- Resolve variable names and infer their types.  We don't need a
-- checking case; checking the type of a variable will fall through to
-- this case.
typecheck Infer (TVar x) = do
  -- Pick the first method that succeeds; if none do, throw an unbound
  -- variable error.
  mt <- runMaybeT . F.asum . map MaybeT $ [tryLocal, tryModule, tryPrim]
  ctx <- ask @TyCtx
  let inScope = map name2String (Ctx.names ctx) ++ opNames ++ [syn | PrimInfo _ syn _ <- M.elems primMap]
      suggestions = suggestionsFrom (name2String x) inScope
  maybe (throw $ Unbound x suggestions) return mt
 where
  -- 1. See if the variable name is bound locally.
  tryLocal = do
    mty <- Ctx.lookup (localName x)
    case mty of
      Just (Forall sig) -> do
        (_, ty) <- unbind sig
        return . Just $ ATVar ty (localName (coerce x))
      Nothing -> return Nothing

  -- 2. See if the variable name is bound in some in-scope module,
  -- throwing an ambiguity error if it is bound in multiple modules.
  tryModule = do
    bs <- Ctx.lookupNonLocal x
    case bs of
      [(m, Forall sig)] -> do
        (_, ty) <- unbind sig
        return . Just $ ATVar ty (m .- coerce x)
      [] -> return Nothing
      _nonEmpty -> throw $ Ambiguous x (map fst bs)

  -- 3. See if we should convert it to a primitive.
  tryPrim =
    case toPrim (name2String x) of
      (prim : _) -> Just <$> typecheck Infer (TPrim prim)
      [] -> return Nothing

--------------------------------------------------
-- Primitives

typecheck Infer (TPrim prim) = do
  ty <- inferPrim prim
  return $ ATPrim ty prim
 where
  inferPrim :: Members '[Writer Constraint, Fresh] r => Prim -> Sem r Type

  ----------------------------------------
  -- Left/right

  inferPrim PrimLeft = do
    a <- freshTy
    b <- freshTy
    return $ a :->: (a :+: b)
  inferPrim PrimRight = do
    a <- freshTy
    b <- freshTy
    return $ b :->: (a :+: b)

  ----------------------------------------
  -- Logic

  inferPrim (PrimBOp op) | op `elem` [And, Or, Impl, Iff] = do
    a <- freshTy
    constraint $ CQual (bopQual op) a
    return $ a :*: a :->: a

  -- See Note [Pattern coverage] -----------------------------
  inferPrim (PrimBOp And) = error "inferPrim And should be unreachable"
  inferPrim (PrimBOp Or) = error "inferPrim Or should be unreachable"
  inferPrim (PrimBOp Impl) = error "inferPrim Impl should be unreachable"
  inferPrim (PrimBOp Iff) = error "inferPrim Iff should be unreachable"
  ------------------------------------------------------------

  inferPrim (PrimUOp Not) = do
    a <- freshTy
    constraint $ CQual QBool a
    return $ a :->: a

  ----------------------------------------
  -- Container conversion

  inferPrim conv | conv `elem` [PrimList, PrimBag, PrimSet] = do
    c <- freshAtom -- make a unification variable for the container type
    a <- freshTy -- make a unification variable for the element type

    -- converting to a set or bag requires being able to sort the elements
    when (conv /= PrimList) $ constraint $ CQual QCmp a

    return $ TyContainer c a :->: primCtrCon conv a
   where
    primCtrCon PrimList = TyList
    primCtrCon PrimBag = TyBag
    primCtrCon _ = TySet

  -- See Note [Pattern coverage] -----------------------------
  inferPrim PrimList = error "inferPrim PrimList should be unreachable"
  inferPrim PrimBag = error "inferPrim PrimBag should be unreachable"
  inferPrim PrimSet = error "inferPrim PrimSet should be unreachable"
  ------------------------------------------------------------

  inferPrim PrimB2C = do
    a <- freshTy
    return $ TyBag a :->: TySet (a :*: TyN)
  inferPrim PrimC2B = do
    a <- freshTy
    c <- freshAtom
    constraint $ CQual QCmp a
    return $ TyContainer c (a :*: TyN) :->: TyBag a
  inferPrim PrimUC2B = do
    a <- freshTy
    c <- freshAtom
    return $ TyContainer c (a :*: TyN) :->: TyBag a
  inferPrim PrimMapToSet = do
    k <- freshTy
    v <- freshTy
    constraint $ CQual QSimple k
    return $ TyMap k v :->: TySet (k :*: v)
  inferPrim PrimSetToMap = do
    k <- freshTy
    v <- freshTy
    constraint $ CQual QSimple k
    return $ TySet (k :*: v) :->: TyMap k v
  inferPrim PrimSummary = do
    a <- freshTy
    constraint $ CQual QSimple a
    return $ TyGraph a :->: TyMap a (TySet a)
  inferPrim PrimVertex = do
    a <- freshTy
    constraint $ CQual QSimple a
    return $ a :->: TyGraph a
  inferPrim PrimEmptyGraph = do
    a <- freshTy
    constraint $ CQual QSimple a
    return $ TyGraph a
  inferPrim PrimOverlay = do
    a <- freshTy
    constraint $ CQual QSimple a
    return $ TyGraph a :*: TyGraph a :->: TyGraph a
  inferPrim PrimConnect = do
    a <- freshTy
    constraint $ CQual QSimple a
    return $ TyGraph a :*: TyGraph a :->: TyGraph a
  inferPrim PrimInsert = do
    a <- freshTy
    b <- freshTy
    constraint $ CQual QSimple a
    return $ a :*: b :*: TyMap a b :->: TyMap a b
  inferPrim PrimLookup = do
    a <- freshTy
    b <- freshTy
    constraint $ CQual QSimple a
    return $ a :*: TyMap a b :->: (TyUnit :+: b)
  ----------------------------------------
  -- Container primitives

  inferPrim (PrimBOp Cons) = do
    a <- freshTy
    return $ a :*: TyList a :->: TyList a

  -- XXX see https://github.com/disco-lang/disco/issues/160
  -- each : (a -> b) × c a -> c b
  inferPrim PrimEach = do
    c <- freshAtom
    a <- freshTy
    b <- freshTy
    return $ (a :->: b) :*: TyContainer c a :->: TyContainer c b

  -- XXX should eventually be (a * a -> a) * c a -> a,
  --   with a check that the function has the right properties.
  -- reduce : (a * a -> a) * a * c a -> a
  inferPrim PrimReduce = do
    c <- freshAtom
    a <- freshTy
    return $ (a :*: a :->: a) :*: a :*: TyContainer c a :->: a

  -- filter : (a -> Bool) × c a -> c a
  inferPrim PrimFilter = do
    c <- freshAtom
    a <- freshTy
    return $ (a :->: TyBool) :*: TyContainer c a :->: TyContainer c a

  -- join : c (c a) -> c a
  inferPrim PrimJoin = do
    c <- freshAtom
    a <- freshTy
    return $ TyContainer c (TyContainer c a) :->: TyContainer c a

  -- merge : (N × N -> N) × c a × c a -> c a   (c = bag or set)
  inferPrim PrimMerge = do
    c <- freshAtom
    a <- freshTy
    constraint $
      cOr
        [ CEq (TyAtom (ABase CtrBag)) (TyAtom c)
        , CEq (TyAtom (ABase CtrSet)) (TyAtom c)
        ]
    let ca = TyContainer c a
    return $ (TyN :*: TyN :->: TyN) :*: ca :*: ca :->: ca
  inferPrim (PrimBOp CartProd) = do
    a <- freshTy
    b <- freshTy
    c <- freshAtom
    return $ TyContainer c a :*: TyContainer c b :->: TyContainer c (a :*: b)
  inferPrim (PrimBOp setOp) | setOp `elem` [Union, Inter, Diff, Subset] = do
    a <- freshTy
    c <- freshAtom
    constraint $
      cOr
        [ CEq (TyAtom (ABase CtrBag)) (TyAtom c)
        , CEq (TyAtom (ABase CtrSet)) (TyAtom c)
        ]
    let ca = TyContainer c a
    let resTy = case setOp of Subset -> TyBool; _ -> ca
    return $ ca :*: ca :->: resTy

  -- See Note [Pattern coverage] -----------------------------
  inferPrim (PrimBOp Union) = error "inferPrim Union should be unreachable"
  inferPrim (PrimBOp Inter) = error "inferPrim Inter should be unreachable"
  inferPrim (PrimBOp Diff) = error "inferPrim Diff should be unreachable"
  inferPrim (PrimBOp Subset) = error "inferPrim Subset should be unreachable"
  ------------------------------------------------------------

  inferPrim (PrimBOp Elem) = do
    a <- freshTy
    c <- freshAtom

    constraint $ CQual QCmp a

    return $ a :*: TyContainer c a :->: TyBool

  ----------------------------------------
  -- Randomness

  inferPrim PrimRandom = return $ (TyN :*: TyN) :*: TyGen :->: (TyN :*: TyGen)
  inferPrim PrimSeed = return $ TyN :->: TyGen
  ----------------------------------------
  -- Arithmetic

  inferPrim (PrimBOp IDiv) = do
    a <- freshTy
    resTy <- cInt a
    return $ a :*: a :->: resTy
  inferPrim (PrimBOp Mod) = do
    a <- freshTy
    constraint $ CSub a TyZ
    return $ a :*: a :->: a
  inferPrim (PrimBOp op) | op `elem` [Add, Mul, Sub, Div] = do
    a <- freshTy
    constraint $ CQual (bopQual op) a
    return $ a :*: a :->: a
  inferPrim (PrimBOp SSub) = do
    argTy <- freshTy
    resTy <- freshTy
    cAbs argTy resTy
    return $ argTy :*: argTy :->: resTy

  -- See Note [Pattern coverage] -----------------------------
  inferPrim (PrimBOp Add) = error "inferPrim Add should be unreachable"
  inferPrim (PrimBOp Mul) = error "inferPrim Mul should be unreachable"
  inferPrim (PrimBOp Sub) = error "inferPrim Sub should be unreachable"
  inferPrim (PrimBOp Div) = error "inferPrim Div should be unreachable"
  inferPrim (PrimBOp SSub) = error "inferPrim SSub should be unreachable"
  ------------------------------------------------------------

  inferPrim (PrimUOp Neg) = do
    a <- freshTy
    constraint $ CQual QSub a
    return $ a :->: a
  inferPrim (PrimBOp Exp) = do
    a <- freshTy
    b <- freshTy
    resTy <- cExp a b
    return $ a :*: b :->: resTy

  ----------------------------------------
  -- Number theory

  inferPrim PrimIsPrime = return $ TyN :->: TyBool
  inferPrim PrimFactor = return $ TyN :->: TyBag TyN
  inferPrim PrimFrac = return $ TyQ :->: (TyZ :*: TyN)
  inferPrim (PrimBOp Divides) = do
    a <- freshTy
    constraint $ CQual QNum a
    return $ a :*: a :->: TyBool

  ----------------------------------------
  -- Choose

  -- For now, a simple typing rule for multinomial coefficients that
  -- requires everything to be Nat.  However, they can be extended to
  -- handle negative or fractional arguments.
  inferPrim (PrimBOp Choose) = do
    b <- freshTy

    -- b can be either Nat (a binomial coefficient)
    -- or a list of Nat (a multinomial coefficient).
    constraint $ cOr [CEq b TyN, CEq b (TyList TyN)]
    return $ TyN :*: b :->: TyN

  ----------------------------------------
  -- Ellipses

  -- Actually 'until' supports more types than this, e.g. Q instead
  -- of N, but this is good enough.  This case is here just for
  -- completeness---in case someone enables primitives and uses it
  -- directly---but typically 'until' is generated only during
  -- desugaring of a container with ellipsis, after typechecking, in
  -- which case it can be assigned a more appropriate type directly.

  inferPrim PrimUntil = return $ TyN :*: TyList TyN :->: TyList TyN
  ----------------------------------------
  -- Crash

  inferPrim PrimCrash = do
    a <- freshTy
    return $ TyString :->: a

  ----------------------------------------
  -- Propositions

  -- 'holds' converts a Prop into a Bool (but might not terminate).
  inferPrim PrimHolds = return $ TyProp :->: TyBool
  -- An binary assertion is just like a comparison, except
  -- the result is a Prop.
  inferPrim (PrimBOp (Should _)) = do
    ty <- freshTy
    constraint $ CQual QCmp ty
    return $ ty :*: ty :->: TyProp

  ----------------------------------------
  -- Comparisons

  -- Infer the type of a comparison. A comparison always has type
  -- Bool, but we have to make sure the subterms have compatible
  -- types.  We also generate a QCmp qualifier, for two reasons:
  -- one, we need to know whether e.g. a comparison was done at a
  -- certain type, so we can decide whether the type is allowed to
  -- be completely polymorphic or not.  Also, comparison of Props is
  -- not allowed.
  inferPrim (PrimBOp op) | op `elem` [Eq, Neq, Lt, Gt, Leq, Geq] = do
    ty <- freshTy
    constraint $ CQual QCmp ty
    return $ ty :*: ty :->: TyBool

  -- See Note [Pattern coverage] -----------------------------
  inferPrim (PrimBOp Eq) = error "inferPrim Eq should be unreachable"
  inferPrim (PrimBOp Neq) = error "inferPrim Neq should be unreachable"
  inferPrim (PrimBOp Lt) = error "inferPrim Lt should be unreachable"
  inferPrim (PrimBOp Gt) = error "inferPrim Gt should be unreachable"
  inferPrim (PrimBOp Leq) = error "inferPrim Leq should be unreachable"
  inferPrim (PrimBOp Geq) = error "inferPrim Geq should be unreachable"
  ------------------------------------------------------------

  ----------------------------------------
  -- Special arithmetic functions: fact, sqrt, floor, ceil, abs

  inferPrim (PrimUOp Fact) = return $ TyN :->: TyN
  inferPrim PrimSqrt = return $ TyN :->: TyN
  inferPrim p | p `elem` [PrimFloor, PrimCeil] = do
    argTy <- freshTy
    resTy <- cInt argTy
    return $ argTy :->: resTy

  -- See Note [Pattern coverage] -----------------------------
  inferPrim PrimFloor = error "inferPrim Floor should be unreachable"
  inferPrim PrimCeil = error "inferPrim Ceil should be unreachable"
  ------------------------------------------------------------

  inferPrim PrimAbs = do
    argTy <- freshTy
    resTy <- freshTy
    cAbs argTy resTy `orElse` cSize argTy resTy
    return $ argTy :->: resTy

  ----------------------------------------
  -- min/max

  inferPrim PrimMin = do
    a <- freshTy
    constraint $ CQual QCmp a
    return $ (a :*: a) :->: a
  inferPrim PrimMax = do
    a <- freshTy
    constraint $ CQual QCmp a
    return $ (a :*: a) :->: a

  ----------------------------------------
  -- power set/bag

  inferPrim PrimPower = do
    a <- freshTy
    c <- freshAtom

    constraint $ CQual QCmp a
    constraint $
      cOr
        [ CEq (TyAtom (ABase CtrSet)) (TyAtom c)
        , CEq (TyAtom (ABase CtrBag)) (TyAtom c)
        ]

    return $ TyContainer c a :->: TyContainer c (TyContainer c a)
  inferPrim PrimLookupSeq = return $ TyList TyN :->: (TyUnit :+: TyString)
  inferPrim PrimExtendSeq = return $ TyList TyN :->: TyList TyN

--------------------------------------------------
-- Base types

-- A few trivial cases for base types.
typecheck Infer TUnit = return ATUnit
typecheck Infer (TBool b) = return $ ATBool TyBool b
typecheck Infer (TChar c) = return $ ATChar c
typecheck Infer (TString cs) = return $ ATString cs
-- typecheck (Check (TyFin n)) (TNat x)     = return $ ATNat (TyFin n) x
typecheck Infer (TNat n) = return $ ATNat TyN n
typecheck Infer (TRat r) = return $ ATRat r
typecheck _ TWild = throw NoTWild
--------------------------------------------------
-- Abstractions (lambdas and quantifiers)

-- Lambdas and quantifiers are similar enough that we can share a
-- bunch of the code, but their typing rules are a bit different.  In
-- particular a lambda
--
--   \(x1:ty1), (x2:ty2) ... . body
--
-- is going to have a type like ty1 -> ty2 -> ... -> resTy, whereas a
-- quantifier like
--
--   ∃(x1:ty1), (x2:ty2) ... . body
--
-- is just going to have the type Prop.  The similarity is that in
-- both cases we have to generate unification variables for any
-- binders with omitted type annotations, and typecheck the body under
-- an extended context.

-- It's only helpful to do lambdas in checking mode, since the
-- provided function type can provide information about the types of
-- the arguments.  For other quantifiers we can just fall back to
-- inference mode.
typecheck (Check checkTy) tm@(TAbs Lam body) = do
  (args, t) <- unbind body

  -- First check that the given type is of the form ty1 -> ty2 ->
  -- ... -> resTy, where the types ty1, ty2 ... match up with any
  -- types declared for the arguments to the lambda (e.g.  (x:tyA)
  -- (y:tyB) -> ...).
  (ctx, typedArgs, resTy) <- checkArgs args checkTy tm

  -- Then check the type of the body under a context extended with
  -- types for all the arguments.
  extends ctx $
    ATAbs Lam checkTy . bind (coerce typedArgs) <$> check t resTy
 where
  -- Given the patterns and their optional type annotations in the
  -- head of a lambda (e.g.  @x (y:Z) (f : N -> N) -> ...@), and the
  -- type at which we are checking the lambda, ensure that:
  --
  --   - The type is of the form @ty1 -> ty2 -> ... -> resTy@ and
  --     there are enough @ty1@, @ty2@, ... to match all the arguments.
  --   - Each pattern successfully checks at its corresponding type.
  --
  -- If it succeeds, return a context binding variables to their
  -- types (as determined by the patterns and the input types) which
  -- we can use to extend when checking the body, a list of the typed
  -- patterns, and the result type of the function.
  checkArgs ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    [Pattern] ->
    Type ->
    Term ->
    Sem r (TyCtx, [APattern], Type)

  -- If we're all out of arguments, the remaining checking type is the
  -- result, and there are no variables to bind in the context.
  checkArgs [] ty _ = return (emptyCtx, [], ty)
  -- Take the next pattern and its annotation; the checking type must
  -- be a function type ty1 -> ty2.
  checkArgs (p : args) ty term = do
    -- Ensure that ty is a function type
    (ty1, ty2) <- ensureConstr2 CArr ty (Left term)

    -- Check the argument pattern against the function domain.
    (pCtx, pTyped) <- checkPattern p ty1

    -- Check the rest of the arguments under the type ty2, returning a
    -- context with the rest of the arguments and the final result type.
    (ctx, typedArgs, resTy) <- checkArgs args ty2 term

    -- Pass the result type through, and put the pattern-bound variables
    -- in the returned context.
    return (pCtx <> ctx, pTyped : typedArgs, resTy)

-- In inference mode, we handle lambdas as well as quantifiers (∀, ∃).
typecheck Infer (TAbs q lam) = do
  -- Open it and get the argument patterns with any type annotations.
  (args, t) <- unbind lam

  -- Replace any missing type annotations with fresh type variables,
  -- and check each pattern at that variable to refine them, collecting
  -- the types of each pattern's bound variables in a context.
  tys <- mapM getAscrOrFresh args
  (pCtxs, typedPats) <- unzip <$> zipWithM checkPattern args tys

  -- In the case of ∀, ∃, have to ensure that the argument types are
  -- searchable.
  when (q `elem` [All, Ex]) $
    -- What's the difference between this and `tys`? Nothing, after
    -- the solver runs, but right now the patterns might have a
    -- concrete type from annotations inside tuples.
    forM_ (map getType typedPats) $ \ty ->
      unless (isSearchable ty) $
        throw $
          NoSearch ty

  -- Extend the context with the given arguments, and then do
  -- something appropriate depending on the quantifier.
  extends (mconcat pCtxs) $ do
    case q of
      -- For lambdas, infer the type of the body, and return an appropriate
      -- function type.
      Lam -> do
        at <- infer t
        return $ ATAbs Lam (mkFunTy tys (getType at)) (bind typedPats at)

      -- For other quantifiers, check that the body has type Prop,
      -- and return Prop.
      _ -> do
        -- ∀, ∃
        at <- check t TyProp
        return $ ATAbs q TyProp (bind typedPats at)
 where
  getAscrOrFresh ::
    Members '[Reader TyDefCtx, Error TCError, Fresh] r =>
    Pattern ->
    Sem r Type
  getAscrOrFresh (PAscr _ ty) = checkTypeValid ty >> pure ty
  getAscrOrFresh _ = freshTy

  -- mkFunTy [ty1, ..., tyn] out = ty1 -> (ty2 -> ... (tyn -> out))
  mkFunTy :: [Type] -> Type -> Type
  mkFunTy tys out = foldr (:->:) out tys

--------------------------------------------------
-- Application

-- Infer the type of a function application by inferring the function
-- type and then checking the argument type.  We don't need a checking
-- case because checking mode doesn't help.
typecheck Infer (TApp t t') = do
  at <- infer t
  let ty = getType at
  (ty1, ty2) <- ensureConstr2 CArr ty (Left t)
  ATApp ty2 at <$> check t' ty1

--------------------------------------------------
-- Tuples

-- Check/infer the type of a tuple.
typecheck mode1 (TTup tup) = uncurry ATTup <$> typecheckTuple mode1 tup
 where
  typecheckTuple ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Mode ->
    [Term] ->
    Sem r (Type, [ATerm])
  typecheckTuple _ [] = error "Impossible! typecheckTuple []"
  typecheckTuple mode [t] = (getType &&& (: [])) <$> typecheck mode t
  typecheckTuple mode (t : ts) = do
    (m, ms) <- ensureConstrMode2 CProd mode (Left $ TTup (t : ts))
    at <- typecheck m t
    (ty, ats) <- typecheckTuple ms ts
    return (getType at :*: ty, at : ats)

----------------------------------------
-- Comparison chain

typecheck Infer (TChain t ls) =
  ATChain TyBool <$> infer t <*> inferChain t ls
 where
  inferChain ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Term ->
    [Link] ->
    Sem r [ALink]
  inferChain _ [] = return []
  inferChain t1 (TLink op t2 : links) = do
    at2 <- infer t2
    _ <- check (TBin op t1 t2) TyBool
    atl <- inferChain t2 links
    return $ ATLink op at2 : atl

----------------------------------------
-- Type operations

typecheck Infer (TTyOp Enumerate t) = do
  checkTypeValid t
  return $ ATTyOp (TyList t) Enumerate t
typecheck Infer (TTyOp Count t) = do
  checkTypeValid t
  return $ ATTyOp (TyUnit :+: TyN) Count t

--------------------------------------------------
-- Containers

-- Literal containers, including ellipses
typecheck mode t@(TContainer c xs ell) = do
  eltMode <- ensureConstrMode1 (containerToCon c) mode (Left t)
  axns <- mapM (\(x, n) -> (,) <$> typecheck eltMode x <*> traverse (`check` TyN) n) xs
  aell <- typecheckEllipsis eltMode ell
  resTy <- case mode of
    Infer -> do
      let tys = [getType at | Just (Until at) <- [aell]] ++ map (getType . fst) axns
      tyv <- freshTy
      constraints $ map (`CSub` tyv) tys
      return $ containerTy c tyv
    Check ty -> return ty
  eltTy <- getEltTy c resTy

  -- See Note [Container literal constraints]
  when (c /= ListContainer && not (P.null xs)) $ constraint $ CQual QCmp eltTy
  when (isJust ell) $ constraint $ CQual QEnum eltTy
  return $ ATContainer resTy c axns aell
 where
  typecheckEllipsis ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Mode ->
    Maybe (Ellipsis Term) ->
    Sem r (Maybe (Ellipsis ATerm))
  typecheckEllipsis _ Nothing = return Nothing
  typecheckEllipsis m (Just (Until tm)) = Just . Until <$> typecheck m tm

-- ~~~~ Note [Container literal constraints]
--
-- It should only be possible to construct something of type Set(a) or
-- Bag(a) when a is comparable, so we can normalize the set or bag
-- value.  For example, Set(N) is OK, but Set(N -> N) is not.  On the
-- other hand, List(a) is fine for any type a.  We want to maintain
-- the invariant that we can only actually obtain a value of type
-- Set(a) or Bag(a) if a is comparable.  This means we will be able to
-- write polymorphic functions that take bags or sets as input without
-- having to specify any constraints --- the only way to call such
-- functions is with element types that actually support comparison.
-- For example, 'unions' can simply have the type Set(Set(a)) ->
-- Set(a).
--
-- Hence, container literals (along with the 'set' and 'bag'
-- conversion functions) serve as "gatekeepers" to make sure we can
-- only construct containers with appropriate element types.  So when
-- we see a container literal, if it is a bag or set literal, we have
-- to introduce an additional QCmp constraint for the element type.
--
-- But not so fast --- with that rule, 'unions' does not type check!
-- To see why, look at the definition:
--
--   unions(ss) = foldr(~∪~, {}, list(ss))
--
-- The empty set literal in the definition means we end up generating
-- a QCmp constraint on the element type anyway.  But there is a
-- solution: we refine our invariant to say that we can only
-- actually obtain a *non-empty* value of type Set(a) or Bag(a) if a
-- is comparable.  Empty bags and sets are allowed to have any element
-- type.  This is safe because there is no way to generate a non-empty
-- set from an empty one, without also making use of something like a
-- non-empty set literal or conversion function.  So we add a special
-- case to the rule that says we only add a QCmp constraint in the
-- case of a *non-empty* set or bag literal.  Now the definition of
-- 'unions' type checks perfectly well.

-- Container comprehensions
typecheck mode tcc@(TContainerComp c bqt) = do
  eltMode <- ensureConstrMode1 (containerToCon c) mode (Left tcc)
  (qs, t) <- unbind bqt
  (aqs, cx) <- inferTelescope inferQual qs
  extends cx $ do
    at <- typecheck eltMode t
    let resTy = case mode of
          Infer -> containerTy c (getType at)
          Check ty -> ty
    return $ ATContainerComp resTy c (bind aqs at)
 where
  inferQual ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Qual ->
    Sem r (AQual, TyCtx)
  inferQual (QBind x (unembed -> t)) = do
    at <- infer t
    ty <- ensureConstr1 (containerToCon c) (getType at) (Left t)
    return (AQBind (coerce x) (embed at), singleCtx (localName x) (toPolyType ty))
  inferQual (QGuard (unembed -> t)) = do
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
  inferBinding ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Binding ->
    Sem r (ABinding, TyCtx)
  inferBinding (Binding mty x (unembed -> t)) = do
    at <- case mty of
      Just (unembed -> ty) -> checkPolyTy t ty
      Nothing -> infer t
    return (ABinding mty (coerce x) (embed at), singleCtx (localName x) (toPolyType $ getType at))

--------------------------------------------------
-- Case

-- Check/infer a case expression.
typecheck _ (TCase []) = throw EmptyCase
typecheck mode (TCase bs) = do
  bs' <- mapM typecheckBranch bs
  resTy <- case mode of
    Check ty -> return ty
    Infer -> do
      x <- freshTy
      constraints $ map ((`CSub` x) . getType) bs'
      return x
  return $ ATCase resTy bs'
 where
  typecheckBranch ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Branch ->
    Sem r ABranch
  typecheckBranch b = do
    (gs, t) <- unbind b
    (ags, ctx) <- inferTelescope inferGuard gs
    extends ctx $
      bind ags <$> typecheck mode t

  -- Infer the type of a guard, returning the type-annotated guard
  -- along with a context of types for any variables bound by the
  -- guard.
  inferGuard ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    Guard ->
    Sem r (AGuard, TyCtx)
  inferGuard (GBool (unembed -> t)) = do
    at <- check t TyBool
    return (AGBool (embed at), emptyCtx)
  inferGuard (GPat (unembed -> t) p) = do
    at <- infer t
    (ctx, apt) <- checkPattern p (getType at)
    return (AGPat (embed at) apt, ctx)
  inferGuard (GLet (Binding mty x (unembed -> t))) = do
    at <- case mty of
      Just (unembed -> ty) -> checkPolyTy t ty
      Nothing -> infer t
    return
      ( AGLet (ABinding mty (coerce x) (embed at))
      , singleCtx (localName x) (toPolyType (getType at))
      )

--------------------------------------------------
-- Type ascription

-- Ascriptions are what let us flip from inference mode into
-- checking mode.
typecheck Infer (TAscr t ty) = checkPolyTyValid ty >> checkPolyTy t ty
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
checkPattern ::
  Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Pattern ->
  Type ->
  Sem r (TyCtx, APattern)
checkPattern (PNonlinear p x) _ = throw $ NonlinearPattern p x
checkPattern p (TyUser name args) = lookupTyDefn name args >>= checkPattern p
checkPattern (PVar x) ty = return (singleCtx (localName x) (toPolyType ty), APVar ty (coerce x))
checkPattern PWild ty = return (emptyCtx, APWild ty)
checkPattern (PAscr p ty1) ty2 = do
  -- We have a pattern that promises to match ty1 and someone is asking
  -- us if it can also match ty2. So we just have to ensure what we're
  -- being asked for is a subtype of what we can promise to cover...
  constraint $ CSub ty2 ty1
  -- ... and then make sure the pattern can actually match what it promised to.
  checkPattern p ty1
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
  ensureEq ty TyString
  return (emptyCtx, APString s)
checkPattern (PTup tup) tupTy = do
  listCtxtAps <- checkTuplePat tup tupTy
  let (ctxs, aps) = unzip listCtxtAps
  return (mconcat ctxs, APTup (foldr1 (:*:) (map getType aps)) aps)
 where
  checkTuplePat ::
    Members '[Reader TyCtx, Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
    [Pattern] ->
    Type ->
    Sem r [(TyCtx, APattern)]
  checkTuplePat [] _ = error "Impossible! checkTuplePat []"
  checkTuplePat [p] ty = do
    -- (:[]) <$> check t ty
    (ctx, apt) <- checkPattern p ty
    return [(ctx, apt)]
  checkTuplePat (p : ps) ty = do
    (ty1, ty2) <- ensureConstr2 CProd ty (Right $ PTup (p : ps))
    (ctx, apt) <- checkPattern p ty1
    rest <- checkTuplePat ps ty2
    return ((ctx, apt) : rest)
checkPattern p@(PInj L pat) ty = do
  (ty1, ty2) <- ensureConstr2 CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty1
  return (ctx, APInj (ty1 :+: ty2) L apt)
checkPattern p@(PInj R pat) ty = do
  (ty1, ty2) <- ensureConstr2 CSum ty (Right p)
  (ctx, apt) <- checkPattern pat ty2
  return (ctx, APInj (ty1 :+: ty2) R apt)

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

-- checkPattern (PNat n) (TyFin m) = return (emptyCtx, APNat (TyFin m) n)
checkPattern (PNat n) ty = do
  constraint $ CSub TyN ty
  return (emptyCtx, APNat ty n)
checkPattern p@(PCons p1 p2) ty = do
  tyl <- ensureConstr1 CList ty (Right p)
  (ctx1, ap1) <- checkPattern p1 tyl
  (ctx2, ap2) <- checkPattern p2 (TyList tyl)
  return (ctx1 <> ctx2, APCons (TyList tyl) ap1 ap2)
checkPattern p@(PList ps) ty = do
  tyl <- ensureConstr1 CList ty (Right p)
  listCtxtAps <- mapM (`checkPattern` tyl) ps
  let (ctxs, aps) = unzip listCtxtAps
  return (mconcat ctxs, APList (TyList tyl) aps)
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
  return (ctx1 <> ctx2, APFrac ty ap1 ap2)

------------------------------------------------------------
-- Constraints for abs, floor/ceiling/idiv, and exp
------------------------------------------------------------

-- | Constraints needed on a function type for it to be the type of
--   the absolute value function.
cAbs :: Members '[Writer Constraint, Fresh] r => Type -> Type -> Sem r ()
cAbs argTy resTy = do
  resTy' <- cPos argTy
  constraint $ CEq resTy resTy'

-- | Constraints needed on a function type for it to be the type of
--   the container size operation.
cSize :: Members '[Writer Constraint, Fresh] r => Type -> Type -> Sem r ()
cSize argTy resTy = do
  a <- freshTy
  c <- freshAtom
  constraint $ CEq (TyContainer c a) argTy
  constraint $ CEq TyN resTy

-- | Given an input type @ty@, return a type which represents the
--   output type of the absolute value function, and generate
--   appropriate constraints.
cPos :: Members '[Writer Constraint, Fresh] r => Type -> Sem r Type
cPos ty = do
  constraint $ CQual QNum ty -- The input type has to be numeric.
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
      constraint $
        cOr
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
cInt :: Members '[Writer Constraint, Fresh] r => Type -> Sem r Type
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
      constraint $
        cOr
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
cExp :: Members '[Writer Constraint, Fresh] r => Type -> Type -> Sem r Type
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
  constraint $
    cOr
      [ cAnd [CQual QNum resTy, CEq ty2 TyN]
      , cAnd [CQual QDiv resTy, CEq ty2 TyZ]
      ]
  return resTy

------------------------------------------------------------
-- Decomposing type constructors
------------------------------------------------------------

-- | Get the argument (element) type of a (known) container type.  Returns a
--   fresh variable with a suitable constraint if the given type is
--   not literally a container type.
getEltTy :: Members '[Writer Constraint, Fresh] r => Container -> Type -> Sem r Type
getEltTy _ (TyContainer _ e) = return e
getEltTy c ty = do
  eltTy <- freshTy
  constraint $ CEq (containerTy c eltTy) ty
  return eltTy

-- | Ensure that a type's outermost constructor matches the provided
--   constructor, returning the types within the matched constructor
--   or throwing a type error.  If the type provided is a type
--   variable, appropriate constraints are generated to guarantee the
--   type variable's outermost constructor matches the provided
--   constructor, and a list of fresh type variables is returned whose
--   count matches the arity of the provided constructor.
ensureConstr ::
  forall r.
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Type ->
  Either Term Pattern ->
  Sem r [Type]
ensureConstr c ty targ = matchConTy c ty
 where
  matchConTy :: Con -> Type -> Sem r [Type]

  -- expand type definitions lazily
  matchConTy c1 (TyUser name args) = lookupTyDefn name args >>= matchConTy c1
  matchConTy c1 (TyCon c2 tys) = do
    matchCon c1 c2
    return tys
  matchConTy c1 tyv@(TyAtom (AVar (U _))) = do
    tyvs <- mapM (const freshTy) (arity c1)
    constraint $ CEq tyv (TyCon c1 tyvs)
    return tyvs
  matchConTy _ _ = matchError

  -- \| Check whether two constructors match, which could include
  --   unifying container variables if we are matching two container
  --   types; otherwise, simply ensure that the constructors are
  --   equal.  Throw a 'matchError' if they do not match.
  matchCon :: Con -> Con -> Sem r ()
  matchCon c1 c2 | c1 == c2 = return ()
  matchCon (CContainer v@(AVar (U _))) (CContainer ctr2) =
    constraint $ CEq (TyAtom v) (TyAtom ctr2)
  matchCon (CContainer ctr1) (CContainer v@(AVar (U _))) =
    constraint $ CEq (TyAtom ctr1) (TyAtom v)
  matchCon _ _ = matchError

  matchError :: Sem r a
  matchError = case targ of
    Left term -> throw (NotCon c term ty)
    Right pat -> throw (PatternType c pat ty)

-- | A variant of ensureConstr that expects to get exactly one
--   argument type out, and throws an error if we get any other
--   number.
ensureConstr1 ::
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Type ->
  Either Term Pattern ->
  Sem r Type
ensureConstr1 c ty targ = do
  tys <- ensureConstr c ty targ
  case tys of
    [ty1] -> return ty1
    _ ->
      error $
        "Impossible! Wrong number of arg types in ensureConstr1 "
          ++ show c
          ++ " "
          ++ show ty
          ++ ": "
          ++ show tys

-- | A variant of ensureConstr that expects to get exactly two
--   argument types out, and throws an error if we get any other
--   number.
ensureConstr2 ::
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Type ->
  Either Term Pattern ->
  Sem r (Type, Type)
ensureConstr2 c ty targ = do
  tys <- ensureConstr c ty targ
  case tys of
    [ty1, ty2] -> return (ty1, ty2)
    _ ->
      error $
        "Impossible! Wrong number of arg types in ensureConstr2 "
          ++ show c
          ++ " "
          ++ show ty
          ++ ": "
          ++ show tys

-- | A variant of 'ensureConstr' that works on 'Mode's instead of
--   'Type's.  Behaves similarly to 'ensureConstr' if the 'Mode' is
--   'Check'; otherwise it generates an appropriate number of copies
--   of 'Infer'.
ensureConstrMode ::
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Mode ->
  Either Term Pattern ->
  Sem r [Mode]
ensureConstrMode c Infer _ = return $ map (const Infer) (arity c)
ensureConstrMode c (Check ty) tp = map Check <$> ensureConstr c ty tp

-- | A variant of 'ensureConstrMode' that expects to get a single
--   'Mode' and throws an error if it encounters any other number.
ensureConstrMode1 ::
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Mode ->
  Either Term Pattern ->
  Sem r Mode
ensureConstrMode1 c m targ = do
  ms <- ensureConstrMode c m targ
  case ms of
    [m1] -> return m1
    _ ->
      error $
        "Impossible! Wrong number of arg types in ensureConstrMode1 "
          ++ show c
          ++ " "
          ++ show m
          ++ ": "
          ++ show ms

-- | A variant of 'ensureConstrMode' that expects to get two 'Mode's
--   and throws an error if it encounters any other number.
ensureConstrMode2 ::
  Members '[Reader TyDefCtx, Writer Constraint, Error TCError, Fresh] r =>
  Con ->
  Mode ->
  Either Term Pattern ->
  Sem r (Mode, Mode)
ensureConstrMode2 c m targ = do
  ms <- ensureConstrMode c m targ
  case ms of
    [m1, m2] -> return (m1, m2)
    _ ->
      error $
        "Impossible! Wrong number of arg types in ensureConstrMode2 "
          ++ show c
          ++ " "
          ++ show m
          ++ ": "
          ++ show ms

-- | Ensure that two types are equal:
--     1. Do nothing if they are literally equal
--     2. Generate an equality constraint otherwise
ensureEq :: Member (Writer Constraint) r => Type -> Type -> Sem r ()
ensureEq ty1 ty2
  | ty1 == ty2 = return ()
  | otherwise = constraint $ CEq ty1 ty2

------------------------------------------------------------
-- Subtyping
------------------------------------------------------------

isSubPolyType ::
  Members '[Input TyDefCtx, Output (Message ann), Fresh] r =>
  PolyType ->
  PolyType ->
  Sem r Bool
isSubPolyType (Forall b1) (Forall b2) = do
  (as1, ty1) <- unbind b1
  (as2, ty2) <- unbind b2
  let c = CAll (bind as1 (CSub ty1 (substs (zip as2 (map TyVar as1)) ty2)))
  debug "======================================================================"
  debug "Checking subtyping..."
  debugPretty (Forall b1)
  debugPretty (Forall b2)
  ss <- runError (evalState (SolutionLimit 1) (solveConstraint c))
  return (either (const False) (not . P.null) ss)

thin :: Members '[Input TyDefCtx, Output (Message ann), Fresh] r => NonEmpty PolyType -> Sem r (NonEmpty PolyType)
thin = fmap NE.fromList . thin' . NE.toList

-- Intuitively, this will always return a nonempty list given a nonempty list as input;
-- we could probably rewrite it in terms of NonEmpty combinators but it's more effort than
-- I cared to spend at the moment.
thin' :: Members '[Input TyDefCtx, Output (Message ann), Fresh] r => [PolyType] -> Sem r [PolyType]
thin' [] = return []
thin' (ty : tys) = do
  ss <- or <$> mapM (`isSubPolyType` ty) tys
  if ss
    then thin' tys
    else do
      tys' <- filterM (fmap not . (ty `isSubPolyType`)) tys
      (ty :) <$> thin' tys'

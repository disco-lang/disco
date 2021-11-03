{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Solve
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Constraint solver for the constraints generated during type
-- checking/inference.
-----------------------------------------------------------------------------

module Disco.Typecheck.Solve where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Coerce
import           GHC.Generics                     (Generic)

import           Control.Arrow                    ((&&&), (***))
import           Control.Lens
import           Data.Bifunctor                   (first, second)
import           Data.Either                      (partitionEithers)
import           Data.List                        (find, foldl', intersect,
                                                   partition)
import           Data.Map                         (Map, (!))
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe,
                                                   mapMaybe)
import           Data.Monoid                      (First (..))
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Tuple

import           Disco.Subst
import qualified Disco.Subst                      as Subst
import           Disco.Typecheck.Constraints
import           Disco.Typecheck.Graph            (Graph)
import qualified Disco.Typecheck.Graph            as G
import           Disco.Typecheck.Unify
import           Disco.Types
import           Disco.Types.Qualifiers
import           Disco.Types.Rules

-- import qualified Debug.Trace                      as Debug

traceM :: Applicative f => String -> f ()
traceM _ = pure ()
-- traceM = Debug.traceM

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM _ = pure ()
-- traceShowM = Debug.traceShowM

--------------------------------------------------
-- Solver errors

-- | Type of errors which can be generated by the constraint solving
--   process.
data SolveError where
  NoWeakUnifier :: SolveError
  NoUnify       :: SolveError
  UnqualBase    :: Qualifier -> BaseTy    -> SolveError
  Unqual        :: Qualifier -> Type      -> SolveError
  QualSkolem    :: Qualifier -> Name Type -> SolveError
  Unknown       :: SolveError
  deriving Show

instance Semigroup SolveError where
  e <> _ = e

instance Monoid SolveError where
  mempty  = Unknown
  mappend = (<>)

-- | Convert 'Nothing' into the given error.
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e Nothing  = throwError e
maybeError _ (Just a) = return a

--------------------------------------------------
-- Solver monad

type SolveM a = FreshMT (Except SolveError) a

runSolveM :: SolveM a -> Either SolveError a
runSolveM = runExcept . runFreshMT

liftExcept :: MonadError e m => Except e a -> m a
liftExcept = either throwError return . runExcept

reifyExcept :: MonadError e m => m a -> m (Either e a)
reifyExcept m = (Right <$> m) `catchError` (return . Left)

filterExcept :: MonadError e m => [m a] -> m [a]
filterExcept ms = do
  es <- mapM reifyExcept ms
  case partitionEithers es of
    (e:_, []) -> throwError e
    (_, as)   -> return as

--------------------------------------------------
-- Simple constraints

data SimpleConstraint where
  (:<:) :: Type -> Type -> SimpleConstraint
  (:=:) :: Type -> Type -> SimpleConstraint
  deriving (Show, Eq, Ord, Generic)

instance Alpha SimpleConstraint

instance Subst Type SimpleConstraint

--------------------------------------------------
-- Simplifier types

-- Uses TH to generate lenses so it has to go here before other stuff.

---------------------------------
-- Variable maps

-- | Information about a particular type variable.  More information
--   may be added in the future (e.g. polarity).
data TyVarInfo = TVI
  { _tyVarIlk  :: First Ilk   -- ^ The ilk (unification or skolem) of the variable, if known
  , _tyVarSort :: Sort        -- ^ The sort (set of qualifiers) of the type variable.
  }
  deriving (Show)

makeLenses ''TyVarInfo

-- | Create a 'TyVarInfo' given an 'Ilk' and a 'Sort'.
mkTVI :: Ilk -> Sort -> TyVarInfo
mkTVI = TVI . First . Just

-- | We can learn different things about a type variable from
--   different places; the 'Semigroup' instance allows combining
--   information about a type variable into a single record.
instance Semigroup TyVarInfo where
  TVI i1 s1 <> TVI i2 s2 = TVI (i1 <> i2) (s1 <> s2)

-- | A 'TyVarInfoMap' records what we know about each type variable;
--   it is a mapping from type variable names to 'TyVarInfo' records.
newtype TyVarInfoMap = VM { unVM :: Map (Name Type) TyVarInfo }
  deriving (Show)

-- | Utility function for acting on a 'TyVarInfoMap' by acting on the
--   underlying 'Map'.
onVM ::
  (Map (Name Type) TyVarInfo -> Map (Name Type) TyVarInfo) ->
  TyVarInfoMap -> TyVarInfoMap
onVM f (VM m) = VM (f m)

-- | Look up a given variable name in a 'TyVarInfoMap'.
lookupVM :: Name Type -> TyVarInfoMap -> Maybe TyVarInfo
lookupVM v = M.lookup v . unVM

-- | Remove the mapping for a particular variable name (if it exists)
--   from a 'TyVarInfoMap'.
deleteVM :: Name Type -> TyVarInfoMap -> TyVarInfoMap
deleteVM = onVM . M.delete

-- | Given a list of type variable names, add them all to the
--   'TyVarInfoMap' as 'Skolem' variables (with a trivial sort).
addSkolems :: [Name Type] -> TyVarInfoMap -> TyVarInfoMap
addSkolems vs = onVM $ \vm -> foldl' (flip (\v -> M.insert v (mkTVI Skolem mempty))) vm vs

-- | The @Semigroup@ instance for 'TyVarInfoMap' unions the two maps,
--   combining the info records for any variables occurring in both
--   maps.
instance Semigroup TyVarInfoMap where
  VM sm1 <> VM sm2 = VM (M.unionWith (<>) sm1 sm2)

instance Monoid TyVarInfoMap where
  mempty  = VM M.empty
  mappend = (<>)

-- | Get the sort of a particular variable recorded in a
--   'TyVarInfoMap'.  Returns the trivial (empty) sort for a variable
--   not in the map.
getSort :: TyVarInfoMap -> Name Type -> Sort
getSort (VM m) v = maybe topSort (view tyVarSort) (M.lookup v m)

-- | Get the 'Ilk' of a variable recorded in a 'TyVarInfoMap'.
--   Returns @Nothing@ if the variable is not in the map, or if its
--   ilk is not known.
getIlk :: TyVarInfoMap -> Name Type -> Maybe Ilk
getIlk (VM m) v = (m ^? ix v . tyVarIlk) >>= getFirst

-- | Extend the sort of a type variable by combining its existing sort
--   with the given one.  Has no effect if the variable is not already
--   in the map.
extendSort :: Name Type -> Sort -> TyVarInfoMap -> TyVarInfoMap
extendSort x s = onVM (at x . _Just . tyVarSort %~ (`S.union` s))

---------------------------------
-- Simplifier state

-- The simplification stage maintains a mutable state consisting of
-- the current qualifier map (containing wanted qualifiers for type
-- variables), the list of remaining SimpleConstraints, and the
-- current substitution.  It also keeps track of seen constraints, so
-- expansion of recursive types can stop when encountering a
-- previously seen constraint.
data SimplifyState = SS
  { _ssVarMap      :: TyVarInfoMap
  , _ssConstraints :: [SimpleConstraint]
  , _ssSubst       :: S
  , _ssSeen        :: Set SimpleConstraint
  }

makeLenses ''SimplifyState

lkup :: (Ord k, Show k, Show (Map k a)) => String -> Map k a -> k -> a
lkup msg m k = fromMaybe (error errMsg) (M.lookup k m)
  where
    errMsg = unlines
      [ "Key lookup error:"
      , "  Key = " ++ show k
      , "  Map = " ++ show m
      , "  Location: " ++ msg
      ]

--------------------------------------------------
-- Top-level solver algorithm

solveConstraint :: TyDefCtx -> Constraint -> SolveM S
solveConstraint tyDefns c = do

  -- Step 1. Open foralls (instantiating with skolem variables) and
  -- collect wanted qualifiers; also expand disjunctions.  Result in a
  -- list of possible constraint sets; each one consists of equational
  -- and subtyping constraints in addition to qualifiers.

  traceShowM c

  traceM "------------------------------"
  traceM "Decomposing constraints..."

  qcList <- decomposeConstraint c

  -- Now try continuing with each set and pick the first one that has
  -- a solution.
  msum (map (uncurry (solveConstraintChoice tyDefns)) qcList)

solveConstraintChoice :: TyDefCtx -> TyVarInfoMap -> [SimpleConstraint] -> SolveM S
solveConstraintChoice tyDefns quals cs = do

  traceM (show quals)
  traceM (show cs)

  -- Step 2. Check for weak unification to ensure termination. (a la
  -- Traytel et al).

  let toEqn (t1 :<: t2) = (t1,t2)
      toEqn (t1 :=: t2) = (t1,t2)
  _ <- maybeError NoWeakUnifier $ weakUnify tyDefns (map toEqn cs)

  -- Step 3. Simplify constraints, resulting in a set of atomic
  -- subtyping constraints.  Also simplify/update qualifier set
  -- accordingly.

  traceM "------------------------------"
  traceM "Running simplifier..."

  (vm, atoms, theta_simp) <- liftExcept (simplify tyDefns quals cs)

  traceM (show vm)
  traceM (show atoms)
  traceM (show theta_simp)

  -- Step 4. Turn the atomic constraints into a directed constraint
  -- graph.

  traceM "------------------------------"
  traceM "Generating constraint graph..."

  -- Some variables might have qualifiers but not participate in any
  -- equality or subtyping relations (see issue #153); make sure to
  -- extract them and include them in the constraint graph as isolated
  -- vertices
  let mkAVar (v, First (Just Skolem)) = AVar (S v)
      mkAVar (v, _                  ) = AVar (U v)
      vars = S.fromList . map (mkAVar . second (view tyVarIlk)) . M.assocs . unVM $ vm
      g = mkConstraintGraph vars atoms

  traceShowM g

  -- Step 5.
  -- Check for any weakly connected components containing more
  -- than one skolem, or a skolem and a base type; such components are
  -- not allowed.  Other WCCs with a single skolem simply unify to
  -- that skolem.

  traceM "------------------------------"
  traceM "Checking WCCs for skolems..."

  (g', theta_skolem) <- liftExcept (checkSkolems tyDefns vm g)
  traceShowM theta_skolem

  -- We don't need to ensure that theta_skolem respects sorts since
  -- checkSkolems will only unify skolem vars with unsorted variables.


  -- Step 6. Eliminate cycles from the graph, turning each strongly
  -- connected component into a single node, unifying all the atoms in
  -- each component.

  traceM "------------------------------"
  traceM "Collapsing SCCs..."

  (g'', theta_cyc) <- liftExcept (elimCycles tyDefns g')

  traceShowM g''
  traceShowM theta_cyc

  -- Check that the resulting substitution respects sorts...
  let sortOK (x, TyAtom (ABase ty))   = hasSort ty (getSort vm x)
      sortOK (_, TyAtom (AVar (U _))) = True
      sortOK p                        = error $ "Impossible! sortOK " ++ show p
  unless (all sortOK (Subst.toList theta_cyc))
    $ throwError NoUnify

  -- ... and update the sort map if we unified any type variables.
  -- Just make sure that if theta_cyc maps x |-> y, then y picks up
  -- the sort of x.

  traceM "Old sort map:"
  traceShowM vm

  let vm' = foldr updateVarMap vm (Subst.toList theta_cyc)
        where
          updateVarMap :: (Name Type, Type) -> TyVarInfoMap -> TyVarInfoMap
          updateVarMap (x, TyAtom (AVar (U y))) vmm = extendSort y (getSort vmm x) vmm
          updateVarMap _                        vmm = vmm

  traceM "Updated sort map:"
  traceShowM vm'

  -- Steps 7 & 8: solve the graph, iteratively finding satisfying
  -- assignments for each type variable based on its successor and
  -- predecessor base types in the graph; then unify all the type
  -- variables in any remaining weakly connected components.

  traceM "------------------------------"
  traceM "Solving for type variables..."

  theta_sol       <- solveGraph vm' g''
  traceShowM theta_sol

  traceM "------------------------------"
  traceM "Composing final substitution..."

  let theta_final = theta_sol @@ theta_cyc @@ theta_skolem @@ theta_simp
  traceShowM theta_final

  return theta_final


--------------------------------------------------
-- Step 1. Constraint decomposition.

decomposeConstraint :: Constraint -> SolveM [(TyVarInfoMap, [SimpleConstraint])]
decomposeConstraint (CSub t1 t2) = return [(mempty, [t1 :<: t2])]
decomposeConstraint (CEq  t1 t2) = return [(mempty, [t1 :=: t2])]
decomposeConstraint (CQual q ty) = (:[]) . (, []) <$> decomposeQual ty q
decomposeConstraint (CAnd cs)    = map mconcat . sequence <$> mapM decomposeConstraint cs
decomposeConstraint CTrue        = return [mempty]
decomposeConstraint (CAll ty)    = do
  (vars, c) <- unbind ty
  let c' = substs (mkSkolems vars) c
  (map . first . addSkolems) vars <$> decomposeConstraint c'

  where
    mkSkolems :: [Name Type] -> [(Name Type, Type)]
    mkSkolems = map (id &&& TySkolem)

decomposeConstraint (COr cs)     = concat <$> filterExcept (map decomposeConstraint cs)

decomposeQual :: Type -> Qualifier -> SolveM TyVarInfoMap
decomposeQual (TyAtom a) q             = checkQual q a
  -- XXX Really we should be able to check by induction whether a
  -- user-defined type has a certain sort.
decomposeQual ty@(TyCon (CUser _) _) q = throwError $ Unqual q ty
decomposeQual ty@(TyCon c tys) q
  = case qualRules c q of
      Nothing -> throwError $ Unqual q ty
      Just qs -> mconcat <$> zipWithM (maybe (return mempty) . decomposeQual) tys qs

checkQual :: Qualifier -> Atom -> SolveM TyVarInfoMap
checkQual q (AVar (U v)) = return . VM . M.singleton v $ mkTVI Unification (S.singleton q)
checkQual q (AVar (S v)) = throwError $ QualSkolem q v
checkQual q (ABase bty)  =
  case hasQual bty q of
    True  -> return mempty
    False -> throwError $ UnqualBase q bty

--------------------------------------------------
-- Step 3. Constraint simplification.

-- SimplifyM a = StateT SimplifyState SolveM a
--
--   (we can't literally write that as the definition since SolveM is
--   a type synonym and hence must be fully applied)

type SimplifyM a = StateT SimplifyState (FreshMT (Except SolveError)) a

-- | This step does unification of equality constraints, as well as
--   structural decomposition of subtyping constraints.  For example,
--   if we have a constraint (x -> y) <: (z -> Int), then we can
--   decompose it into two constraints, (z <: x) and (y <: Int); if we
--   have a constraint v <: (a,b), then we substitute v ↦ (x,y) (where
--   x and y are fresh type variables) and continue; and so on.
--
--   After this step, the remaining constraints will all be atomic
--   constraints, that is, only of the form (v1 <: v2), (v <: b), or
--   (b <: v), where v is a type variable and b is a base type.

simplify :: TyDefCtx -> TyVarInfoMap -> [SimpleConstraint] -> Except SolveError (TyVarInfoMap, [(Atom, Atom)], S)
simplify tyDefns origVM cs
  = (\(SS vm' cs' s' _) -> (vm', map extractAtoms cs', s'))
  <$> contFreshMT (execStateT simplify' (SS origVM cs idS S.empty)) n
  where

    fvNums :: Alpha a => [a] -> [Integer]
    fvNums = map (name2Integer :: Name Type -> Integer) . toListOf fv

    -- Find first unused integer in constraint free vars and sort map
    -- domain, and use it to start the fresh var generation, so we don't
    -- generate any "fresh" names that interfere with existing names
    n1 = maximum0 . fvNums $ cs
    n = succ . maximum . (n1:) . fvNums . M.keys . unVM $ origVM

    maximum0 [] = 0
    maximum0 xs = maximum xs

    -- Extract the type atoms from an atomic constraint.
    extractAtoms :: SimpleConstraint -> (Atom, Atom)
    extractAtoms (TyAtom a1 :<: TyAtom a2) = (a1, a2)
    extractAtoms c = error $ "Impossible: simplify left non-atomic or non-subtype constraint " ++ show c

    -- Iterate picking one simplifiable constraint and simplifying it
    -- until none are left.
    simplify' :: SimplifyM ()
    simplify' = do
      -- q <- gets fst
      -- traceM (pretty q)
      -- traceM ""

      mc <- pickSimplifiable
      case mc of
        Nothing -> return ()
        Just s  -> do

          traceM (show s)
          traceM "---------------------------------------"

          simplifyOne s
          simplify'

    -- Pick out one simplifiable constraint, removing it from the list
    -- of constraints in the state.  Return Nothing if no more
    -- constraints can be simplified.
    pickSimplifiable :: SimplifyM (Maybe SimpleConstraint)
    pickSimplifiable = do
      curCs <- use ssConstraints
      case pick simplifiable curCs of
        Nothing     -> return Nothing
        Just (a,as) -> do
          ssConstraints .= as
          return (Just a)

    -- Pick the first element from a list satisfying the given
    -- predicate, returning the element and the list with the element
    -- removed.
    pick :: (a -> Bool) -> [a] -> Maybe (a,[a])
    pick _ [] = Nothing
    pick p (a:as)
      | p a       = Just (a,as)
      | otherwise = second (a:) <$> pick p as

    -- Check if a constraint can be simplified.  An equality
    -- constraint can always be "simplified" via unification.  A
    -- subtyping constraint can be simplified if either it involves a
    -- type constructor (in which case we can decompose it), or if it
    -- involves two base types (in which case it can be removed if the
    -- relationship holds).
    simplifiable :: SimpleConstraint -> Bool
    simplifiable (_ :=: _)                               = True
    simplifiable (TyCon {} :<: TyCon {})                 = True
    simplifiable (TyVar {} :<: TyCon {})                 = True
    simplifiable (TyCon {} :<: TyVar {})                 = True
    simplifiable (TyCon (CUser _) _ :<: _)               = True
    simplifiable (_ :<: TyCon (CUser _) _)               = True
    simplifiable (TyAtom (ABase _) :<: TyAtom (ABase _)) = True

    simplifiable _                                       = False

    -- Simplify the given simplifiable constraint.  If the constraint
    -- has already been seen before (due to expansion of a recursive
    -- type), just throw it away and stop.
    simplifyOne :: SimpleConstraint -> SimplifyM ()
    simplifyOne c = do
      seen <- use ssSeen
      case c `S.member` seen of
        True  -> return ()
        False -> do
          ssSeen %= S.insert c
          simplifyOne' c

    -- XXX comment me
    simplifyOne' :: SimpleConstraint -> SimplifyM ()

    -- If we have an equality constraint, run unification on it.  The
    -- resulting substitution is applied to the remaining constraints
    -- as well as prepended to the current substitution.

    simplifyOne' (ty1 :=: ty2) =
      case unify tyDefns [(ty1, ty2)] of
        Nothing -> throwError NoUnify
        Just s' -> extendSubst s'

    -- If we see a constraint of the form (T <: a), where T is a
    -- user-defined type and a is a type variable, then just turn it
    -- into an equality (T = a).  This is sound but probably not
    -- complete.  The alternative seems quite complicated, possibly
    -- even undecidable.  See https://github.com/disco-lang/disco/issues/207 .
    simplifyOne' (ty1@(TyCon (CUser _) _) :<: ty2@TyVar{})
      = simplifyOne' (ty1 :=: ty2)

    -- Otherwise, expand the user-defined type and continue.
    simplifyOne' (TyCon (CUser t) ts :<: ty2) =
      case M.lookup t tyDefns of
        Nothing  -> throwError Unknown
        Just (TyDefBody _ body) ->
          ssConstraints %= ((body ts :<: ty2) :)

    -- Turn  a <: T  into  a = T.  See comment above.
    simplifyOne' (ty1@TyVar{} :<: ty2@(TyCon (CUser _) _))
      = simplifyOne' (ty1 :=: ty2)

    simplifyOne' (ty1 :<: TyCon (CUser t) ts) =
      case M.lookup t tyDefns of
        Nothing  -> throwError Unknown
        Just (TyDefBody _ body) ->
          ssConstraints %= ((ty1 :<: body ts) :)

    -- Given a subtyping constraint between two type constructors,
    -- decompose it if the constructors are the same (or fail if they
    -- aren't), taking into account the variance of each argument to
    -- the constructor.  Container types are a special case;
    -- recursively generate a subtyping constraint for their
    -- constructors as well.
    simplifyOne' (TyCon c1@(CContainer ctr1) tys1 :<: TyCon (CContainer ctr2) tys2) =
      ssConstraints %=
        (( (TyAtom ctr1 :<: TyAtom ctr2)
         : zipWith3 variance (arity c1) tys1 tys2
         )
         ++)

    simplifyOne' (TyCon c1 tys1 :<: TyCon c2 tys2)
      | c1 /= c2  = throwError NoUnify
      | otherwise =
          ssConstraints %= (zipWith3 variance (arity c1) tys1 tys2 ++)

    -- Given a subtyping constraint between a variable and a type
    -- constructor, expand the variable into the same constructor
    -- applied to fresh type variables.
    simplifyOne' con@(TyVar a   :<: TyCon c _) = expandStruct a c con
    simplifyOne' con@(TyCon c _ :<: TyVar a  ) = expandStruct a c con

    -- Given a subtyping constraint between two base types, just check
    -- whether the first is indeed a subtype of the second.  (Note
    -- that we only pattern match here on type atoms, which could
    -- include variables, but this will only ever get called if
    -- 'simplifiable' was true, which checks that both are base
    -- types.)
    simplifyOne' (TyAtom (ABase b1) :<: TyAtom (ABase b2)) = do
      case isSubB b1 b2 of
        True  -> return ()
        False -> throwError NoUnify

    simplifyOne' (s :<: t) =
      error $ "Impossible! simplifyOne' " ++ show s ++ " :<: " ++ show t

    expandStruct :: Name Type -> Con -> SimpleConstraint -> SimplifyM ()
    expandStruct a c con = do
      as <- mapM (const (TyVar <$> fresh (string2Name "a"))) (arity c)
      let s' = a |-> TyCon c as
      ssConstraints %= (con:)
      extendSubst s'

    -- 1. compose s' with current subst
    -- 2. apply s' to constraints
    -- 3. apply s' to qualifier map and decompose
    extendSubst :: S -> SimplifyM ()
    extendSubst s' = do
      ssSubst %= (s'@@)
      ssConstraints %= applySubst s'
      substVarMap s'

    substVarMap :: S -> SimplifyM ()
    substVarMap s' = do
      vm <- use ssVarMap

      -- 1. Get quals for each var in domain of s' and match them with
      -- the types being substituted for those vars.

      let tySorts :: [(Type, Sort)]
          tySorts = map (second (view tyVarSort)) . mapMaybe (traverse (`lookupVM` vm) . swap) $ Subst.toList s'

          tyQualList :: [(Type, Qualifier)]
          tyQualList = concatMap (sequenceA . second S.toList) tySorts

      -- 2. Decompose the resulting qualifier constraints

      vm' <- lift $ mconcat <$> mapM (uncurry decomposeQual) tyQualList

      -- 3. delete domain of s' from vm and merge in decomposed quals.

      ssVarMap .= vm' <> foldl' (flip deleteVM) vm (dom s')

      -- The above works even when unifying two variables.  Say we have
      -- the TyVarInfoMap
      --
      --   a |-> {add}
      --   b |-> {sub}
      --
      -- and we get back theta = [a |-> b].  The domain of theta
      -- consists solely of a, so we look up a in the TyVarInfoMap and get
      -- {add}.  We therefore generate the constraint 'add (theta a)'
      -- = 'add b' which can't be decomposed at all, and hence yields
      -- the TyVarInfoMap {b |-> {add}}.  We then delete a from the
      -- original TyVarInfoMap and merge the result with the new TyVarInfoMap,
      -- yielding {b |-> {sub,add}}.


    -- Create a subtyping constraint based on the variance of a type
    -- constructor argument position: in the usual order for
    -- covariant, and reversed for contravariant.
    variance Co     ty1 ty2 = ty1 :<: ty2
    variance Contra ty1 ty2 = ty2 :<: ty1

--------------------------------------------------
-- Step 4: Build constraint graph

-- | Given a list of atoms and atomic subtype constraints (each pair
--   @(a1,a2)@ corresponds to the constraint @a1 <: a2@) build the
--   corresponding constraint graph.
mkConstraintGraph :: Ord a => Set a -> [(a, a)] -> Graph a
mkConstraintGraph as cs = G.mkGraph nodes (S.fromList cs)
  where
    nodes = as `S.union` S.fromList (cs ^.. traverse . each)

--------------------------------------------------
-- Step 5: Check skolems

-- | Check for any weakly connected components containing more than
--   one skolem, or a skolem and a base type, or a skolem and any
--   variables with nontrivial sorts; such components are not allowed.
--   If there are any WCCs with a single skolem, no base types, and
--   only unsorted variables, just unify them all with the skolem and
--   remove those components.
checkSkolems :: TyDefCtx -> TyVarInfoMap -> Graph Atom -> Except SolveError (Graph UAtom, S)
checkSkolems tyDefns vm graph = do
  let skolemWCCs :: [Set Atom]
      skolemWCCs = filter (any isSkolem) $ G.wcc graph

      ok wcc =  S.size (S.filter isSkolem wcc) <= 1
             && all (\case { ABase _    -> False
                           ; AVar (S _) -> True
                           ; AVar (U v) -> maybe True (S.null . view tyVarSort) (lookupVM v vm) })
                wcc

      (good, bad) = partition ok skolemWCCs

  unless (null bad) $ throwError NoUnify

  -- take all good sets and
  --   (1) delete them from the graph
  --   (2) unify them all with the skolem
  unifyWCCs graph idS good

  where
    noSkolems :: Atom -> UAtom
    noSkolems (ABase b)    = UB b
    noSkolems (AVar (U v)) = UV v
    noSkolems (AVar (S v)) = error $ "Skolem " ++ show v ++ " remaining in noSkolems"

    unifyWCCs g s []     = return (G.map noSkolems g, s)
    unifyWCCs g s (u:us) = do
      traceM $ "Unifying " ++ show (u:us) ++ "..."

      let g' = foldl' (flip G.delete) g u

          ms' = unifyAtoms tyDefns (S.toList u)
      case ms' of
        Nothing -> throwError NoUnify
        Just s' -> unifyWCCs g' (atomToTypeSubst s' @@ s) us

--------------------------------------------------
-- Step 6: Eliminate cycles

-- | Eliminate cycles in the constraint set by collapsing each
--   strongly connected component to a single node, (unifying all the
--   types in the SCC). A strongly connected component is a maximal
--   set of nodes where every node is reachable from every other by a
--   directed path; since we are using directed edges to indicate a
--   subtyping constraint, this means every node must be a subtype of
--   every other, and the only way this can happen is if all are in
--   fact equal.
--
--   Of course, this step can fail if the types in a SCC are not
--   unifiable.  If it succeeds, it returns the collapsed graph (which
--   is now guaranteed to be acyclic, i.e. a DAG) and a substitution.
elimCycles :: TyDefCtx -> Graph UAtom -> Except SolveError (Graph UAtom, S)
elimCycles tyDefns = elimCyclesGen uatomToTypeSubst (unifyUAtoms tyDefns)

elimCyclesGen
  :: forall a b. (Subst a a, Ord a)
  => (Substitution a -> Substitution b) -> ([a] -> Maybe (Substitution a))
  -> Graph a -> Except SolveError (Graph a, Substitution b)
elimCyclesGen genSubst genUnify g
  = maybeError NoUnify
  $ (G.map fst &&& (genSubst . compose . S.map snd . G.nodes)) <$> g'
  where

    g' :: Maybe (Graph (a, Substitution a))
    g' = G.sequenceGraph $ G.map unifySCC (G.condensation g)

    unifySCC :: Set a -> Maybe (a, Substitution a)
    unifySCC uatoms = case S.toList uatoms of
      []       -> error "Impossible! unifySCC on the empty set"
      as@(a:_) -> (flip applySubst a &&& id) <$> genUnify as

------------------------------------------------------------
-- Steps 7 and 8: Constraint resolution
------------------------------------------------------------

-- | Rels stores the set of base types and variables related to a
--   given variable in the constraint graph (either predecessors or
--   successors, but not both).
data Rels = Rels
  { baseRels :: Set BaseTy
  , varRels  :: Set (Name Type)
  }
  deriving (Show, Eq)

-- | A RelMap associates each variable to its sets of base type and
--   variable predecessors and successors in the constraint graph.
type RelMap = Map (Name Type, Dir) Rels

-- | Modify a @RelMap@ to record the fact that we have solved for a
--   type variable.  In particular, delete the variable from the
--   @RelMap@ as a key, and also update the relative sets of every
--   other variable to remove this variable and add the base type we
--   chose for it.
substRel :: Name Type -> BaseTy -> RelMap -> RelMap
substRel x ty
  = M.delete (x,SuperTy)
  . M.delete (x,SubTy)
  . M.map (\r@(Rels b v) -> if x `S.member` v then Rels (S.insert ty b) (S.delete x v) else r)

-- | Essentially dirtypesBySort vm rm dir t s x finds all the
--   dir-types (sub- or super-) of t which have sort s, relative to
--   the variables in x.  This is \overbar{T}_S^X (resp. \underbar...)
--   from Traytel et al.
dirtypesBySort :: TyVarInfoMap -> RelMap -> Dir -> BaseTy -> Sort -> Set (Name Type) -> [BaseTy]
dirtypesBySort vm relMap dir t s x

    -- Keep only those supertypes t' of t
  = keep (dirtypes dir t) $ \t' ->
      -- which have the right sort, and such that
      hasSort t' s &&

      -- for all variables beta \in x,
      forAll x (\beta ->

        -- there is at least one type t'' which is a subtype of t'
        -- which would be a valid solution for beta, that is,
        exists (dirtypes (other dir) t') $ \t'' ->

          -- t'' has the sort beta is supposed to have, and
          hasSort t'' (getSort vm beta) &&

          -- t'' is a supertype of every base type predecessor of beta.
          forAll (baseRels (lkup "dirtypesBySort, beta rel" relMap (beta, other dir)))
            (isDirB dir t''))

    -- The above comments are written assuming dir = Super; of course,
    -- if dir = Sub then just swap "super" and "sub" everywhere.

  where
    forAll, exists :: Foldable t => t a -> (a -> Bool) -> Bool
    forAll = flip all
    exists = flip any
    keep   = flip filter

-- | Sort-aware infimum or supremum.
limBySort :: TyVarInfoMap -> RelMap -> Dir -> [BaseTy] -> Sort -> Set (Name Type) -> Maybe BaseTy
limBySort vm rm dir ts s x
  = (\is -> find (\lim -> all (\u -> isDirB dir u lim) is) is)
  . isects
  . map (\t -> dirtypesBySort vm rm dir t s x)
  $ ts
  where
    isects = foldr1 intersect

lubBySort, glbBySort :: TyVarInfoMap -> RelMap -> [BaseTy] -> Sort -> Set (Name Type) -> Maybe BaseTy
lubBySort vm rm = limBySort vm rm SuperTy
glbBySort vm rm = limBySort vm rm SubTy

-- | From the constraint graph, build the sets of sub- and super- base
--   types of each type variable, as well as the sets of sub- and
--   supertype variables.  For each type variable x in turn, try to
--   find a common supertype of its base subtypes which is consistent
--   with the sort of x and with the sorts of all its sub-variables,
--   as well as symmetrically a common subtype of its supertypes, etc.
--   Assign x one of the two: if it has only successors, assign it
--   their inf; otherwise, assign it the sup of its predecessors.  If
--   it has both, we have a choice of whether to assign it the sup of
--   predecessors or inf of successors; both lead to a sound &
--   complete algorithm.  We choose to assign it the sup of its
--   predecessors in this case, since it seems nice to default to
--   "simpler" types lower down in the subtyping chain.
solveGraph :: TyVarInfoMap -> Graph UAtom -> SolveM S
solveGraph vm g = atomToTypeSubst . unifyWCC <$> go topRelMap
  where
    unifyWCC :: Substitution BaseTy -> Substitution Atom
    unifyWCC s = compose (map mkEquateSubst wccVarGroups) @@ fmap ABase s
      where
        wccVarGroups :: [Set (Name Type)]
        wccVarGroups  = map (S.map getVar) . filter (all uisVar) . applySubst s $ G.wcc g
        getVar (UV v) = v
        getVar (UB b) = error
          $ "Impossible! Base type " ++ show b ++ " in solveGraph.getVar"

        mkEquateSubst :: Set (Name Type) -> Substitution Atom
        mkEquateSubst = mkEquations . S.toList

        mkEquations (a:as) = Subst.fromList . map (\v -> (coerce v, AVar (U a))) $ as
        mkEquations []     = error "Impossible! Empty set of names in mkEquateSubst"

            -- After picking concrete base types for all the type
            -- variables we can, the only thing possibly remaining in
            -- the graph are components containing only type variables
            -- and no base types.  It is sound, and simplifies the
            -- generated types considerably, to simply unify any type
            -- variables which are related by subtyping constraints.
            -- That is, we collect all the type variables in each
            -- weakly connected component and unify them.
            --
            -- As an example where this final step makes a difference,
            -- consider a term like @\x. (\y.y) x@.  A fresh type
            -- variable is generated for the type of @x@, and another
            -- for the type of @y@; the application of @(\y.y)@ to @x@
            -- induces a subtyping constraint between the two type
            -- variables.  The most general type would be something
            -- like @forall a b. (a <: b) => a -> b@, but we want to
            -- avoid generating unnecessary subtyping constraints (the
            -- type system might not even support subtyping qualifiers
            -- like this).  Instead, we unify the two type variables
            -- and the resulting type is @forall a. a -> a@.

    -- Get the successor and predecessor sets for all the type variables.
    topRelMap :: RelMap
    topRelMap
      = M.map (uncurry Rels . (S.fromAscList *** S.fromAscList)
               . partitionEithers . map uatomToEither . S.toList)
      $ M.mapKeys (,SuperTy) subMap `M.union` M.mapKeys (,SubTy) superMap

    subMap, superMap :: Map (Name Type) (Set UAtom)
    (subMap, superMap) = (onlyVars *** onlyVars) $ G.cessors g

    onlyVars :: Map UAtom (Set UAtom) -> Map (Name Type) (Set UAtom)
    onlyVars = M.mapKeys fromVar . M.filterWithKey (\a _ -> uisVar a)
      where
        fromVar (UV x) = x
        fromVar _      = error "Impossible! UB but uisVar."

    go :: RelMap -> SolveM (Substitution BaseTy)
    go relMap = traceShowM relMap >> case as of

      -- No variables left that have base type constraints.
      []    -> return idS

      -- Solve one variable at a time.  See below.
      (a:_) -> do
        traceM $ "Solving for " ++ show a
        case solveVar a of
          Nothing       -> do
            traceM $ "Couldn't solve for " ++ show a
            throwError NoUnify

          -- If we solved for a, delete it from the maps, apply the
          -- resulting substitution to the remainder (updating the
          -- relMap appropriately), and recurse.  The substitution we
          -- want will be the composition of the substitution for a
          -- with the substitution generated by the recursive call.
          --
          -- Note we don't need to delete a from the TyVarInfoMap; we
          -- never use the set of keys from the TyVarInfoMap for
          -- anything (indeed, some variables might not be keys if
          -- they have an empty sort), so it doesn't matter if old
          -- variables hang around in it.
          Just s -> do
            traceShowM s
            (@@ s) <$> go (substRel a (fromJust $ Subst.lookup (coerce a) s) relMap)

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
        -- -> N as well.  Both are acceptable.
        --
        -- In fact, this exact graph comes from (^x.x+1) which was
        -- erroneously being inferred to have type Z -> N when I first
        -- wrote the code.

        -- Get only the variables we can solve on this pass, which
        -- have base types in their predecessor or successor set.  If
        -- there are no such variables, then start picking any
        -- remaining variables with a sort and pick types for them
        -- (disco doesn't have qualified polymorphism so we can't just
        -- leave them).
        asBase
          = map fst
          . filter (not . S.null . baseRels . lkup "solveGraph.go.as" relMap)
          $ M.keys relMap
        as = case asBase of
          [] -> filter ((/= topSort) . getSort vm) . map fst $ M.keys relMap
          _  -> asBase

        -- Solve for a variable, failing if it has no solution, otherwise returning
        -- a substitution for it.
        solveVar :: Name Type -> Maybe (Substitution BaseTy)
        solveVar v =
          case ((v,SuperTy), (v,SubTy)) & over both (S.toList . baseRels . lkup "solveGraph.solveVar" relMap) of
            -- No sub- or supertypes; the only way this can happen is
            -- if it has a nontrivial sort.
            --
            -- Traytel et al. don't seem to have a rule saying what to
            -- do in this case (see Fig. 16 on p. 16 of their long
            -- version).  We used to just pick a type that inhabits
            -- the sort, but this is wrong; see
            -- https://github.com/disco-lang/disco/issues/192.
            --
            -- For now, let's assume that any situation in which we
            -- have no base sub- or supertypes but we do have
            -- nontrivial sorts means that we are dealing with numeric
            -- types; so we can just call N a base subtype and go from there.

            ([], []) ->
              -- Debug.trace (show v ++ " has no sub- or supertypes.  Assuming N as a subtype.")
              (coerce v |->) <$> lubBySort vm relMap [N] (getSort vm v)
                (varRels (lkup "solveVar none, rels" relMap (v,SubTy)))

            -- Only supertypes.  Just assign a to their inf, if one exists.
            (bsupers, []) ->
              -- Debug.trace (show v ++ " has only supertypes (" ++ show bsupers ++ ")") $
              (coerce v |->) <$> glbBySort vm relMap bsupers (getSort vm v)
                (varRels (lkup "solveVar bsupers, rels" relMap (v,SuperTy)))

            -- Only subtypes.  Just assign a to their sup.
            ([], bsubs)   ->
              -- Debug.trace (show v ++ " has only subtypes (" ++ show bsubs ++ ")") $
              -- Debug.trace ("sortmap: " ++ show vm) $
              -- Debug.trace ("relmap: " ++ show relMap) $
              -- Debug.trace ("sort for " ++ show v ++ ": " ++ show (getSort vm v)) $
              -- Debug.trace ("relvars: " ++ show (varRels (relMap ! (v,SubTy)))) $
              (coerce v |->) <$> lubBySort vm relMap bsubs (getSort vm v)
                (varRels (lkup "solveVar bsubs, rels" relMap (v,SubTy)))

            -- Both successors and predecessors.  Both must have a
            -- valid bound, and the bounds must not overlap.  Assign a
            -- to the sup of its predecessors.
            (bsupers, bsubs) -> do
              ub <- glbBySort vm relMap bsupers (getSort vm v)
                      (varRels (relMap ! (v,SuperTy)))
              lb <- lubBySort vm relMap bsubs   (getSort vm v)
                      (varRels (relMap ! (v,SubTy)))
              case isSubB lb ub of
                True  -> Just (coerce v |-> lb)
                False -> Nothing

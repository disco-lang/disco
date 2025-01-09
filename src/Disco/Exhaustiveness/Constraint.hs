module Disco.Exhaustiveness.Constraint where

import Control.Applicative (Alternative)
import Control.Monad (foldM, guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (partition)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Disco.Effects.Fresh (Fresh)
import qualified Disco.Exhaustiveness.TypeInfo as TI
import Polysemy
import qualified Disco.Types as Ty
import Polysemy.Reader
import Data.Bifunctor (first)

newtype Context = Context {getCtxVars :: [TI.TypedVar]}
  deriving (Show, Eq)

addVars :: Context -> [TI.TypedVar] -> Context
addVars (Context ctx) vars = Context (ctx ++ vars)

data Constraint where
  CMatch :: TI.DataCon -> [TI.TypedVar] -> Constraint
  CNot :: TI.DataCon -> Constraint
  CWasOriginally :: TI.TypedVar -> Constraint
  deriving (Show, Eq, Ord)

posMatch :: [Constraint] -> Maybe (TI.DataCon, [TI.TypedVar])
posMatch constraints = listToMaybe $ mapMaybe (\case (CMatch k ys) -> Just (k, ys); _ -> Nothing) constraints

negMatches :: [Constraint] -> [TI.DataCon]
negMatches = mapMaybe (\case (CNot k) -> Just k; _ -> Nothing)

type ConstraintFor = (TI.TypedVar, Constraint)

-- Resolves term equalities, finding the leftmost id for a variable
-- I believe I3 of section 3.4 allows us to
-- do a linear scan from right to left
lookupVar :: TI.TypedVar -> [ConstraintFor] -> TI.TypedVar
lookupVar x = foldr getNextId x
  where
    getNextId (x', CWasOriginally y) | x' == x = const y
    getNextId _ = id

alistLookup :: (Eq a) => a -> [(a, b)] -> [b]
alistLookup a = map snd . filter ((== a) . fst)

onVar :: TI.TypedVar -> [ConstraintFor] -> [Constraint]
onVar x cs = alistLookup (lookupVar x cs) cs

type NormRefType = (Context, [ConstraintFor])

addConstraints :: Members '[Fresh, Reader Ty.TyDefCtx] r => NormRefType -> [ConstraintFor] -> MaybeT (Sem r) NormRefType
addConstraints = foldM addConstraint

addConstraint :: Members '[Fresh, Reader Ty.TyDefCtx] r => NormRefType -> ConstraintFor -> MaybeT (Sem r) NormRefType
addConstraint nref@(_, cns) (x, c) = do
  breakIf $ any (conflictsWith c) (onVar x cns)
  addConstraintHelper nref (lookupVar x cns, c)

addConstraintHelper :: Members '[Fresh, Reader Ty.TyDefCtx] r => NormRefType -> ConstraintFor -> MaybeT (Sem r) NormRefType
addConstraintHelper nref@(ctx, cns) cf@(origX, c) = case c of
  --- Equation (10)
  CMatch k args -> do
    case getConstructorArgs k (onVar origX cns) of
      -- 10c -- TODO(colin): Still need to add type constraints!
      Just args' ->
        addConstraints
          nref
          (zipWith (\a b -> (a, CWasOriginally b)) args args')
      Nothing -> return added
  --- Equation (11)
  CNot _ -> do
    inh <- lift (inhabited added origX)
    guard inh -- ensure that origX is still inhabited, as per I2
    return added
  -- Equation (14)
  CWasOriginally y -> do
    let origY = lookupVar y cns
    if origX == origY
      then return nref
      else do
        let (noX', withX') = partition ((/= origX) . fst) cns
        addConstraints (ctx, noX' ++ [cf]) (substituteVarIDs origY origX withX')
  where
    added = (ctx, cns ++ [cf])

-----
----- Helper functions for adding constraints:
-----

breakIf :: (Alternative f) => Bool -> f ()
breakIf = guard . not

-- | Returns a predicate that returns true if another
--   constraint conflicts with the one given.
--   This alone is not sufficient to test
--   if a constraint can be added, but it
--   filters out the easy negatives early on
conflictsWith :: Constraint -> (Constraint -> Bool)
conflictsWith c = case c of
  CMatch k _ -> \case
    (CMatch k' _) | k /= k' -> True -- 10a
    (CNot k') | k == k' -> True -- 10b
    _ -> False
  CNot k -> \case
    (CMatch k' _) | k == k' -> True -- 11a
    _ -> False
  CWasOriginally _ -> const False

-- | Search for a MatchDataCon that is matching on k specifically
--   (there should be at most one, see I4 in section 3.4)
--   and if it exists, return the variable ids of its arguments
getConstructorArgs :: TI.DataCon -> [Constraint] -> Maybe [TI.TypedVar]
getConstructorArgs k cfs =
  listToMaybe $
    mapMaybe (\case (CMatch k' vs) | k' == k -> Just vs; _ -> Nothing) cfs

-- | substituting y *for* x
--   ie replace the second with the first, replace x with y
substituteVarIDs :: TI.TypedVar -> TI.TypedVar -> [ConstraintFor] -> [ConstraintFor]
substituteVarIDs y x = map (first subst)
  where
    subst var = if var == x then y else x

-- | Deals with I2 form section 3.4
--   if a variable in the context has a resolvable type, there must be at least one constructor
--   which can be instantiated without contradiction of the refinement type
--   This function tests if this is true
--   NOTE(colin): we may eventually have type constraints
--   and we would need to worry pulling them from nref here
inhabited :: Members '[Fresh, Reader Ty.TyDefCtx] r => NormRefType -> TI.TypedVar -> Sem r Bool
inhabited n var = do
  tyCtx <- ask @Ty.TyDefCtx
  case TI.tyDataCons (TI.getType var) tyCtx of
    TI.Infinite _ -> return True -- assume opaque types are inhabited
    TI.Finite constructors -> do
      or <$> mapM (instantiate n var) constructors

-- | Attempts to "instantiate" a match of the dataconstructor k on x
--   If we can add the MatchDataCon constraint to the normalized refinement
--   type without contradiction (a Nothing value),
--   then x is inhabited by k and we return true
instantiate :: Members '[Fresh, Reader Ty.TyDefCtx] r => NormRefType -> TI.TypedVar -> TI.DataCon -> Sem r Bool
instantiate (ctx, cns) var k = do
  args <- TI.newVars $ TI.dcTypes k
  let attempt = (ctx `addVars` args, cns) `addConstraint` (var, CMatch k args)
  isJust <$> runMaybeT attempt

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Inhabitants2 where

import Control.Applicative
import Control.Monad (foldM, forM, guard, join, replicateM)
import Control.Monad.State (runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (nub, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Types as Ty
import qualified Uncovered as U

-- type NormRefType = (U.Context, [ConstraintFor])

-- type ConstraintFor = (F.VarID, Constraint)

data Context = Context {typeMap :: TypeMap, aliasMap :: AliasMap}
  deriving (Show, Eq)

instance Semigroup Context where
  (<>) = undefined

type TypeMap = M.Map F.VarID Ty.Type

type AliasMap = M.Map F.VarID F.VarID

lookupVar :: F.VarID -> Context -> Maybe F.VarID
lookupVar varId Context {aliasMap = a} = M.lookup varId a

insertEqualilty :: F.VarID -> F.VarID -> Context -> Maybe Context
insertEqualilty original current ctx@Context {typeMap = t, aliasMap = a} = do
  original' <- lookupVar original ctx
  return Context {typeMap = t, aliasMap = M.insert current original' a}

-- insertType

type ConstraintMap = M.Map F.VarID Constraint
type OpaqueConstraintMap = M.Map F.VarID OpaqueConstraint

mergeConstraints :: Context -> ConstraintMap -> ConstraintMap -> (Maybe Context, ConstraintMap)
mergeConstraints ctx = M.mergeA forMissing forMissing forMatched
  where
    preserve _ x = (Just ctx, x)
    forMissing = M.traverseMissing preserve
    forMatched = M.zipWithMaybeAMatched (mergeConstraint ctx)

-- The outer maybe decides failure of the whole nref
-- the inner maybe should always be just I think
mergeConstraint :: Context -> F.VarID -> Constraint -> Constraint -> (Maybe Context, Maybe Constraint)
mergeConstraint ctx key c1 c2 =
    case (c1,c2) of
        (PosMatch k args, PosMatch k' args') ->
            if k == k'
                then undefined
                else (Nothing, Nothing)
        (PosMatch k args, NegMatch nks) ->
            if k `elem` nks
                then (Nothing, Nothing)
                else (Just ctx, Just (PosMatch k args))
        (NegMatch nks, PosMatch k args) ->
            if k `elem` nks
                then (Nothing, Nothing)
                else (Just ctx, Just (PosMatch k args))
        (NegMatch nks, NegMatch nks') ->
            if undefined 
                then (Just ctx, Just (NegMatch (NE.append nks nks')))
                else (Nothing, Nothing)


type NormRefType = (Context, AllConstraints)
type AllConstraints = (ConstraintMap, OpaqueConstraintMap)

data Constraint where
  PosMatch :: Ty.DataConstructor -> [F.VarID] -> Constraint
  NegMatch :: NE.NonEmpty Ty.DataConstructor -> Constraint
  deriving (Show, Eq, Ord)

data OpaqueConstraint where
  PosOpaqueMatch :: Int -> OpaqueConstraint
  NegOpaqueMatch :: NE.NonEmpty Int -> OpaqueConstraint
  deriving (Show, Eq, Ord)

data InhabPat where
  IPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
  IPWild :: InhabPat
  IPIntLit :: Int -> InhabPat
  IPNotIntLits :: [Int] -> InhabPat
  deriving (Show, Eq, Ord)

-- genInhab :: NE.NonEmpty F.Frame -> Context -> U.Formula -> [InhabPat]
-- genInhab frames context formula = do
--   let mNrefs = S.toList <$> normalize (context, []) formula
--   let (nrefs, frames') = runState mNrefs frames
--   nref <- nrefs
--   join $ expandVars frames' nref context
--
-- expandVars :: NE.NonEmpty F.Frame -> NormRefType -> U.Context -> [[InhabPat]]
-- expandVars frame nset vars = do
--   traverse (expandVar frame nset) vars
--
-- expandPos :: NE.NonEmpty F.Frame -> NormRefType -> (F.VarID, Ty.Type) -> Pos -> [InhabPat]
-- expandPos frames nref var pos = case pos of
--   PosInt i -> return $ IPIntLit i
--   Pos k args -> IPMatch k <$> expandVars frames nref (zip args (Ty.dcTypes k))
--
-- expandNeg :: NE.NonEmpty F.Frame -> NormRefType -> (F.VarID, Ty.Type) -> [Neg] -> [InhabPat]
-- expandNeg frames nref var neg = undefined
--
-- expandVar :: NE.NonEmpty F.Frame -> NormRefType -> (F.VarID, Ty.Type) -> [InhabPat]
-- expandVar frames nref@(_, cns) var@(x, xType) =
--   -- case posInfo of
--   --   Just pm -> expandPos frames nref (x, xType) pm
--   --   Nothing -> case negInfo of
--   --     [] -> return IPWild
--   --     nm -> expandNeg
--   case (posInfo, negInfo) of
--     (Nothing, []) -> return IPWild
--     (Nothing, nm) -> expandNeg frames nref var nm
--     (Just pm, _) -> expandPos frames nref var pm
--   where
--     -- if xType == Ty.int
--     --   then case posIntMatch of
--     --     Just i -> return $ IPIntLit i
--     --     Nothing -> case negIntMatch of
--     --       [] -> return IPWild
--     --       is -> return $ IPNotIntLits (nub is)
--     --   else case posMatch of
--     --     Just (k, ys) -> do
--     --       l <- expandVars frames nref (zip ys (Ty.dcTypes k))
--     --       return $ IPMatch k l
--     --     Nothing -> case negMatch of
--     --       [] -> return IPWild
--     --       _ ->
--     --         do
--     --           let (matchers, frames') =
--     --                 runState
--     --                   ( catMaybes
--     --                       <$> forM
--     --                         (Ty.dataCons xType)
--     --                         ( \dc -> do
--     --                             frs <- replicateM (length . Ty.dcTypes $ dc) (F.fresh Nothing)
--     --                             runMaybeT (nref `addConstraint` (x, PosMatch $ Pos dc frs))
--     --                         )
--     --                   )
--     --                   frames
--     --           if null matchers
--     --             then return IPWild
--     --             else do
--     --               m <- matchers
--     --               expandVar frames' m (x, xType)
--
--     constraintsOnX = onVar x cns
--     posInfo = listToMaybe $ mapMaybe (\case PosMatch m -> Just m; _ -> Nothing) constraintsOnX
--     negInfo = mapMaybe (\case NegMatch m -> Just m; _ -> Nothing) constraintsOnX
--     posMatch = listToMaybe $ mapMaybe (\case PosMatch (Pos k ys) -> Just (k, ys); _ -> Nothing) constraintsOnX
--     negMatch = mapMaybe (\case NegMatch (Neg k) -> Just k; _ -> Nothing) constraintsOnX
--     posIntMatch = listToMaybe $ mapMaybe (\case PosMatch (PosInt i) -> Just i; _ -> Nothing) constraintsOnX
--     negIntMatch = mapMaybe (\case NegMatch (NegInt i) -> Just i; _ -> Nothing) constraintsOnX
--
-- normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
-- normalize nref (f1 `U.And` f2) = do
--   n1 <- S.toList <$> normalize nref f1
--   rest <- traverse (`normalize` f2) n1
--   return $ S.unions rest
-- normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
-- normalize nref (U.Literal fl) = maybe S.empty S.singleton <$> runMaybeT (nref `addLiteral` fl)
--
-- addLiteral :: NormRefType -> U.Literal -> MaybeT F.Fresh NormRefType
-- addLiteral n@(context, constraints) flit = case flit of
--   U.F -> MaybeT $ pure Nothing
--   U.T -> return n
--   U.Let x xType y -> (context ++ [(x, xType)], constraints) `addConstraint` (x, TermEquality y)
--   U.MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, PosMatch $ Pos k ys)
--   U.NotDataCon k x -> n `addConstraint` (x, NegMatch $ Neg $ NE.singleton k)
--   U.MatchIntLit i x -> n `addConstraint` (x, PosMatch $ PosInt i)
--   U.NotIntLit i x -> n `addConstraint` (x, NegMatch $ NegInt $ NE.singleton i)
--
-- addConstraints :: NormRefType -> ConstraintMap -> MaybeT F.Fresh NormRefType
-- addConstraints = foldM addConstraint
--
-- addConstraint :: NormRefType -> ConstraintMap -> MaybeT F.Fresh NormRefType
-- addConstraint nref@(_, cns) (x, c) = do
--   breakIf $ any (conflictsWith c) (onVar x cns)
--   addConstraintHelper nref (lookupVar x cns, c)
--
-- addConstraintHelper :: NormRefType -> (F.VarID, Constraint) -> MaybeT F.Fresh NormRefType
-- addConstraintHelper nref@(ctx, cns) cf@(origX, c) = case c of
--   --- Equation (10)
--   PosMatch (Pos k args) -> do
--     case getConstructorArgs k (onVar origX cns) of
--       -- 10c -- TODO(colin): Still need to add type constraints!
--       Just args' -> addConstraints nref (zipWith (\a b -> (a, TermEquality b)) args args')
--       Nothing -> return added
--   --- Equation (11)
--   NegMatch (Neg _) -> do
--     inh <- lift (inhabited added origX (lookupType origX ctx))
--     guard inh -- ensure that origX is still inhabited, as per I2
--     return added
--   -- Equation (14)
--   TermEquality y -> do
--     let origY = lookupVar y cns
--     if origX == origY
--       then return nref
--       else do
--         let (noX', withX') = partition ((/= origX) . fst) cns
--         addConstraints (ctx, noX' ++ [cf]) (substituteVarIDs origY origX withX')
--   _ ->
--     -- MatchIntLit and NotIntLit are handled here
--     --   MatchIntLit does not require special treatment,
--     --   because integer data constructors have no arguments.
--     -- NotIntLit does not require special treatment,
--     --   because we assume no one will write out every integer
--     --   data constructor. So therefore we can assume inhabitation
--     return added
--   where
--     added = (ctx, cns ++ [cf])
--
-- -----
-- ----- Helper functions for adding constraints:
-- -----
--
-- breakIf :: (Alternative f) => Bool -> f ()
-- breakIf = guard . not
--
-- -- Returns a predicate that returns true if another
-- -- constraint conflicts with the one given.
-- -- This alone is not sufficient to test
-- -- if a constraint can be added, but it
-- -- filters out the easy negatives early on
-- conflictsWith :: Constraint -> (Constraint -> Bool)
-- conflictsWith c = case c of
--   PosMatch (Pos k _) -> \case
--     PosMatch (Pos k' _) | k /= k' -> True -- 10a
--     NegMatch (Neg k') | elem k k' -> True -- 10b
--     _ -> False
--   NegMatch (Neg k) -> \case
--     PosMatch (Pos k' _) | elem k' k -> True -- 11a
--     _ -> False
--   -- PosMatch (PosInt i) -> (== NegMatch (NegInt i))
--   -- NegMatch (NegInt i) -> (== PosMatch (PosInt i))
--   PosMatch (PosInt i) -> \case
--     NegMatch (NegInt i') | elem i i' -> True
--     _ -> False
--   NegMatch (NegInt i) -> \case
--     PosMatch (PosInt i') | elem i' i -> True
--     _ -> False
--   TermEquality _ -> const False
--
-- -- Search for a MatchDataCon that is matching on k specifically
-- -- (there should be at most one, see I4 in section 3.4)
-- -- and if it exists, return the variable ids of its arguments
-- getConstructorArgs :: Ty.DataConstructor -> [Constraint] -> Maybe [F.VarID]
-- getConstructorArgs k cfs =
--   listToMaybe $
--     mapMaybe (\case PosMatch (Pos k' v) | k' == k -> Just v; _ -> Nothing) cfs
--
-- -- substituting y *for* x
-- -- ie replace the second with the first, replace x with y
-- substituteVarIDs :: F.VarID -> F.VarID -> ConstraintMap -> ConstraintMap
-- substituteVarIDs y x = map (\case (x', c) | x' == x -> (y, c); cf -> cf)
--
-- -- Deals with I2 form section 3.4
-- -- if a variable in the context has a resolvable type, there must be at least one constructor
-- -- which can be instantiated without contradiction of the refinement type
-- -- This function tests if this is true
-- inhabited :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
-- inhabited n x tau = or <$> mapM (instantiate n x) (constructors n tau)
--
-- -- the NormRefType is taken in because we may eventually have type constraints
-- -- and we would need to worry about that here
-- constructors :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
-- constructors _ Ty.Type {Ty.dataCons = ks} = ks
--
-- -- Attempts to "instantiate" a match of the dataconstructor k on x
-- -- If we can add the MatchDataCon constraint to the normalized refinement
-- -- type without contradiction (a Nothing value),
-- -- then x is inhabited by k and we return true
-- instantiate :: NormRefType -> F.VarID -> Ty.DataConstructor -> F.Fresh Bool
-- instantiate (ctx, cns) x k = do
--   newVarIDs <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
--   let newVars = zip newVarIDs (Ty.dcTypes k)
--   let attempt = (ctx ++ newVars, cns) `addConstraint` (x, PosMatch $ Pos k newVarIDs)
--   isJust <$> runMaybeT attempt

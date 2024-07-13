{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Inhabitants where

import qualified Annotated as A
import Control.Applicative
import Control.Monad (foldM, forM, guard, replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (fold)
import Data.List (nub, partition)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Fresh as F
import qualified Types as Ty
import qualified Uncovered as U

type NormRefType = (U.Context, [ConstraintFor])

type ConstraintFor = (F.VarID, Constraint)

data Constraint where
  MatchDataCon :: Ty.DataConstructor -> [F.VarID] -> Constraint
  NotDataCon :: Ty.DataConstructor -> Constraint
  MatchIntLit :: Int -> Constraint
  NotIntLit :: Int -> Constraint
  TermEquality :: F.VarID -> Constraint
  deriving (Show, Eq, Ord)

-- data Pos where
--   Pos :: Ty.DataConstructor -> [F.VarID] -> Pos
--   PosInt :: Int -> Pos
--
-- data Neg where
--   Neg :: Ty.DataConstructor -> Neg
--   NegInt :: Int -> Neg

accessableRedundant :: A.Ant -> U.Context -> F.Fresh ([Int], [Int])
accessableRedundant ant args = case ant of
  A.Grhs ref i -> do
    nothing <- null . getPossibilities <$> genInhab ref args
    return $
      if nothing
        then ([], [i])
        else ([i], [])
  A.Branch a1 a2 -> mappend <$> accessableRedundant a1 args <*> accessableRedundant a2 args

-- Resolves term equalities, finding the leftmost id for a variable
-- I believe I3 of section 3.4 allows us to
-- do a linear scan from right to left
lookupVar :: F.VarID -> [ConstraintFor] -> F.VarID
lookupVar x = foldr getNextId x
  where
    getNextId (x', TermEquality y) | x' == x = const y
    getNextId _ = id

alistLookup :: (Eq a) => a -> [(a, b)] -> [b]
alistLookup a = map snd . filter ((== a) . fst)

lookupType :: F.VarID -> U.Context -> Ty.Type
lookupType x ctx = fromMaybe (error errMsg) (listToMaybe $ alistLookup x ctx)
  where
    errMsg = "Var not found in context: " ++ show x ++ " " ++ show ctx

onVar :: F.VarID -> [ConstraintFor] -> [Constraint]
onVar x cs = alistLookup (lookupVar x cs) cs

data InhabPat where
  IPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
  IPWild :: InhabPat
  IPIntLit :: Int -> InhabPat
  IPNotIntLits :: [Int] -> InhabPat
  deriving (Show, Eq, Ord)

newtype Possibilities a = Possibilities {getPossibilities :: [a]}
  deriving (Show, Eq, Ord)

retSingle :: (Monad m) => a -> m (Possibilities a)
retSingle i = return $ Possibilities [i]

-- retMulti :: (Monad m) => [a] -> m (Possibilities a)
-- retMulti is = return $ Possibilities is
--
-- unionPossible :: [Possibilities a] -> Possibilities a
-- unionPossible pss = Possibilities $ concatMap (\(Possibilities ps) -> ps) pss

genInhab :: U.RefinementType -> U.Context -> F.Fresh (Possibilities InhabPat)
genInhab (ctx, formula) args = do
  nrefs <- S.toList <$> normalize (ctx, []) formula
  genInhabNorm nrefs args

genInhabNorm :: [NormRefType] -> U.Context -> F.Fresh (Possibilities InhabPat)
genInhabNorm nrefs args = do
  n <- sequence [expandVar nref arg | nref <- nrefs, arg <- args]
  return $ anyOf n

-- I don't actually know what to call this
-- Abbreviation for "multiple cartesian product"
-- I think of the [a]s as column vectors
-- that are different heights
-- This function finds all possible paths from the
-- left to the right
--
-- When I wasn't properly using the state monad,
-- I think traverse just did this for me,
-- but here I had to think a little
--
-- Or you can kind of think of it in a DFS way
--
-- Retun all elements of the cartesian product of a list of sets
-- multiCart :: [[a]] -> [[a]]
-- multiCart [] = [[]]
-- multiCart (as : rests) = [a : rest | a <- as, rest <- multiCart rests]

-- cart2 :: (Show a) => [[a]] -> [[a]]
-- cart2 [] = [[]]
-- cart2 total@(as : rests) =
--   if length as > 1
--     then error $ show total
--     else [a : rest | a <- as, rest <- multiCart rests]

-- works
-- cart3 :: [[a]] -> [[a]]
-- cart3 = sequence

-- cartProdPossiblities :: [Possibilities a] -> Possibilities [a]
-- cartProdPossiblities [] = Possibilities []
-- cartProdPossiblities (Possibilities as : rests) = Possibilities $ do
--     a <- as
--     rest <- getPossible (cartProdPossiblities rests)
--     return (a : rest)

instance Functor Possibilities where
  fmap f (Possibilities a) = Possibilities (f <$> a)

instance Semigroup (Possibilities a) where
  (Possibilities p1) <> (Possibilities p2) = Possibilities $ p1 <> p2

instance Monoid (Possibilities a) where
  mempty = Possibilities []

prod :: Possibilities a -> Possibilities [a] -> Possibilities [a]
prod (Possibilities xs) (Possibilities yss) = Possibilities [x : ys | x <- xs, ys <- yss]

allCombinations :: [Possibilities a] -> Possibilities [a]
allCombinations = foldr prod nil
  where
    -- note, nil /= mempty
    -- VERY important
    nil = Possibilities [[]]

anyOf :: [Possibilities a] -> Possibilities a
anyOf = fold

-- cartProdPossiblities (Possibilities as : rests) = Possibilities [a : rest | a <- as, rest <- getPossible (cartProdPossiblities rests)]

-- or alternatively:
-- cart5 :: [Possibilities a] -> Possibilities [a]
-- cart5 ps = Possibilities (mapM getPossible ps)
--
-- cart6 :: [[a]] -> [[a]]
-- cart6 = sequence
--
-- cart7 :: [[a]] -> [[a]]
-- cart7 = foldr (liftA2 (:)) []
--
-- cart8 :: [[a]] -> [[a]]
-- cart8 = foldr cart []
--   where cart xs ys = [x : y | x <- xs, y <- ys]

expandVars :: NormRefType -> U.Context -> F.Fresh (Possibilities [InhabPat])
expandVars nref args = allCombinations <$> mapM (expandVar nref) args

-- newtype ArgsList = ArgsList {getArgs :: [Possibilities InhabPat]}

-- sequence <$> mapM (expandVar nref) args
-- l <- mapM (expandVar nref) args
-- if length (concat l) > length l then
--   error $ show l
--   else multiCart <$> mapM (expandVar nref) args

-- [[match foo, match bar], [wild]]
--
-- match foo |
--           | wild
-- match bar |
--
-- match foo, wild
-- match bar, wild

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
-- Maybe we could make this part of the type system somehow?
-- I don't know how
mkIPMatch :: Ty.DataConstructor -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (Ty.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPMatch k pats

expandVar :: NormRefType -> (F.VarID, Ty.Type) -> F.Fresh (Possibilities InhabPat)
expandVar nref@(_, cns) var@(x, xType) =
  if xType == Ty.int
    then case posIntMatch of
      Just i -> retSingle $ IPIntLit i
      Nothing -> case negIntMatch of
        [] -> retSingle IPWild
        is -> retSingle $ IPNotIntLits (nub is)
    else case posMatch of
      Just (k, ys) -> do
        -- get all possible inhabited argument lists
        argsPossibilities <- expandVars nref (zip ys (Ty.dcTypes k))
        -- create matching inhabited patterns for each of
        -- the possible argument lists
        return (mkIPMatch k <$> argsPossibilities)
      Nothing -> case negMatch of
        [] -> retSingle IPWild
        _ ->
          do
            let tryAddDc dc = do
                  frs <- replicateM (length . Ty.dcTypes $ dc) (F.fresh Nothing)
                  runMaybeT (nref `addConstraint` (x, MatchDataCon dc frs))

            -- Try to add a positive constraint for each data constructor
            -- to the current nref
            -- If any of these additions succeed, save that nref,
            -- it now has positive information
            matchers <- catMaybes <$> forM (Ty.dataCons xType) tryAddDc

            if null matchers
              then retSingle IPWild
              -- Go down each possible timeline, and get the
              -- Inhabited patterns for each
              -- Then simply concat to get a full list of
              -- the Inhabited patterns for each timeline
              -- else concat <$> mapM (`expandVar` var) matchers
              -- else do
              --   a <- mapM (`expandVar` var) matchers
              --   if length a > 1
              --     then error $ show a
              --     else return $ inhabUnions a
              else anyOf <$> mapM (`expandVar` var) matchers
  where
    constraintsOnX = onVar x cns
    posMatch = listToMaybe $ mapMaybe (\case MatchDataCon k ys -> Just (k, ys); _ -> Nothing) constraintsOnX
    negMatch = mapMaybe (\case NotDataCon k -> Just k; _ -> Nothing) constraintsOnX
    posIntMatch = listToMaybe $ mapMaybe (\case MatchIntLit i -> Just i; _ -> Nothing) constraintsOnX
    negIntMatch = mapMaybe (\case NotIntLit i -> Just i; _ -> Nothing) constraintsOnX

normalize :: NormRefType -> U.Formula -> F.Fresh (S.Set NormRefType)
normalize nref (f1 `U.And` f2) = do
  n1 <- S.toList <$> normalize nref f1
  rest <- traverse (`normalize` f2) n1
  return $ S.unions rest
normalize nref (f1 `U.Or` f2) = S.union <$> normalize nref f1 <*> normalize nref f2
normalize nref (U.Literal fl) = maybe S.empty S.singleton <$> runMaybeT (nref `addLiteral` fl)

addLiteral :: NormRefType -> U.Literal -> MaybeT F.Fresh NormRefType
addLiteral n@(context, constraints) flit = case flit of
  U.F -> MaybeT $ pure Nothing
  U.T -> return n
  U.Let x xType y -> (context ++ [(x, xType)], constraints) `addConstraint` (x, TermEquality y)
  U.MatchDataCon k ys x -> (context ++ zip ys (Ty.dcTypes k), constraints) `addConstraint` (x, MatchDataCon k ys)
  U.NotDataCon k x -> n `addConstraint` (x, NotDataCon k)
  U.MatchIntLit i x -> n `addConstraint` (x, MatchIntLit i)
  U.NotIntLit i x -> n `addConstraint` (x, NotIntLit i)

addConstraints :: NormRefType -> [ConstraintFor] -> MaybeT F.Fresh NormRefType
addConstraints = foldM addConstraint

addConstraint :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraint nref@(_, cns) (x, c) = do
  breakIf $ any (conflictsWith c) (onVar x cns)
  addConstraintHelper nref (lookupVar x cns, c)

addConstraintHelper :: NormRefType -> ConstraintFor -> MaybeT F.Fresh NormRefType
addConstraintHelper nref@(ctx, cns) cf@(origX, c) = case c of
  --- Equation (10)
  MatchDataCon k args -> do
    case getConstructorArgs k (onVar origX cns) of
      -- 10c -- TODO(colin): Still need to add type constraints!
      Just args' -> addConstraints nref (zipWith (\a b -> (a, TermEquality b)) args args')
      Nothing -> return added
  --- Equation (11)
  NotDataCon _ -> do
    inh <- lift (inhabited added origX (lookupType origX ctx))
    guard inh -- ensure that origX is still inhabited, as per I2
    return added
  -- Equation (14)
  TermEquality y -> do
    let origY = lookupVar y cns
    if origX == origY
      then return nref
      else do
        let (noX', withX') = partition ((/= origX) . fst) cns
        addConstraints (ctx, noX' ++ [cf]) (substituteVarIDs origY origX withX')
  _ ->
    -- MatchIntLit and NotIntLit are handled here
    --   MatchIntLit does not require special treatment,
    --   because integer data constructors have no arguments.
    -- NotIntLit does not require special treatment,
    --   because we assume no one will write out every integer
    --   data constructor. So therefore we can assume inhabitation
    return added
  where
    added = (ctx, cns ++ [cf])

-----
----- Helper functions for adding constraints:
-----

breakIf :: (Alternative f) => Bool -> f ()
breakIf = guard . not

-- Returns a predicate that returns true if another
-- constraint conflicts with the one given.
-- This alone is not sufficient to test
-- if a constraint can be added, but it
-- filters out the easy negatives early on
conflictsWith :: Constraint -> (Constraint -> Bool)
conflictsWith c = case c of
  MatchDataCon k _ -> \case
    MatchDataCon k' _ | k /= k' -> True -- 10a
    NotDataCon k' | k == k' -> True -- 10b
    _ -> False
  NotDataCon k -> \case
    MatchDataCon k' _ | k == k' -> True -- 11a
    _ -> False
  MatchIntLit i -> (== NotIntLit i)
  NotIntLit i -> (== MatchIntLit i)
  TermEquality _ -> const False

-- Search for a MatchDataCon that is matching on k specifically
-- (there should be at most one, see I4 in section 3.4)
-- and if it exists, return the variable ids of its arguments
getConstructorArgs :: Ty.DataConstructor -> [Constraint] -> Maybe [F.VarID]
getConstructorArgs k cfs =
  listToMaybe $
    mapMaybe (\case MatchDataCon k' v | k' == k -> Just v; _ -> Nothing) cfs

-- substituting y *for* x
-- ie replace the second with the first, replace x with y
substituteVarIDs :: F.VarID -> F.VarID -> [ConstraintFor] -> [ConstraintFor]
substituteVarIDs y x = map (\case (x', c) | x' == x -> (y, c); cf -> cf)

-- Deals with I2 form section 3.4
-- if a variable in the context has a resolvable type, there must be at least one constructor
-- which can be instantiated without contradiction of the refinement type
-- This function tests if this is true
inhabited :: NormRefType -> F.VarID -> Ty.Type -> F.Fresh Bool
inhabited n x tau = or <$> mapM (instantiate n x) (constructors n tau)

-- the NormRefType is taken in because we may eventually have type constraints
-- and we would need to worry about that here
constructors :: NormRefType -> Ty.Type -> [Ty.DataConstructor]
constructors _ Ty.Type {Ty.dataCons = ks} = ks

-- Attempts to "instantiate" a match of the dataconstructor k on x
-- If we can add the MatchDataCon constraint to the normalized refinement
-- type without contradiction (a Nothing value),
-- then x is inhabited by k and we return true
instantiate :: NormRefType -> F.VarID -> Ty.DataConstructor -> F.Fresh Bool
instantiate (ctx, cns) x k = do
  newVarIDs <- replicateM (length . Ty.dcTypes $ k) (F.fresh Nothing)
  let newVars = zip newVarIDs (Ty.dcTypes k)
  let attempt = (ctx ++ newVars, cns) `addConstraint` (x, MatchDataCon k newVarIDs)
  isJust <$> runMaybeT attempt

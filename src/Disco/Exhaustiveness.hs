{-# LANGUAGE PatternSynonyms #-}

module Disco.Exhaustiveness where

import Control.Monad (forM, forM_, when, zipWithM)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Disco.AST.Generic (Side (..))
import Disco.AST.Typed
  ( APattern,
    ATerm,
    pattern APBool,
    pattern APChar,
    pattern APCons,
    pattern APInj,
    pattern APList,
    pattern APNat,
    pattern APString,
    pattern APTup,
    pattern APUnit,
    pattern APVar,
    pattern APWild,
  )
import Disco.Effects.Fresh (Fresh)
import qualified Disco.Exhaustiveness.Constraint as C
import qualified Disco.Exhaustiveness.Possibilities as Poss
import qualified Disco.Exhaustiveness.TypeInfo as TI
import qualified Disco.Types as Ty
import Polysemy
import Polysemy.Reader
import Text.Show.Pretty (pPrint)
import Unbound.Generics.LocallyNameless (Name)

checkClauses :: (Members '[Fresh, Reader Ty.TyDefCtx, Embed IO] r) => Name ATerm -> [Ty.Type] -> NonEmpty [APattern] -> Sem r ()
checkClauses name types pats = do
  args <- TI.newVars types
  cl <- zipWithM (desugarClause args) [1 ..] (NonEmpty.toList pats)
  let gdt = foldr1 Branch cl

  let argsNref = (C.Context args, [])
  (normalizedNrefs, annotated) <- ua [argsNref] gdt

  -- inhab <- Poss.getPossibilities <$> findInhabitants normalizedNrefs args
  redundant <- findRedundant annotated args

  examples <- findPosExamples normalizedNrefs args

  -- when False $ do
  --   embed $ putStrLn "=====DEBUG=============================="
  --   embed $ putStrLn "GDT:"
  --   embed $ pPrint gdt
  --   embed $ putStrLn "UNCOVERED:"
  --   embed $ pPrint inhab
  --   embed $ putStrLn "REDUNDANT:"
  --   embed $ pPrint redundant
  --   embed $ putStrLn "=====GUBED=============================="

  let jspace = foldr (\a b -> a ++ " " ++ b) ""

  -- when (not . null $ inhab) $ do
  --   let patLine ipats = do
  --         let ipatArgs = jspace (map prettyInhab ipats)
  --         putStrLn $ show name ++ " " ++ ipatArgs ++ "= ..."
  --
  --   embed $ putStrLn $ "Warning: You haven't matched against:"
  --   embed $ forM_ inhab patLine

  when (not . null $ examples) $ do
    embed $ putStrLn $ "Warning: You haven't covered these cases:"
    let exampleLine exArgs = do
          let line = jspace (map prettyExample exArgs)
          putStrLn $ show name ++ " " ++ line ++ "= ..."
    embed $ forM_ examples exampleLine

  when (not . null $ redundant) $ do
    embed $ putStrLn $ "Warning: In function \"" ++ show name ++ "\", these clause numbers (1-indexed) are redundant:"
    embed $ putStrLn $ show redundant

  return ()

desugarClause :: (Members '[Fresh, Embed IO] r) => [TI.TypedVar] -> Int -> [APattern] -> Sem r Gdt
desugarClause args clauseIdx argsPats = do
  guards <- zipWithM desugarMatch args argsPats
  return $ foldr Guarded (Grhs clauseIdx) $ concat guards

-- To work with the LYG algorithm, we need to desugar n-tuples to nested pairs
-- Just having a Tuple type with a variable number of arguments breaks.
-- Imagine we have
-- foo (1,2,3) = ...
-- foo (1,(2,n)) = ...
-- if we keep things in our nice "sugared" form, the solver will get confused,
-- and not realize that the last element of the tuple is fully covered by n,
-- because there will be two notions of last element: the last in the triple and
-- the last in the nested pair
desugarTuplePats :: [APattern] -> APattern
desugarTuplePats [] = error "Found empty tuple, what happened?"
desugarTuplePats [p] = p
desugarTuplePats (pfst : rest) = APTup (Ty.getType pfst Ty.:*: Ty.getType psnd) [pfst, psnd]
  where
    psnd = desugarTuplePats rest

{-
TODO(colin): handle remaining patterns
  , APNeg     --required for integers / rationals?
  , APFrac    --required for rationals?
  algebraic (probably will be replaced anyway):
  , APAdd
  , APMul
  , APSub
-}
desugarMatch :: (Members '[Fresh, Embed IO] r) => TI.TypedVar -> APattern -> Sem r [Guard]
desugarMatch var pat = do
  case pat of
    (APTup (ta Ty.:*: tb) [pfst, psnd]) -> do
      varFst <- TI.newVar ta
      varSnd <- TI.newVar tb
      guardsFst <- desugarMatch varFst pfst
      guardsSnd <- desugarMatch varSnd psnd
      let guardPair = (var, GMatch (TI.pair ta tb) [varFst, varSnd])
      return $ [guardPair] ++ guardsFst ++ guardsSnd
    (APTup ty [_, _]) -> error $ "Tuple type that wasn't a pair???: " ++ show ty
    (APTup _ sugary) -> desugarMatch var (desugarTuplePats sugary)
    (APCons _ subHead subTail) -> do
      let typeHead = Ty.getType subHead
      let typeTail = Ty.getType subTail
      varHead <- TI.newVar typeHead
      varTail <- TI.newVar typeTail
      guardsHead <- desugarMatch varHead subHead
      guardsTail <- desugarMatch varTail subTail
      let guardCons = (var, (GMatch (TI.cons typeHead typeTail) [varHead, varTail]))
      return $ [guardCons] ++ guardsHead ++ guardsTail
    -- We have to desugar Lists into Cons and Nils
    (APList _ []) -> do
      return [(var, GMatch TI.nil [])]
    (APList ty (phead : ptail)) -> do
      -- APCons have the type of the list they are part of
      desugarMatch var (APCons ty phead (APList ty ptail))
    -- we have to desugar to a list, becuse we can match strings with cons
    (APString str) -> do
      let strType = (Ty.TyList Ty.TyC)
      desugarMatch var (APList strType (map APChar str))
    -- These are more straightforward:
    (APWild _) -> return []
    (APVar ty name) -> do
      let newAlias = TI.TypedVar (name, ty)
      return [(newAlias, GWasOriginally var)]
    (APNat _ nat) -> return [(var, GMatch (TI.natural nat) [])]
    (APUnit) -> return [(var, GMatch TI.unit [])]
    (APBool b) -> return [(var, GMatch (TI.bool b) [])]
    (APChar c) -> return [(var, GMatch (TI.char c) [])]
    (APInj (tl Ty.:+: tr) side subPat) -> do
      let dc = case side of
            L -> TI.left tl
            R -> TI.right tr
      newVar <- case side of
        L -> TI.newVar tl
        R -> TI.newVar tr
      guards <- desugarMatch newVar subPat
      return $ [(var, GMatch dc [newVar])] ++ guards
    e -> return []

data Gdt where
  Grhs :: Int -> Gdt
  Branch :: Gdt -> Gdt -> Gdt
  Guarded :: Guard -> Gdt -> Gdt
  deriving (Show, Eq)

type Guard = (TI.TypedVar, GuardConstraint)

data GuardConstraint where
  GMatch :: TI.DataCon -> [TI.TypedVar] -> GuardConstraint
  GWasOriginally :: TI.TypedVar -> GuardConstraint
  deriving (Show, Eq)

newtype Literal = Literal (TI.TypedVar, LitCond)

data LitCond where
  LitMatch :: TI.DataCon -> [TI.TypedVar] -> LitCond
  LitNot :: TI.DataCon -> LitCond
  LitWasOriginally :: TI.TypedVar -> LitCond
  deriving (Show, Eq, Ord)

data Ant where
  AGrhs :: [C.NormRefType] -> Int -> Ant
  ABranch :: Ant -> Ant -> Ant
  deriving (Show)

ua :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> Gdt -> Sem r ([C.NormRefType], Ant)
ua nrefs gdt = case gdt of
  Grhs k -> return ([], AGrhs nrefs k)
  Branch t1 t2 -> do
    (n1, u1) <- ua nrefs t1
    (n2, u2) <- ua n1 t2
    return (n2, ABranch u1 u2)
  Guarded (y, GWasOriginally x) t -> do
    n <- addLitMulti nrefs $ Literal (y, LitWasOriginally x)
    ua n t
  Guarded (x, (GMatch dc args)) t -> do
    n <- addLitMulti nrefs $ Literal (x, LitMatch dc args)
    (n', u) <- ua n t
    n'' <- addLitMulti nrefs $ Literal (x, LitNot dc)
    return (n'' ++ n', u)

addLitMulti :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> Literal -> Sem r [C.NormRefType]
addLitMulti [] _ = return []
addLitMulti (n : ns) lit = do
  r <- runMaybeT $ addLiteral n lit
  case r of
    Nothing -> addLitMulti ns lit
    Just (ctx, cfs) -> do
      ns' <- addLitMulti ns lit
      return $ (ctx, cfs) : ns'

addLiteral :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => C.NormRefType -> Literal -> MaybeT (Sem r) C.NormRefType
addLiteral (context, constraints) (Literal (x, c)) = case c of
  LitWasOriginally z ->
    (context `C.addVars` [x], constraints) `C.addConstraint` (x, C.CWasOriginally z)
  LitMatch dc args ->
    (context `C.addVars` args, constraints) `C.addConstraint` (x, C.CMatch dc args)
  LitNot dc ->
    (context, constraints) `C.addConstraint` (x, C.CNot dc)

data InhabPat where
  IPIs :: TI.DataCon -> [InhabPat] -> InhabPat
  IPNot :: [TI.DataCon] -> InhabPat
  deriving (Show, Eq, Ord)

join :: [a] -> [a] -> [a] -> [a]
join j a b = a ++ j ++ b

joinComma :: [String] -> String
joinComma = foldr1 (join ", ")

joinSpace :: [String] -> String
joinSpace = foldr1 (join " ")

-- TODO(colin): maybe fully print out tuples even if they have wildcars in the middle?
-- e.g. (1,_,_) instead of just (1,_)
-- Also, the display for matches against strings is really weird,
-- as strings are lists of chars.
-- Maybe for strings, we just list the top 3 uncovered patterns
-- consiting of only postive information, sorted by length?
-- Also, how should we print out sum types?
-- We can do right(thing) or (right thing), or (right(thing))
-- Which should we chose?
prettyInhab :: InhabPat -> String
prettyInhab (IPNot []) = "_"
prettyInhab (IPNot nots) = "Not{" ++ joinComma (map dcToString nots) ++ "}"
prettyInhab (IPIs TI.DataCon {TI.dcIdent = TI.KPair, TI.dcTypes = _} [ifst, isnd]) =
  "(" ++ prettyInhab ifst ++ ", " ++ prettyInhab isnd ++ ")"
prettyInhab (IPIs TI.DataCon {TI.dcIdent = TI.KCons, TI.dcTypes = _} [ihead, itail]) =
  "(" ++ prettyInhab ihead ++ " :: " ++ prettyInhab itail ++ ")"
prettyInhab (IPIs TI.DataCon {TI.dcIdent = TI.KLeft, TI.dcTypes = _} [l]) =
  "(left " ++ prettyInhab l ++ ")"
prettyInhab (IPIs TI.DataCon {TI.dcIdent = TI.KRight, TI.dcTypes = _} [r]) =
  "(right " ++ prettyInhab r ++ ")"
prettyInhab (IPIs dc []) = dcToString dc
prettyInhab (IPIs dc args) = dcToString dc ++ " " ++ joinSpace (map prettyInhab args)

data ExamplePat where
  ExamplePat :: TI.DataCon -> [ExamplePat] -> ExamplePat
  deriving (Show)

prettyExample :: ExamplePat -> String
prettyExample (ExamplePat TI.DataCon {TI.dcIdent = TI.KPair, TI.dcTypes = _} [ifst, isnd]) =
  "(" ++ prettyExample ifst ++ ", " ++ prettyExample isnd ++ ")"
prettyExample (ExamplePat TI.DataCon {TI.dcIdent = TI.KCons, TI.dcTypes = _} [ihead, itail]) =
  "(" ++ prettyExample ihead ++ " :: " ++ prettyExample itail ++ ")"
prettyExample (ExamplePat TI.DataCon {TI.dcIdent = TI.KLeft, TI.dcTypes = _} [l]) =
  "(left " ++ prettyExample l ++ ")"
prettyExample (ExamplePat TI.DataCon {TI.dcIdent = TI.KRight, TI.dcTypes = _} [r]) =
  "(right " ++ prettyExample r ++ ")"
prettyExample (ExamplePat dc []) = dcToString dc
prettyExample (ExamplePat dc args) = dcToString dc ++ " " ++ joinSpace (map prettyExample args)

dcToString :: TI.DataCon -> String
dcToString TI.DataCon {TI.dcIdent = ident} = case ident of
  TI.KBool b -> show b
  TI.KChar c -> show c
  TI.KNat n -> show n
  TI.KNil -> "[]"
  TI.KUnit -> "unit"
  TI.KUnkown -> "_"
  -- TODO(colin): find a way to remove these? These two shouldn't be reachable
  -- If we were in an IPIs, we already handled these above
  -- If we were in an IPNot, these aren't from opaque types,
  -- so they shouldn't appear in a Not{}
  TI.KPair -> ","
  TI.KCons -> "::"
  TI.KLeft -> "left()"
  TI.KRight -> "right()"

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
mkIPMatch :: TI.DataCon -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPIs k pats

-- No longer used to report to the user like in Haskell
-- we use findPosExamples instead because we think that will be easier
-- on a student using disco for the first time
-- However, this function is still used in redundancy checking
findInhabitants :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findInhabitants nrefs args = do
  a <- forM nrefs (`findAllForNref` args)
  return $ Poss.anyOf a

findAllForNref :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => C.NormRefType -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findAllForNref nref args = do
  argPats <- forM args (`findVarInhabitants` nref)
  return $ Poss.allCombinations argPats

findVarInhabitants :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => TI.TypedVar -> C.NormRefType -> Sem r (Poss.Possibilities InhabPat)
findVarInhabitants var nref@(_, cns) =
  case posMatch of
    Just (k, args) -> do
      argPossibilities <- findAllForNref nref args
      return (mkIPMatch k <$> argPossibilities)
    Nothing -> case nub negMatches of
      [] -> Poss.retSingle $ IPNot []
      neg -> do
        tyCtx <- ask @Ty.TyDefCtx
        case TI.tyDataCons (TI.getType var) tyCtx of
          TI.Infinite _ -> Poss.retSingle $ IPNot neg
          TI.Finite dcs ->
            do
              let tryAddDc dc = do
                    vars <- TI.newVars (TI.dcTypes dc)
                    runMaybeT (nref `C.addConstraint` (var, C.CMatch dc vars))

              -- Try to add a positive constraint for each data constructor
              -- to the current nref
              -- If any of these additions succeed, save that nref,
              -- it now has positive information
              posNrefs <- catMaybes <$> forM dcs tryAddDc

              if null posNrefs
                then Poss.retSingle $ IPNot []
                else Poss.anyOf <$> forM posNrefs (findVarInhabitants var)
  where
    constraintsOnX = C.onVar var cns
    posMatch = C.posMatch constraintsOnX
    negMatches = C.negMatches constraintsOnX

findRedundant :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => Ant -> [TI.TypedVar] -> Sem r [Int]
findRedundant ant args = case ant of
  AGrhs ref i -> do
    uninhabited <- Poss.none <$> findInhabitants ref args
    return [i | uninhabited]
  ABranch a1 a2 -> mappend <$> findRedundant a1 args <*> findRedundant a2 args

-- Less general version of the above inhabitant finding function
-- returns a maximum of 3 possible args lists that haven't been matched against,
-- as to not overwhelm new users of the language.
findPosExamples :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> [TI.TypedVar] -> Sem r [[ExamplePat]]
findPosExamples nrefs args = do
  a <- forM nrefs (`findAllPosForNref` args)
  return $ take 3 $ Poss.getPossibilities $ Poss.anyOf a

findAllPosForNref :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => C.NormRefType -> [TI.TypedVar] -> Sem r (Poss.Possibilities [ExamplePat])
findAllPosForNref nref args = do
  argPats <- forM args (`findVarPosExamples` nref)
  return $ Poss.allCombinations argPats

findVarPosExamples :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => TI.TypedVar -> C.NormRefType -> Sem r (Poss.Possibilities ExamplePat)
findVarPosExamples var nref@(_, cns) =
  case posMatch of
    Just (k, args) -> do
      argPossibilities <- findAllPosForNref nref args
      return (mkExampleMatch k <$> argPossibilities)
    Nothing -> do
      tyCtx <- ask @Ty.TyDefCtx
      let dcs = getPosFrom (TI.getType var) tyCtx negMatches
      let tryAddDc dc = do
            vars <- TI.newVars (TI.dcTypes dc)
            runMaybeT (nref `C.addConstraint` (var, C.CMatch dc vars))
      -- Try to add a positive constraint for each data constructor
      -- to the current nref
      -- If any of these additions succeed, save that nref,
      -- it now has positive information
      posNrefs <- catMaybes <$> forM dcs tryAddDc

      Poss.anyOf <$> forM posNrefs (findVarPosExamples var)
  where
    constraintsOnX = C.onVar var cns
    posMatch = C.posMatch constraintsOnX
    negMatches = C.negMatches constraintsOnX

getPosFrom :: Ty.Type -> Ty.TyDefCtx -> [TI.DataCon] -> [TI.DataCon]
getPosFrom ty ctx neg = take 3 $ filter (\x -> not (x `elem` neg)) dcs
  where
    dcs = case TI.tyDataCons ty ctx of
      TI.Finite dcs' -> dcs'
      TI.Infinite dcs' -> dcs'

mkExampleMatch :: TI.DataCon -> [ExamplePat] -> ExamplePat
mkExampleMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else ExamplePat k pats

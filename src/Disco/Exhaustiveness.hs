{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :  Disco.Exhaustiveness
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Entry point into the exhaustiveness checker.
-- Converts information into a format the checker understands,
-- then pretty prints the results of running it.
module Disco.Exhaustiveness where

import Control.Monad (forM, unless, zipWithM)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Disco.AST.Generic (Side (..))
import Disco.AST.Surface
  ( Pattern,
    prettyPatternP,
    pattern PBool,
    pattern PChar,
    pattern PInj,
    pattern PList,
    pattern PNat,
    pattern PNeg,
    pattern PString,
    pattern PTup,
    pattern PUnit,
    pattern PWild,
  )
import Disco.AST.Typed
  ( APattern,
    ATerm,
    pattern APBool,
    pattern APChar,
    pattern APCons,
    pattern APInj,
    pattern APList,
    pattern APNat,
    pattern APNeg,
    pattern APString,
    pattern APTup,
    pattern APUnit,
    pattern APVar,
    pattern APWild,
  )
import Disco.Effects.Fresh (Fresh)
import Disco.Effects.LFresh (LFresh, runLFresh)
import qualified Disco.Exhaustiveness.Constraint as C
import qualified Disco.Exhaustiveness.Possibilities as Poss
import qualified Disco.Exhaustiveness.TypeInfo as TI
import Disco.Messages
import Disco.Pretty (Doc, initPA, withPA)
import qualified Disco.Pretty.DSL as DSL
import Disco.Pretty.Prec (PA, funPA)
import qualified Disco.Types as Ty
import Polysemy
import Polysemy.Output
import Polysemy.Reader
import Unbound.Generics.LocallyNameless (Name)

-- | This exhaustiveness checking algorithm is based on the paper
--   "Lower Your Guards: A Compositional Pattern-Match Coverage Checker":
--   https://www.microsoft.com/en-us/research/uploads/prod/2020/03/lyg.pdf
--
--   Some simplifications were made to adapt the algorithm to suit Disco.
--   The most notable change is that here we always generate (at most) 3
--   concrete examples of uncovered patterns, instead of finding the most
--   general complete description of every uncovered input.
checkClauses :: (Members '[Fresh, Reader Ty.TyDefCtx, Output (Message ann), Embed IO] r) => Name ATerm -> [Ty.Type] -> NonEmpty [APattern] -> Sem r ()
checkClauses name types pats = do
  args <- TI.newVars types
  cl <- zipWithM (desugarClause args) [1 ..] (NonEmpty.toList pats)
  let gdt = foldr1 Branch cl

  let argsNref = []
  (normalizedNrefs, _) <- ua [argsNref] gdt

  examples <- findPosExamples normalizedNrefs args

  unless (null examples) $ do
    let prettyExampleArgs exArgs =
          DSL.hcat $ map prettyPrintExample exArgs

    let prettyExampleLine prettyArgs =
          DSL.text (show name) DSL.<> prettyArgs DSL.<+> DSL.text "= ..."

    let prettyExamples =
          DSL.vcat $ map (prettyExampleLine . prettyExampleArgs) examples

    warn $
      DSL.text "Warning: the function"
        DSL.<+> DSL.text (show name)
        DSL.<+> DSL.text "is undefined for some inputs. For example:"
        DSL.$+$ prettyExamples

prettyPrintExample :: ExamplePat -> Sem r (Doc ann)
prettyPrintExample = runLFresh . runReader initPA . prettyPrintPattern . exampleToDiscoPattern

prettyPrintPattern :: (Members '[Reader PA, LFresh] r) => Pattern -> Sem r (Doc ann)
prettyPrintPattern = withPA funPA . prettyPatternP

exampleToDiscoPattern :: ExamplePat -> Pattern
exampleToDiscoPattern e@(ExamplePat TI.DataCon {TI.dcIdent = ident, TI.dcTypes = types} args) = case (ident, args) of
  (TI.KUnknown, _) -> PWild
  (TI.KUnit, _) -> PUnit
  (TI.KBool b, _) -> PBool b
  (TI.KNat n, _) -> PNat n
  (TI.KInt z, _) ->
    if z >= 0
      then PNat z
      else PNeg (PNat (abs z))
  (TI.KPair, _) -> PTup $ map exampleToDiscoPattern $ resugarPair e
  (TI.KCons, _) ->
    if take 1 types == [Ty.TyC]
      then PString $ resugarString e
      else PList $ map exampleToDiscoPattern $ resugarList e
  (TI.KNil, _) -> PList []
  (TI.KChar c, _) -> PChar c
  (TI.KLeft, [l]) -> PInj L $ exampleToDiscoPattern l
  (TI.KRight, [r]) -> PInj R $ exampleToDiscoPattern r
  (TI.KLeft, _) -> error "Found KLeft data constructor with 0 or multiple arguments"
  (TI.KRight, _) -> error "Found KRight data constructor with 0 or multiple arguments"

resugarPair :: ExamplePat -> [ExamplePat]
resugarPair e@(ExamplePat TI.DataCon {TI.dcIdent = ident} args) = case (ident, args) of
  (TI.KPair, [efst, esnd]) -> efst : resugarPair esnd
  (_, _) -> [e]

resugarList :: ExamplePat -> [ExamplePat]
resugarList (ExamplePat TI.DataCon {TI.dcIdent = ident} args) = case (ident, args) of
  (TI.KCons, [ehead, etail]) -> ehead : resugarList etail
  (_, _) -> []

resugarString :: ExamplePat -> String
resugarString (ExamplePat TI.DataCon {TI.dcIdent = ident} args) = case (ident, args) of
  (TI.KCons, [ehead, etail]) -> assumeExampleChar ehead : resugarString etail
  (_, _) -> []

assumeExampleChar :: ExamplePat -> Char
assumeExampleChar (ExamplePat TI.DataCon {TI.dcIdent = TI.KChar c} _) = c
assumeExampleChar _ = error "Wrongly assumed that an ExamplePat was a Char"

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

-- | Convert a Disco APattern into a list of Guards which cover that pattern
--
--   These patterns are currently not handled:
--     , APNeg     --still need to handle rational case
--     , APFrac    --required for rationals?
--     algebraic (probably will be eventually replaced anyway):
--     , APAdd
--     , APMul
--     , APSub
--   These (or some updated version of them) may be handled eventually,
--   once updated arithmetic patterns are merged.
--
--   We treat unhandled patterns as if they are exhaustively matched against
--   (aka, they are seen as wildcards by the checker).
--   This necessarily results in some false negatives, but no false positives.
desugarMatch :: (Members '[Fresh, Embed IO] r) => TI.TypedVar -> APattern -> Sem r [Guard]
desugarMatch var pat = case pat of
  (APTup (ta Ty.:*: tb) [pfst, psnd]) -> do
    varFst <- TI.newVar ta
    varSnd <- TI.newVar tb
    guardsFst <- desugarMatch varFst pfst
    guardsSnd <- desugarMatch varSnd psnd
    let guardPair = (var, GMatch (TI.pair ta tb) [varFst, varSnd])
    return $ [guardPair] ++ guardsFst ++ guardsSnd
  (APTup _ sugary) -> desugarMatch var (desugarTuplePats sugary)
  (APCons _ subHead subTail) -> do
    let typeHead = Ty.getType subHead
    let typeTail = Ty.getType subTail
    varHead <- TI.newVar typeHead
    varTail <- TI.newVar typeTail
    guardsHead <- desugarMatch varHead subHead
    guardsTail <- desugarMatch varTail subTail
    let guardCons = (var, GMatch (TI.cons typeHead typeTail) [varHead, varTail])
    return $ [guardCons] ++ guardsHead ++ guardsTail
  -- We have to desugar Lists into Cons and Nils
  (APList _ []) -> return [(var, GMatch TI.nil [])]
  (APList ty (phead : ptail)) -> do
    -- APCons have the type of the list they are part of
    desugarMatch var (APCons ty phead (APList ty ptail))
  -- we have to desugar to a list, becuse we can match strings with cons
  (APString str) -> do
    let strType = Ty.TyList Ty.TyC
    desugarMatch var (APList strType (map APChar str))
  -- A bit of strangeness is required here because of how patterns work
  (APNat Ty.TyN nat) -> return [(var, GMatch (TI.natural nat) [])]
  (APNat Ty.TyZ z) -> return [(var, GMatch (TI.integer z) [])]
  (APNeg Ty.TyZ (APNat Ty.TyN z)) -> return [(var, GMatch (TI.integer (-z)) [])]
  -- These are more straightforward:
  (APWild _) -> return []
  (APVar ty name) -> do
    let newAlias = TI.TypedVar (name, ty)
    return [(newAlias, GWasOriginally var)]
  APUnit -> return [(var, GMatch TI.unit [])]
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
    return $ (var, GMatch dc [newVar]) : guards
  _ -> return []

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
  Guarded (x, GMatch dc args) t -> do
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
    Just cfs -> do
      ns' <- addLitMulti ns lit
      return $ cfs : ns'

addLiteral :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => C.NormRefType -> Literal -> MaybeT (Sem r) C.NormRefType
addLiteral constraints (Literal (x, c)) = case c of
  LitWasOriginally z ->
    constraints `C.addConstraint` (x, C.CWasOriginally z)
  LitMatch dc args ->
    constraints `C.addConstraint` (x, C.CMatch dc args)
  LitNot dc ->
    constraints `C.addConstraint` (x, C.CNot dc)

data InhabPat where
  IPIs :: TI.DataCon -> [InhabPat] -> InhabPat
  IPNot :: [TI.DataCon] -> InhabPat
  deriving (Show, Eq, Ord)

-- Sanity check: are we giving the dataconstructor the
-- correct number of arguments?
mkIPMatch :: TI.DataCon -> [InhabPat] -> InhabPat
mkIPMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else IPIs k pats

findInhabitants :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findInhabitants nrefs args = do
  a <- forM nrefs (`findAllForNref` args)
  return $ Poss.anyOf a

findAllForNref :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => C.NormRefType -> [TI.TypedVar] -> Sem r (Poss.Possibilities [InhabPat])
findAllForNref nref args = do
  argPats <- forM args (`findVarInhabitants` nref)
  return $ Poss.allCombinations argPats

findVarInhabitants :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => TI.TypedVar -> C.NormRefType -> Sem r (Poss.Possibilities InhabPat)
findVarInhabitants var cns =
  case posMatch of
    Just (k, args) -> do
      argPossibilities <- findAllForNref cns args
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
                    runMaybeT (cns `C.addConstraint` (var, C.CMatch dc vars))

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

data ExamplePat where
  ExamplePat :: TI.DataCon -> [ExamplePat] -> ExamplePat
  deriving (Show)

-- | Less general version of the above inhabitant finding function
--   returns a maximum of 3 possible args lists that haven't been matched against,
--   as to not overwhelm new users of the language.
--   This is essentially a DFS, and it has a bad habit of
--   trying to build infinite lists whenever it can, so we give it a max depth of 32
--   If we reach 32 levels of nested dataconstructors in this language,
--   it is pretty safe to assume we were chasing after an infinite structure
findPosExamples :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => [C.NormRefType] -> [TI.TypedVar] -> Sem r [[ExamplePat]]
findPosExamples nrefs args = do
  a <- forM nrefs (\nref -> findAllPosForNref 32 nref args)
  return $ take 3 $ Poss.getPossibilities $ Poss.anyOf a

findAllPosForNref :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => Int -> C.NormRefType -> [TI.TypedVar] -> Sem r (Poss.Possibilities [ExamplePat])
findAllPosForNref fuel nref args = do
  argPats <- forM args (\arg -> findVarPosExamples (fuel - 1) arg nref)
  return $ Poss.allCombinations argPats

findVarPosExamples :: (Members '[Fresh, Reader Ty.TyDefCtx] r) => Int -> TI.TypedVar -> C.NormRefType -> Sem r (Poss.Possibilities ExamplePat)
findVarPosExamples fuel var cns =
  if fuel < 0
    then return mempty
    else case posMatch of
      Just (k, args) -> do
        argPossibilities <- findAllPosForNref (fuel - 1) cns args
        return (mkExampleMatch k <$> argPossibilities)
      Nothing -> do
        tyCtx <- ask @Ty.TyDefCtx
        let dcs = getPosFrom (TI.getType var) tyCtx negMatches
        let tryAddDc dc = do
              vars <- TI.newVars (TI.dcTypes dc)
              runMaybeT (cns `C.addConstraint` (var, C.CMatch dc vars))
        -- Try to add a positive constraint for each data constructor
        -- to the current nref
        -- If any of these additions succeed, save that nref,
        -- it now has positive information
        posNrefs <- catMaybes <$> forM dcs tryAddDc

        Poss.anyOf <$> forM posNrefs (findVarPosExamples (fuel - 1) var)
  where
    constraintsOnX = C.onVar var cns
    posMatch = C.posMatch constraintsOnX
    negMatches = C.negMatches constraintsOnX

getPosFrom :: Ty.Type -> Ty.TyDefCtx -> [TI.DataCon] -> [TI.DataCon]
getPosFrom ty ctx neg = take 3 $ filter (`notElem` neg) dcs
  where
    dcs = case TI.tyDataCons ty ctx of
      TI.Finite dcs' -> dcs'
      TI.Infinite dcs' -> dcs'

mkExampleMatch :: TI.DataCon -> [ExamplePat] -> ExamplePat
mkExampleMatch k pats =
  if length (TI.dcTypes k) /= length pats
    then error $ "Wrong number of DataCon args" ++ show (k, pats)
    else ExamplePat k pats

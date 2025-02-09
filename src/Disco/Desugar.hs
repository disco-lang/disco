{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-x-data-list-nonempty-unzip #-}

{-# HLINT ignore "Functor law" #-}

-- |
-- Module      :  Disco.Desugar
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Desugaring the typechecked surface language to a (still typed)
-- simpler language.
module Disco.Desugar (
  -- * Running desugaring computations
  runDesugar,

  -- * Programs, terms, and properties
  desugarDefn,
  desugarTerm,
  desugarProperty,

  -- * Case expressions and patterns
  desugarBranch,
  desugarGuards,
)
where

import Control.Monad (zipWithM)
import Data.Bool (bool)
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import Disco.AST.Desugared
import Disco.AST.Surface
import Disco.AST.Typed
import Disco.Effects.Fresh
import Disco.Module
import Disco.Names
import Disco.Syntax.Operators
import Disco.Syntax.Prims
import Disco.Typecheck (containerTy)
import Disco.Types
import Polysemy (Member, Sem, run)
import Unbound.Generics.LocallyNameless (
  Bind,
  Name,
  bind,
  embed,
  name2String,
  string2Name,
  unembed,
  unrebind,
 )
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

------------------------------------------------------------
-- Running desugaring computations
------------------------------------------------------------

-- | Run a desugaring computation.
runDesugar :: Sem '[Fresh] a -> a
runDesugar = run . runFresh1

-- Using runFresh1 is a bit of a hack; that way we won't
-- ever pick a name with #0 (which is what is generated by default
-- by string2Name), hence won't conflict with any existing free
-- variables which came from the parser.

------------------------------------------------------------
-- ATerm DSL
------------------------------------------------------------

-- A tiny DSL for building certain ATerms, which is helpful for
-- writing desugaring rules.

-- Make a local ATVar.
atVar :: Type -> Name ATerm -> ATerm
atVar ty x = ATVar ty (QName LocalName x)

tapp :: ATerm -> ATerm -> ATerm
tapp t1 t2 = ATApp resTy t1 t2
 where
  resTy = case getType t1 of
    (_ :->: r) -> r
    ty -> error $ "Impossible! Got non-function type " ++ show ty ++ " in tapp"

mkBin :: Type -> BOp -> ATerm -> ATerm -> ATerm
mkBin resTy bop t1 t2 =
  tapp (ATPrim (getType t1 :*: getType t2 :->: resTy) (PrimBOp bop)) (mkPair t1 t2)

mkUn :: Type -> UOp -> ATerm -> ATerm
mkUn resTy uop t = tapp (ATPrim (getType t :->: resTy) (PrimUOp uop)) t

mkPair :: ATerm -> ATerm -> ATerm
mkPair t1 t2 = mkTup [t1, t2]

mkTup :: [ATerm] -> ATerm
mkTup ts = ATTup (foldr1 (:*:) (map getType ts)) ts

tapps :: ATerm -> [ATerm] -> ATerm
tapps t ts = tapp t (mkTup ts)

infixr 2 ||.
(||.) :: ATerm -> ATerm -> ATerm
(||.) = mkBin TyBool Or

infixl 6 -., +.
(-.) :: ATerm -> ATerm -> ATerm
at1 -. at2 = mkBin (getType at1) Sub at1 at2

(+.) :: ATerm -> ATerm -> ATerm
at1 +. at2 = mkBin (getType at1) Add at1 at2

infixl 7 /.
(/.) :: ATerm -> ATerm -> ATerm
at1 /. at2 = mkBin (getType at1) Div at1 at2

infix 4 <., >=.
(<.) :: ATerm -> ATerm -> ATerm
(<.) = mkBin TyBool Lt

(>=.) :: ATerm -> ATerm -> ATerm
(>=.) = mkBin TyBool Geq

(|.) :: ATerm -> ATerm -> ATerm
(|.) = mkBin TyBool Divides

infix 4 ==.
(==.) :: ATerm -> ATerm -> ATerm
(==.) = mkBin TyBool Eq

tnot :: ATerm -> ATerm
tnot = tapp (ATPrim (TyBool :->: TyBool) (PrimUOp Not))

(<==.) :: ATerm -> [AGuard] -> ABranch
t <==. gs = bind (toTelescope gs) t

fls :: ATerm
fls = ATBool TyBool False

tru :: ATerm
tru = ATBool TyBool True

tif :: ATerm -> AGuard
tif t = AGBool (embed t)

ctrNil :: Container -> Type -> ATerm
ctrNil ctr ty = ATContainer (containerTy ctr ty) ctr [] Nothing

ctrSingleton :: Container -> ATerm -> ATerm
ctrSingleton ctr t = ATContainer (containerTy ctr (getType t)) ctr [(t, Nothing)] Nothing

------------------------------------------------------------
-- Making DTerms
------------------------------------------------------------

dtVar :: Type -> Name DTerm -> DTerm
dtVar ty x = DTVar ty (QName LocalName x)

dtapp :: DTerm -> DTerm -> DTerm
dtapp t1 t2 = DTApp resTy t1 t2
 where
  resTy = case getType t1 of
    (_ :->: r) -> r
    ty -> error $ "Impossible! Got non-function type " ++ show ty ++ " in dtapp"

dtbin :: Type -> Prim -> DTerm -> DTerm -> DTerm
dtbin resTy p dt1 dt2 =
  dtapp (DTPrim (getType dt1 :*: getType dt2 :->: resTy) p) (mkDTPair dt1 dt2)

mkDTPair :: DTerm -> DTerm -> DTerm
mkDTPair dt1 dt2 = DTPair (getType dt1 :*: getType dt2) dt1 dt2

------------------------------------------------------------
-- Definition desugaring
------------------------------------------------------------

-- | Desugar a definition (consisting of a collection of pattern
--   clauses with bodies) into a core language term.
desugarDefn :: Member Fresh r => Defn -> Sem r DTerm
desugarDefn (Defn _ patTys bodyTy def) =
  desugarAbs Lam (foldr (:->:) bodyTy patTys) def

------------------------------------------------------------
-- Abstraction desugaring
------------------------------------------------------------

-- | Desugar an abstraction -- that is, a collection of clauses
--   with their corresponding patterns. Definitions are abstractions
--   (which happen to be named), and source-level lambdas are also
--   abstractions (which happen to have only one clause).
desugarAbs :: Member Fresh r => Quantifier -> Type -> NonEmpty Clause -> Sem r DTerm
-- Special case for compiling a single lambda with no pattern matching directly to a lambda
desugarAbs Lam ty (cl@(unsafeUnbind -> ([APVar _ _], _)) :| []) = do
  (ps, at) <- unbind cl
  case ps of
    [APVar _ x] -> do
      d <- desugarTerm at
      return $ DTAbs Lam ty (bind (coerce x) d)
    _ -> error "desugarAbs: impossible: ps must be a singleton APVar"
-- General case
desugarAbs quant overallTy body = do
  clausePairs <- unbindClauses body
  let (pats, bodies) = NE.unzip clausePairs
  let patTys = map getType (NE.head pats)
  let bodyTy = getType (NE.head bodies)

  -- generate dummy variables for lambdas
  args <- zipWithM (\_ i -> fresh (string2Name ("arg" ++ show i))) (NE.head pats) [0 :: Int ..]

  -- Create lambdas and one big case.  Recursively desugar the case to
  -- deal with arithmetic patterns.
  let branches = NE.zipWith (mkBranch (zip args patTys)) bodies pats
  dcase <- desugarTerm $ ATCase bodyTy (NE.toList branches)
  return $ mkAbs quant overallTy patTys (coerce args) dcase
 where
  mkBranch :: [(Name ATerm, Type)] -> ATerm -> [APattern] -> ABranch
  mkBranch xs b ps = bind (mkGuards xs ps) b

  mkGuards :: [(Name ATerm, Type)] -> [APattern] -> Telescope AGuard
  mkGuards xs ps = toTelescope $ zipWith AGPat (map (\(x, ty) -> embed (atVar ty x)) xs) ps

  -- To make searches fairer, we lift up directly nested abstractions
  -- with the same quantifier when there's only a single clause. That
  -- way, we generate a chain of abstractions followed by a case, instead
  -- of a bunch of alternating abstractions and cases.
  unbindClauses :: Member Fresh r => NonEmpty Clause -> Sem r (NonEmpty ([APattern], ATerm))
  unbindClauses (c :| []) | quant `elem` [All, Ex] = do
    (ps, t) <- liftClause c
    return ((ps, addDbgInfo ps t) :| [])
  unbindClauses cs = mapM unbind cs

  liftClause :: Member Fresh r => Bind [APattern] ATerm -> Sem r ([APattern], ATerm)
  liftClause c =
    unbind c >>= \case
      (ps, ATAbs q _ c') | q == quant -> do
        (ps', b) <- liftClause c'
        return (ps ++ ps', b)
      (ps, b) -> return (ps, b)

  -- Wrap a term in a test frame to report the values of all variables
  -- bound in the patterns.
  addDbgInfo :: [APattern] -> ATerm -> ATerm
  addDbgInfo ps = ATTest (map withName $ concatMap varsBound ps)
   where
    withName (n, ty) = (name2String n, ty, n)

------------------------------------------------------------
-- Term desugaring
------------------------------------------------------------

-- | Desugar the application of a "bag from counts" primitive on a
--   list, either a safe or unsafe variant.
desugarCList2B :: Member Fresh r => Prim -> Type -> Type -> Type -> Sem r DTerm
desugarCList2B p ty cts b = do
  c <- fresh (string2Name "c")
  body <-
    desugarTerm $
      tapp
        (ATPrim (TyBag cts :->: TyBag b) p)
        ( tapp
            (ATPrim (TyList cts :->: TyBag cts) PrimBag)
            (atVar (TyList cts) c)
        )
  return $ mkLambda ty [c] body

-- | Desugar the @min@ and @max@ functions into conditional expressions.
desugarMinMax :: Member Fresh r => Prim -> Type -> Sem r DTerm
desugarMinMax m ty = do
  p <- fresh (string2Name "p")
  a <- fresh (string2Name "a")
  b <- fresh (string2Name "b")
  body <-
    desugarTerm $
      ATCase
        ty
        [ bind
            (toTelescope [AGPat (embed (atVar ty p)) (APTup (ty :*: ty) [APVar ty a, APVar ty b])])
            $ ATCase
              ty
              [ atVar ty (if m == PrimMin then a else b) <==. [tif (atVar ty a <. atVar ty b)]
              , atVar ty (if m == PrimMin then b else a) <==. []
              ]
        ]
  return $ mkLambda ((ty :*: ty) :->: ty) [p] body

-- | Desugar a typechecked term.
desugarTerm :: Member Fresh r => ATerm -> Sem r DTerm
desugarTerm (ATVar ty x) = return $ DTVar ty (coerce x)
desugarTerm (ATPrim (ty1 :->: resTy) (PrimUOp uop))
  | uopDesugars ty1 resTy uop = desugarPrimUOp ty1 resTy uop
desugarTerm (ATPrim (ty1 :*: ty2 :->: resTy) (PrimBOp bop))
  | bopDesugars ty1 ty2 resTy bop = desugarPrimBOp ty1 ty2 resTy bop
desugarTerm (ATPrim ty@(TyList cts :->: TyBag b) PrimC2B) = desugarCList2B PrimC2B ty cts b
desugarTerm (ATPrim ty@(TyList cts :->: TyBag b) PrimUC2B) = desugarCList2B PrimUC2B ty cts b
desugarTerm (ATPrim (_ :->: ty) PrimMin) = desugarMinMax PrimMin ty
desugarTerm (ATPrim (_ :->: ty) PrimMax) = desugarMinMax PrimMax ty
desugarTerm (ATPrim ty x) = return $ DTPrim ty x
desugarTerm ATUnit = return DTUnit
desugarTerm (ATBool ty b) = return $ DTBool ty b
desugarTerm (ATChar c) = return $ DTChar c
desugarTerm (ATString cs) =
  desugarContainer (TyList TyC) ListContainer (map (\c -> (ATChar c, Nothing)) cs) Nothing
desugarTerm (ATAbs q ty lam) = desugarAbs q ty (NE.singleton lam)
-- Special cases for fully applied operators
desugarTerm (ATApp resTy (ATPrim _ (PrimUOp uop)) t)
  | uopDesugars (getType t) resTy uop = desugarUnApp resTy uop t
desugarTerm (ATApp resTy (ATPrim _ (PrimBOp bop)) (ATTup _ [t1, t2]))
  | bopDesugars (getType t1) (getType t2) resTy bop = desugarBinApp resTy bop t1 t2
desugarTerm (ATApp ty t1 t2) =
  DTApp ty <$> desugarTerm t1 <*> desugarTerm t2
desugarTerm (ATTup ty ts) = desugarTuples ty ts
desugarTerm (ATNat ty n) = return $ DTNat ty n
desugarTerm (ATRat r) = return $ DTRat r
desugarTerm (ATTyOp ty op t) = return $ DTTyOp ty op t
desugarTerm (ATChain _ t1 links) = desugarTerm $ expandChain t1 links
desugarTerm (ATContainer ty c es mell) = desugarContainer ty c es mell
desugarTerm (ATContainerComp _ ctr bqt) = do
  (qs, t) <- unbind bqt
  desugarComp ctr t qs
desugarTerm (ATLet _ t) = do
  (bs, t2) <- unbind t
  desugarLet (fromTelescope bs) t2
desugarTerm (ATCase ty bs) = DTCase ty <$> mapM desugarBranch bs
desugarTerm (ATTest info t) = DTTest (coerce info) <$> desugarTerm t

-- | Desugar a property by wrapping its corresponding term in a test
--   frame to catch its exceptions & convert booleans to props.
desugarProperty :: Member Fresh r => AProperty -> Sem r DTerm
desugarProperty p = DTTest [] <$> desugarTerm (setType TyProp p)

------------------------------------------------------------
-- Desugaring operators
------------------------------------------------------------

-- | Test whether a given unary operator is one that needs to be
--   desugared, given the type of the argument and result.
uopDesugars :: Type -> Type -> UOp -> Bool
-- uopDesugars _ (TyFin _) Neg = True
uopDesugars TyProp TyProp Not = False
uopDesugars _ _ uop = uop == Not

desugarPrimUOp :: Member Fresh r => Type -> Type -> UOp -> Sem r DTerm
desugarPrimUOp argTy resTy op = do
  x <- fresh (string2Name "arg")
  body <- desugarUnApp resTy op (atVar argTy x)
  return $ mkLambda (argTy :->: resTy) [x] body

-- | Test whether a given binary operator is one that needs to be
--   desugared, given the two types of the arguments and the type of the result.
bopDesugars :: Type -> Type -> Type -> BOp -> Bool
bopDesugars _ TyN _ Choose = True
-- bopDesugars _   _   (TyFin _) bop | bop `elem` [Add, Mul] = True

-- Eq and Lt at type Prop desugar into ShouldEq, ShouldLt .
bopDesugars _ _ TyProp bop | bop `elem` [Eq, Neq, Lt, Gt, Leq, Geq, Divides] = True
bopDesugars _ _ _ bop =
  bop
    `elem` [ And
           , Or
           , Impl
           , Iff
           , Neq
           , Gt
           , Leq
           , Geq
           , IDiv
           , Sub
           , SSub
           , Inter
           , Diff
           , Union
           , Subset
           ]

-- | Desugar a primitive binary operator at the given type.
desugarPrimBOp :: Member Fresh r => Type -> Type -> Type -> BOp -> Sem r DTerm
desugarPrimBOp ty1 ty2 resTy op = do
  p <- fresh (string2Name "pair1")
  x <- fresh (string2Name "arg1")
  y <- fresh (string2Name "arg2")
  let argsTy = ty1 :*: ty2
  body <- desugarBinApp resTy op (atVar ty1 x) (atVar ty2 y)
  return $
    mkLambda (argsTy :->: resTy) [p] $
      DTCase
        resTy
        [ bind
            (toTelescope [DGPat (embed (dtVar argsTy (coerce p))) (DPPair argsTy (coerce x) (coerce y))])
            body
        ]

-- | Desugar a saturated application of a unary operator.
--   The first argument is the type of the result.
desugarUnApp :: Member Fresh r => Type -> UOp -> ATerm -> Sem r DTerm
-- Desugar negation on TyFin to a negation on TyZ followed by a mod.
-- See the comments below re: Add and Mul on TyFin.
-- desugarUnApp (TyFin n) Neg t =
--   desugarTerm $ mkBin (TyFin n) Mod (mkUn TyZ Neg t) (ATNat TyN n)

-- XXX This should be turned into a standard library definition.
-- not t ==> {? false if t, true otherwise ?}
desugarUnApp _ Not t =
  desugarTerm $
    ATCase
      TyBool
      [ fls <==. [AGBool (embed t)]
      , tru <==. []
      ]
desugarUnApp ty uop t = error $ "Impossible! desugarUnApp " ++ show ty ++ " " ++ show uop ++ " " ++ show t

-- | Desugar a saturated application of a binary operator.
--   The first argument is the type of the result.
desugarBinApp :: Member Fresh r => Type -> BOp -> ATerm -> ATerm -> Sem r DTerm
-- Implies, and, or should all be turned into a standard library
-- definition.  This will require first (1) adding support for
-- modules/a standard library, including (2) the ability to define
-- infix operators.

-- And, Or, Impl at type Prop are primitive Prop constructors so don't
-- desugar; but we push the Prop type down so we properly desugar
-- their arguments.
desugarBinApp TyProp b t1 t2 | b `elem` [And, Or, Impl] = do
  d1 <- desugarTerm $ setType TyProp t1
  d2 <- desugarTerm $ setType TyProp t2
  pure $ dtbin TyProp (PrimBOp b) d1 d2

-- t1 and t2 ==> {? t2 if t1, false otherwise ?}
desugarBinApp _ And t1 t2 =
  desugarTerm $
    ATCase
      TyBool
      [ t2 <==. [tif t1]
      , fls <==. []
      ]
-- (t1 implies t2) ==> (not t1 or t2)
desugarBinApp _ Impl t1 t2 = desugarTerm $ tnot t1 ||. t2
-- (t1 iff t2) ==> (t1 == t2)
desugarBinApp _ Iff t1 t2 = desugarTerm $ t1 ==. t2
-- t1 or t2 ==> {? true if t1, t2 otherwise ?})
desugarBinApp _ Or t1 t2 =
  desugarTerm $
    ATCase
      TyBool
      [ tru <==. [tif t1]
      , t2 <==. []
      ]
desugarBinApp TyProp op t1 t2
  | op `elem` [Eq, Neq, Lt, Gt, Leq, Geq, Divides] = desugarTerm $ mkBin TyProp (Should op) t1 t2
desugarBinApp _ Neq t1 t2 = desugarTerm $ tnot (t1 ==. t2)
desugarBinApp _ Gt t1 t2 = desugarTerm $ t2 <. t1
desugarBinApp _ Leq t1 t2 = desugarTerm $ tnot (t2 <. t1)
desugarBinApp _ Geq t1 t2 = desugarTerm $ tnot (t1 <. t2)
-- t1 // t2 ==> floor (t1 / t2)
desugarBinApp resTy IDiv t1 t2 =
  desugarTerm $
    ATApp resTy (ATPrim (getType t1 :->: resTy) PrimFloor) (mkBin (getType t1) Div t1 t2)
-- Desugar normal binomial coefficient (n choose k) to a multinomial
-- coefficient with a singleton list, (n choose [k]).
-- Note this will only be called when (getType t2 == TyN); see bopDesugars.
desugarBinApp _ Choose t1 t2 =
  desugarTerm $ mkBin TyN Choose t1 (ctrSingleton ListContainer t2)
desugarBinApp ty Sub t1 t2 = desugarTerm $ mkBin ty Add t1 (mkUn ty Neg t2)
desugarBinApp ty SSub t1 t2 =
  desugarTerm $
    -- t1 -. t2 ==> {? 0 if t1 < t2, t1 - t2 otherwise ?}
    ATCase
      ty
      [ ATNat ty 0 <==. [tif (t1 <. t2)]
      , mkBin ty Sub t1 t2 <==. []
      -- NOTE, the above is slightly bogus since the whole point of SSub is
      -- because we can't subtract naturals.  However, this will
      -- immediately desugar to a DTerm.  When we write a linting
      -- typechecker for DTerms we should allow subtraction on TyN!
      ]
-- Addition and multiplication on TyFin just desugar to the operation
-- followed by a call to mod.
-- desugarBinApp (TyFin n) op t1 t2
--   | op `elem` [Add, Mul] = desugarTerm $
--       mkBin (TyFin n) Mod
--         (mkBin TyN op t1 t2)
--         (ATNat TyN n)
-- Note the typing of this is a bit funny: t1 and t2 presumably
-- have type (TyFin n), and now we are saying that applying 'op'
-- to them results in TyN, then applying 'mod' results in a TyFin
-- n again.  Using TyN as the intermediate result is necessary so
-- we don't fall into an infinite desugaring loop, and intuitively
-- makes sense because the idea is that we first do the operation
-- as a normal operation in "natural land" and then do a mod.
--
-- We will have to think carefully about how the linting
-- typechecker for DTerms should treat TyN and TyFin.  Probably
-- something like this will work: TyFin is a subtype of TyN, and
-- TyN can be turned into TyFin with mod.  (We don't want such
-- typing rules in the surface disco language itself because
-- implicit coercions from TyFin -> N don't commute with
-- operations like addition and multiplication, e.g. 3+3 yields 1
-- if we add them in Z5 and then coerce to Nat, but 6 if we first
-- coerce both and then add.

-- Intersection, difference, and union all desugar to an application
-- of 'merge' with an appropriate combining operation.
desugarBinApp ty op t1 t2
  | op `elem` [Inter, Diff, Union] =
      desugarTerm $
        tapps
          (ATPrim ((TyN :*: TyN :->: TyN) :*: ty :*: ty :->: ty) PrimMerge)
          [ ATPrim (TyN :*: TyN :->: TyN) (mergeOp ty op)
          , t1
          , t2
          ]
 where
  mergeOp _ Inter = PrimMin
  mergeOp _ Diff = PrimBOp SSub
  mergeOp (TySet _) Union = PrimMax
  mergeOp (TyBag _) Union = PrimBOp Add
  mergeOp _ _ = error $ "Impossible! mergeOp " ++ show ty ++ " " ++ show op

-- A ⊆ B  <==>  (A ⊔ B = B)
--   where ⊔ denotes 'merge max'.
--   Note it is NOT union, since this doesn't work for bags.
--   e.g.  bag [1] union bag [1,2] =  bag [1,1,2] /= bag [1,2].
desugarBinApp _ Subset t1 t2 =
  desugarTerm $
    tapps
      (ATPrim (ty :*: ty :->: TyBool) (PrimBOp Eq))
      [ tapps
          (ATPrim ((TyN :*: TyN :->: TyN) :*: ty :*: ty :->: ty) PrimMerge)
          [ ATPrim (TyN :*: TyN :->: TyN) PrimMax
          , t1
          , t2
          ]
      , t2 -- XXX sharing
      ]
 where
  ty = getType t1
desugarBinApp ty bop t1 t2 = error $ "Impossible! desugarBinApp " ++ show ty ++ " " ++ show bop ++ " " ++ show t1 ++ " " ++ show t2

------------------------------------------------------------
-- Desugaring other stuff
------------------------------------------------------------

-- | Desugar a container comprehension.  First translate it into an
--   expanded ATerm and then recursively desugar that.
desugarComp :: Member Fresh r => Container -> ATerm -> Telescope AQual -> Sem r DTerm
desugarComp ctr t qs = expandComp ctr t qs >>= desugarTerm

-- | Expand a container comprehension into an equivalent ATerm.
expandComp :: Member Fresh r => Container -> ATerm -> Telescope AQual -> Sem r ATerm
-- [ t | ] = [ t ]
expandComp ctr t TelEmpty = return $ ctrSingleton ctr t
-- [ t | q, qs ] = ...
expandComp ctr t (TelCons (unrebind -> (q, qs))) =
  case q of
    -- [ t | x in l, qs ] = join (map (\x -> [t | qs]) l)
    AQBind x (unembed -> lst) -> do
      tqs <- expandComp ctr t qs
      let c = containerTy ctr
          tTy = getType t
          xTy = case getType lst of
            TyContainer _ e -> e
            _ -> error "Impossible! Not a container in expandComp"
          joinTy = c (c tTy) :->: c tTy
          mapTy = (xTy :->: c tTy) :*: c xTy :->: c (c tTy)
      return $
        tapp (ATPrim joinTy PrimJoin) $
          tapp
            (ATPrim mapTy PrimEach)
            ( mkPair
                (ATAbs Lam (xTy :->: c tTy) (bind [APVar xTy x] tqs))
                lst
            )

    -- [ t | g, qs ] = if g then [ t | qs ] else []
    AQGuard (unembed -> g) -> do
      tqs <- expandComp ctr t qs
      return $
        ATCase
          (containerTy ctr (getType t))
          [ tqs <==. [tif g]
          , ctrNil ctr (getType t) <==. []
          ]

-- | Desugar a let into applications of a chain of nested lambdas.
--   /e.g./
--
--     @let x = s, y = t in q@
--
--   desugars to
--
--     @(\x. (\y. q) t) s@
desugarLet :: Member Fresh r => [ABinding] -> ATerm -> Sem r DTerm
desugarLet [] t = desugarTerm t
desugarLet ((ABinding _ x (unembed -> t1)) : ls) t =
  dtapp
    <$> ( DTAbs Lam (getType t1 :->: getType t)
            <$> (bind (coerce x) <$> desugarLet ls t)
        )
    <*> desugarTerm t1

-- | Desugar a lambda from a list of argument names and types and the
--   desugared @DTerm@ expression for its body. It will be desugared
--   to a chain of one-argument lambdas. /e.g./
--
--     @\x y z. q@
--
--   desugars to
--
--     @\x. \y. \z. q@
mkLambda :: Type -> [Name ATerm] -> DTerm -> DTerm
mkLambda funty args c = go funty args
 where
  go _ [] = c
  go ty@(_ :->: ty2) (x : xs) = DTAbs Lam ty (bind (coerce x) (go ty2 xs))
  go ty as = error $ "Impossible! mkLambda.go " ++ show ty ++ " " ++ show as

mkQuant :: Quantifier -> [Type] -> [Name ATerm] -> DTerm -> DTerm
mkQuant q argtys args c = foldr quantify c (zip args argtys)
 where
  quantify (x, ty) body = DTAbs q ty (bind (coerce x) body)

mkAbs :: Quantifier -> Type -> [Type] -> [Name ATerm] -> DTerm -> DTerm
mkAbs Lam funty _ args c = mkLambda funty args c
mkAbs q _ argtys args c = mkQuant q argtys args c

-- | Desugar a tuple to nested pairs, /e.g./ @(a,b,c,d) ==> (a,(b,(c,d)))@.a
desugarTuples :: Member Fresh r => Type -> [ATerm] -> Sem r DTerm
desugarTuples _ [t] = desugarTerm t
desugarTuples ty@(_ :*: ty2) (t : ts) = DTPair ty <$> desugarTerm t <*> desugarTuples ty2 ts
desugarTuples ty ats =
  error $ "Impossible! desugarTuples " ++ show ty ++ " " ++ show ats

-- | Expand a chain of comparisons into a sequence of binary
--   comparisons combined with @and@.  Note we only expand it into
--   another 'ATerm' (which will be recursively desugared), because
--   @and@ itself also gets desugared.
--
--   For example, @a < b <= c > d@ becomes @a < b and b <= c and c > d@.
expandChain :: ATerm -> [ALink] -> ATerm
expandChain _ [] = error "Can't happen! expandChain _ []"
expandChain t1 [ATLink op t2] = mkBin TyBool op t1 t2
expandChain t1 (ATLink op t2 : links) =
  mkBin
    TyBool
    And
    (mkBin TyBool op t1 t2)
    (expandChain t2 links)

-- | Desugar a branch of a case expression.
desugarBranch :: Member Fresh r => ABranch -> Sem r DBranch
desugarBranch b = do
  (ags, at) <- unbind b
  dgs <- desugarGuards ags
  d <- desugarTerm at
  return $ bind dgs d

-- | Desugar the list of guards in one branch of a case expression.
--   Pattern guards essentially remain as they are; boolean guards get
--   turned into pattern guards which match against @true@.
desugarGuards :: Member Fresh r => Telescope AGuard -> Sem r (Telescope DGuard)
desugarGuards = fmap (toTelescope . concat) . mapM desugarGuard . fromTelescope
 where
  desugarGuard :: Member Fresh r => AGuard -> Sem r [DGuard]

  -- A Boolean guard is desugared to a pattern-match on @true = right(unit)@.
  desugarGuard (AGBool (unembed -> at)) = do
    dt <- desugarTerm at
    desugarMatch dt (APInj TyBool R APUnit)

  -- 'let x = t' is desugared to 'when t is x'.
  desugarGuard (AGLet (ABinding _ x (unembed -> at))) = do
    dt <- desugarTerm at
    varMatch dt (coerce x)

  -- Desugaring 'when t is p' is the most complex case; we have to
  -- break down the pattern and match it incrementally.
  desugarGuard (AGPat (unembed -> at) p) = do
    dt <- desugarTerm at
    desugarMatch dt p

  -- Desugar a guard of the form 'when dt is p'.  An entire match is
  -- the right unit to desugar --- as opposed to, say, writing a
  -- function to desugar a pattern --- since a match may desugar to
  -- multiple matches, and on recursive calls we need to know what
  -- term/variable should be bound to the pattern.
  --
  -- A match may desugar to multiple matches for two reasons:
  --
  --   1. Nested patterns 'explode' into a 'telescope' matching one
  --   constructor at a time, for example, 'when t is (x,y,3)'
  --   becomes 'when t is (x,x0) when x0 is (y,x1) when x1 is 3'.
  --   This makes the order of matching explicit and enables lazy
  --   matching without requiring special support from the
  --   interpreter other than WHNF reduction.
  --
  --   2. Matches against arithmetic patterns desugar to a
  --   combination of matching, computation, and boolean checks.
  --   For example, 'when t is (y+1)' becomes 'when t is x0 if x0 >=
  --   1 let y = x0-1'.
  desugarMatch :: Member Fresh r => DTerm -> APattern -> Sem r [DGuard]
  desugarMatch dt (APVar ty x) = mkMatch dt (DPVar ty (coerce x))
  desugarMatch _ (APWild _) = return []
  desugarMatch dt APUnit = mkMatch dt DPUnit
  desugarMatch dt (APBool b) = desugarMatch dt (APInj TyBool (bool L R b) APUnit)
  desugarMatch dt (APNat ty n) = desugarMatch (dtbin TyBool (PrimBOp Eq) dt (DTNat ty n)) (APBool True)
  desugarMatch dt (APChar c) = desugarMatch (dtbin TyBool (PrimBOp Eq) dt (DTChar c)) (APBool True)
  desugarMatch dt (APString s) = desugarMatch dt (APList (TyList TyC) (map APChar s))
  desugarMatch dt (APTup tupTy pat) = desugarTuplePats tupTy dt pat
   where
    desugarTuplePats :: Member Fresh r => Type -> DTerm -> [APattern] -> Sem r [DGuard]
    desugarTuplePats _ _ [] = error "Impossible! desugarTuplePats []"
    desugarTuplePats _ t [p] = desugarMatch t p
    desugarTuplePats ty@(_ :*: ty2) t (p : ps) = do
      (x1, gs1) <- varForPat p
      (x2, gs2) <- case ps of
        [APVar _ px2] -> return (coerce px2, [])
        _ -> do
          x <- fresh (string2Name "x")
          (x,) <$> desugarTuplePats ty2 (dtVar ty2 x) ps
      fmap concat . sequence $
        [ mkMatch t $ DPPair ty x1 x2
        , return gs1
        , return gs2
        ]
    desugarTuplePats ty _ _ =
      error $ "Impossible! desugarTuplePats with non-pair type " ++ show ty
  desugarMatch dt (APInj ty s p) = do
    (x, gs) <- varForPat p
    fmap concat . sequence $
      [ mkMatch dt $ DPInj ty s x
      , return gs
      ]
  desugarMatch dt (APCons ty p1 p2) = do
    y <- fresh (string2Name "y")
    (x1, gs1) <- varForPat p1
    (x2, gs2) <- varForPat p2

    let eltTy = getType p1
        unrolledTy = eltTy :*: ty
    fmap concat . sequence $
      [ mkMatch dt (DPInj ty R y)
      , mkMatch (dtVar unrolledTy y) (DPPair unrolledTy x1 x2)
      , return gs1
      , return gs2
      ]
  desugarMatch dt (APList ty []) = desugarMatch dt (APInj ty L APUnit)
  desugarMatch dt (APList ty ps) =
    desugarMatch dt $ foldr (APCons ty) (APList ty []) ps
  -- when dt is (p + t) ==> when dt is x0; let v = t; [if x0 >= v]; when x0-v is p
  desugarMatch dt (APAdd ty _ p t) = arithBinMatch posRestrict (-.) dt ty p t
   where
    posRestrict plusty
      | plusty `elem` [TyN, TyF] = Just (>=.)
      | otherwise = Nothing

  -- when dt is (p * t) ==> when dt is x0; let v = t; [if v divides x0]; when x0 / v is p
  desugarMatch dt (APMul ty _ p t) = arithBinMatch intRestrict (/.) dt ty p t
   where
    intRestrict plusty
      | plusty `elem` [TyN, TyZ] = Just (flip (|.))
      | otherwise = Nothing

  -- when dt is (p - t) ==> when dt is x0; let v = t; when x0 + v is p
  desugarMatch dt (APSub ty p t) = arithBinMatch (const Nothing) (+.) dt ty p t
  -- when dt is (p/q) ==> when $frac(dt) is (p, q)
  desugarMatch dt (APFrac _ p q) =
    desugarMatch
      (dtapp (DTPrim (TyQ :->: TyZ :*: TyN) PrimFrac) dt)
      (APTup (TyZ :*: TyN) [p, q])
  -- when dt is (-p) ==> when dt is x0; if x0 < 0; when -x0 is p
  desugarMatch dt (APNeg ty p) = do
    -- when dt is x0
    (x0, g1) <- varFor dt

    -- if x0 < 0
    g2 <- desugarGuard $ AGBool (embed (atVar ty (coerce x0) <. ATNat ty 0))

    -- when -x0 is p
    neg <- desugarTerm $ mkUn ty Neg (atVar ty (coerce x0))
    g3 <- desugarMatch neg p

    return (g1 ++ g2 ++ g3)

  mkMatch :: Member Fresh r => DTerm -> DPattern -> Sem r [DGuard]
  mkMatch dt dp = return [DGPat (embed dt) dp]

  varMatch :: Member Fresh r => DTerm -> Name DTerm -> Sem r [DGuard]
  varMatch dt x = mkMatch dt (DPVar (getType dt) x)

  varFor :: Member Fresh r => DTerm -> Sem r (Name DTerm, [DGuard])
  varFor (DTVar _ (QName _ x)) = return (x, []) -- XXX return a name + provenance??
  varFor dt = do
    x <- fresh (string2Name "x")
    g <- varMatch dt x
    return (x, g)

  varForPat :: Member Fresh r => APattern -> Sem r (Name DTerm, [DGuard])
  varForPat (APVar _ x) = return (coerce x, [])
  varForPat p = do
    x <- fresh (string2Name "px") -- changing this from x fixed a bug and I don't know why =(
    (x,) <$> desugarMatch (dtVar (getType p) x) p

  arithBinMatch ::
    Member Fresh r =>
    (Type -> Maybe (ATerm -> ATerm -> ATerm)) ->
    (ATerm -> ATerm -> ATerm) ->
    DTerm ->
    Type ->
    APattern ->
    ATerm ->
    Sem r [DGuard]
  arithBinMatch restrict inverse dt ty p t = do
    (x0, g1) <- varFor dt

    -- let v = t
    t' <- desugarTerm t
    (v, g2) <- varFor t'

    g3 <- case restrict ty of
      Nothing -> return []
      -- if x0 `cmp` v
      Just cmp ->
        desugarGuard $
          AGBool (embed (atVar ty (coerce x0) `cmp` atVar (getType t) (coerce v)))

    -- when x0 `inverse` v is p
    inv <- desugarTerm (atVar ty (coerce x0) `inverse` atVar (getType t) (coerce v))
    g4 <- desugarMatch inv p

    return (g1 ++ g2 ++ g3 ++ g4)

-- | Desugar a container literal such as @[1,2,3]@ or @{1,2,3}@.
desugarContainer :: Member Fresh r => Type -> Container -> [(ATerm, Maybe ATerm)] -> Maybe (Ellipsis ATerm) -> Sem r DTerm
-- Literal list containers desugar to nested applications of cons.
desugarContainer ty ListContainer es Nothing =
  foldr (dtbin ty (PrimBOp Cons)) (DTNil ty) <$> mapM (desugarTerm . fst) es
-- A list container with an ellipsis @[x, y, z .. e]@ desugars to an
-- application of the primitive 'until' function.
desugarContainer ty@(TyList _) ListContainer es (Just (Until t)) =
  dtbin ty PrimUntil
    <$> desugarTerm t
    <*> desugarContainer ty ListContainer es Nothing
-- If desugaring a bag and there are any counts specified, desugar to
-- an application of bagFromCounts to a bag of pairs (with a literal
-- value of 1 filled in for missing counts as needed).
desugarContainer (TyBag eltTy) BagContainer es mell
  | any (isJust . snd) es =
      dtapp (DTPrim (TySet (eltTy :*: TyN) :->: TyBag eltTy) PrimC2B)
        <$> desugarContainer (TyBag (eltTy :*: TyN)) BagContainer counts mell
 where
  -- turn e.g.  x # 3, y   into   (x, 3), (y, 1)
  counts =
    [ (ATTup (eltTy :*: TyN) [t, fromMaybe (ATNat TyN 1) n], Nothing)
    | (t, n) <- es
    ]

-- Other containers desugar to an application of the appropriate
-- container conversion function to the corresponding desugared list.
desugarContainer ty _ es mell =
  dtapp (DTPrim (TyList eltTy :->: ty) conv)
    <$> desugarContainer (TyList eltTy) ListContainer es mell
 where
  (conv, eltTy) = case ty of
    TyBag e -> (PrimBag, e)
    TySet e -> (PrimSet, e)
    _ -> error $ "Impossible! Non-container type " ++ show ty ++ " in desugarContainer"

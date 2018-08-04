{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Desugar
-- Copyright   :  (c) 2016 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Desugaring the typechecked surface language to the untyped core
-- language.
--
-----------------------------------------------------------------------------

-- XXX TODO
--   Write linting typechecker for DTerm?

module Disco.Desugar
       ( -- * Desugaring monad
         DSM, runDSM

         -- * Programs and terms
       , desugarDefn, desugarTerm

         -- * Case expressions and patterns
       , desugarBranch, desugarGuards, desugarPattern
       )
       where

import           Control.Monad.Cont

import           Data.Coerce
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Desugared
import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Syntax.Operators
import           Disco.Typecheck.Monad
import           Disco.Types

------------------------------------------------------------
-- Desugaring monad
------------------------------------------------------------

-- | The desugaring monad.  Currently, needs only the ability to
--   generate fresh names (to deal with binders).
type DSM = FreshM

-- | Run a computation in the desugaring monad.
runDSM :: DSM a -> a
runDSM = runFreshM

------------------------------------------------------------
-- ATerm DSL
------------------------------------------------------------

-- A tiny DSL for building certain ATerms, which is helpful for
-- writing desugaring rules.

infixr 2 ||.
(||.) :: ATerm -> ATerm -> ATerm
(||.) = ATBin TyBool Or

infix 4 <.
(<.) :: ATerm -> ATerm -> ATerm
(<.) = ATBin TyBool Lt

infix 4 ==.
(==.) :: ATerm -> ATerm -> ATerm
(==.) = ATBin TyBool Eq

tnot :: ATerm -> ATerm
tnot = ATUn TyBool Not

(<==.) :: ATerm -> [AGuard] -> ABranch
t <==. gs = bind (toTelescope gs) t

fls :: ATerm
fls = ATBool False

tru :: ATerm
tru = ATBool True

tif :: ATerm -> AGuard
tif t = AGBool (embed t)

nil :: Type -> ATerm
nil ty = ATContainer (TyList ty) ListContainer [] Nothing

tsingleton :: ATerm -> ATerm
tsingleton t = ATContainer (TyList (getType t)) ListContainer [t] Nothing

------------------------------------------------------------
-- Definition desugaring
------------------------------------------------------------

-- | Desugar a definition (of the form @f pat1 .. patn = term@, but
--   without the name @f@ of the thing being defined) into a core
--   language term.  Each pattern desugars to an anonymous function,
--   appropriately combined with a case expression for non-variable
--   patterns.
--
--   For example, @f n (x,y) = n*x + y@ desugars to something like
--
--   @
--     n -> p -> { n*x + y  when p = (x,y)
--   @
desugarDefn :: Defn -> DSM DTerm
desugarDefn (Defn _ patTys bodyTy def) = do
  clausePairs <- mapM unbind def
  let (pats, bodies) = unzip clausePairs

  -- generate dummy variables for lambdas
  args <- zipWithM (\_ i -> fresh (string2Name ("arg" ++ show i))) (head pats) [0 :: Int ..]
  branches <- zipWithM (mkBranch (zip args patTys)) bodies pats

  -- Create lambdas and one big case
  return $ mkLambda (foldr TyArr bodyTy patTys) (coerce args) (DTCase bodyTy branches)

  where
    mkBranch :: [(Name DTerm, Type)] -> ATerm -> [APattern] -> DSM DBranch
    mkBranch xs b ps = do
      b'  <- desugarTerm b
      let ps' = map desugarPattern ps
      return $ bind (mkGuards xs ps') b'

    mkGuards :: [(Name DTerm, Type)] -> [DPattern] -> Telescope DGuard
    mkGuards xs ps = toTelescope $ zipWith DGPat (map (\(x,ty) -> embed (DTVar ty x)) xs) ps

------------------------------------------------------------
-- Term desugaring
------------------------------------------------------------

-- | Desugar a typechecked term.
desugarTerm :: ATerm -> DSM DTerm
desugarTerm (ATVar ty x)         = return $ DTVar ty (coerce x)
desugarTerm ATUnit               = return $ DTUnit
desugarTerm (ATBool b)           = return $ DTBool b
desugarTerm (ATChar c)           = return $ DTChar c
desugarTerm (ATString cs)        = desugarContainer (TyList TyC) ListContainer (map ATChar cs) Nothing
desugarTerm (ATAbs ty lam)       = do
  (args, t) <- unbind lam
  mkLambda ty (map fst args) <$> desugarTerm t
desugarTerm (ATApp ty t1 t2)     =
  DTApp ty <$> desugarTerm t1 <*> desugarTerm t2
desugarTerm (ATTup ty ts)        = desugarTuples ty ts
desugarTerm (ATInj ty s t)       =
  DTInj ty s <$> desugarTerm t
desugarTerm (ATNat ty n)         = return $ DTNat ty n
desugarTerm (ATRat r)            = return $ DTRat r

-- not t ==> {? false if t, true otherwise ?}
-- This should be turned into a standard library definition.
desugarTerm (ATUn _ Not t)       =
  desugarTerm $
    ATCase TyBool
      [ fls <==. [AGBool (embed t)]
      , tru <==. []
      ]

-- Desugar negation on TyFin to a negation on TyZ followed by a mod.
-- See the comments below re: Add and Mul on TyFin.
desugarTerm (ATUn (TyFin n) Neg t) =
  desugarTerm $ ATBin (TyFin n) Mod (ATUn TyZ Neg t) (ATNat TyN n)

desugarTerm (ATUn ty op t)       = DTUn ty op <$> desugarTerm t

-- Implies, and, or should all be turned into a standard library
-- definition.  This will require first (1) adding support for
-- modules/a standard library, including (2) the ability to define
-- infix operators.

-- (t1 implies t2) ==> (not t1 or t2)
desugarTerm (ATBin _ Impl t1 t2) = desugarTerm $ tnot t1 ||. t2

desugarTerm (ATBin _ And t1 t2)  = do
  -- t1 and t2 ==> {? t2 if t1, false otherwise ?}
  desugarTerm $
    ATCase TyBool
      [ t2  <==. [tif t1]
      , fls <==. []
      ]
desugarTerm (ATBin _ Or t1 t2) = do
  -- t1 or t2 ==> {? true if t1, t2 otherwise ?})
  desugarTerm $
    ATCase TyBool
      [ tru <==. [tif t1]
      , t2  <==. []
      ]
desugarTerm (ATBin ty Sub t1 t2)  = desugarTerm $ ATBin ty Add t1 (ATUn ty Neg t2)
desugarTerm (ATBin ty SSub t1 t2) = desugarTerm $
  -- t1 -. t2 ==> {? 0 if t1 < t2, t1 - t2 otherwise ?}
  ATCase ty
    [ ATNat ty 0         <==. [tif (t1 <. t2)]
    , ATBin ty Sub t1 t2 <==. []
      -- NOTE, the above is slightly bogus since the whole point of SSub is
      -- because we can't subtract naturals.  However, this will
      -- immediately desugar to a DTerm.  When we write a linting
      -- typechecker for DTerms we should allow subtraction on TyN!
    ]
desugarTerm (ATBin ty IDiv t1 t2) = desugarTerm $ ATUn ty Floor (ATBin (getType t1) Div t1 t2)
desugarTerm (ATBin _ Neq t1 t2)   = desugarTerm $ tnot (t1 ==. t2)
desugarTerm (ATBin _ Gt  t1 t2)   = desugarTerm $ t2 <. t1
desugarTerm (ATBin _ Leq t1 t2)   = desugarTerm $ tnot (t2 <. t1)
desugarTerm (ATBin _ Geq t1 t2)   = desugarTerm $ tnot (t1 <. t2)

-- Addition and multiplication on TyFin just desugar to the operation
-- followed by a call to mod.
desugarTerm (ATBin (TyFin n) op t1 t2)
  | op `elem` [Add, Mul]
  = desugarTerm $
      ATBin (TyFin n) Mod
        (ATBin TyN op t1 t2)
        (ATNat TyN n)
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

-- Desugar normal binomial coefficient (n choose k) to a multinomial
-- coefficient with a singleton list, (n choose [k]).
desugarTerm (ATBin ty Choose t1 t2)
  | getType t2 == TyN = desugarTerm $ ATBin ty Choose t1 (tsingleton t2)

desugarTerm (ATBin ty op t1 t2)   = DTBin ty op <$> desugarTerm t1 <*> desugarTerm t2

desugarTerm (ATTyOp ty op t)      = return $ DTTyOp ty op t

desugarTerm (ATChain _ t1 links)  = desugarTerm $ expandChain t1 links

desugarTerm (ATContainer ty c es mell) = desugarContainer ty c es mell

desugarTerm (ATContainerComp (TyList eltTy) ListContainer bqt) = do
  (qs, t) <- unbind bqt
  desugarComp eltTy t qs

desugarTerm (ATContainerComp ty ListContainer _) =
  error $ "Non-TyList type passed to desugarTerm for a ListContainer" ++ show ty

desugarTerm (ATContainerComp ty _ _) = error $ "desugar ContainerComp unimplemented for " ++ show ty

desugarTerm (ATLet _ t) = do
  (bs, t2) <- unbind t
  desugarLet (fromTelescope bs) t2

desugarTerm (ATCase ty bs) = DTCase ty <$> mapM desugarBranch bs

------------------------------------------------------------
-- Desugaring other stuff
------------------------------------------------------------

-- | Desugar a list comprehension.  First translate it into an
--   expanded ATerm and then recursively desugar that.
desugarComp :: Type -> ATerm -> Telescope AQual -> DSM DTerm
desugarComp ty t qs = expandComp ty t qs >>= desugarTerm

-- | Expand a list comprehension into an equivalent ATerm.
expandComp :: Type -> ATerm -> Telescope AQual -> DSM ATerm

-- [ t | ] = [ t ]
expandComp _ t TelEmpty = return $ tsingleton t

-- [ t | q, qs ] = ...
expandComp xTy t (TelCons (unrebind -> (q,qs)))
  = case q of
      -- [ t | x in l, qs ] = concat (map (\x -> [t | qs]) l)
      AQBind x (unembed -> lst) -> do
        tqs <- expandComp (TyList xTy) t qs
        let resTy        = TyList (getType t)
            concatTy     = TyArr (TyList resTy) resTy
            mapTy        = TyArr (TyArr xTy resTy) (TyArr (TyList xTy) (TyList resTy))
        return $ ATApp resTy (ATVar concatTy (string2Name "concat")) $
          ATApp (TyList resTy)
            (ATApp (TyArr (TyList xTy) (TyList resTy))
              (ATVar mapTy (string2Name "mapList"))
              (ATAbs (TyArr xTy resTy) (bind [(x, embed xTy)] tqs))
            )
            lst

      -- [ t | g, qs ] = if g then [ t | qs ] else []
      AQGuard (unembed -> g)    -> do
        tqs <- expandComp (TyList xTy) t qs
        return $ ATCase (TyList (getType t))
          [ tqs             <==. [tif g]
          , nil (getType t) <==. []
          ]

-- | Desugar a let into applications of a chain of nested lambdas.
--   /e.g./
--
--     @let x = s, y = t in q@
--
--   desugars to
--
--     @(\x. (\y. q) t) s@
desugarLet :: [ABinding] -> ATerm -> DSM DTerm
desugarLet [] t = desugarTerm t
desugarLet ((ABinding _ x (unembed -> t1)) : ls) t =
  DTApp (getType t)
    <$> (DTLam (TyArr (getType t1) (getType t))
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
    go _ []                    = c
    go ty@(TyArr _ ty2) (x:xs) = DTLam ty (bind (coerce x) (go ty2 xs))

    go ty as = error $ "Impossible! mkLambda.go " ++ show ty ++ " " ++ show as

-- | Desugar a tuple to nested pairs, /e.g./ @(a,b,c,d) ==> (a,(b,(c,d)))@.a
desugarTuples :: Type -> [ATerm] -> DSM DTerm
desugarTuples _ [t]                    = desugarTerm t
desugarTuples ty@(TyPair _ ty2) (t:ts) = DTPair ty <$> desugarTerm t <*> desugarTuples ty2 ts
desugarTuples ty ats
  = error $ "Impossible! desugarTuples " ++ show ty ++ " " ++ show ats

-- | Expand a chain of comparisons into a sequence of binary
--   comparisons combined with @and@.  Note we only expand it into
--   another 'ATerm' (which will be recursively desugared), because
--   @and@ itself also gets desugared.
--
--   For example, @a < b <= c > d@ becomes @a < b and b <= c and c > d@.
expandChain :: ATerm -> [ALink] -> ATerm
expandChain _ [] = error "Can't happen! expandChain _ []"
expandChain t1 [ATLink op t2] = ATBin TyBool op t1 t2
expandChain t1 (ATLink op t2 : links) =
  ATBin TyBool And
    (ATBin TyBool op t1 t2)
    (expandChain t2 links)

-- | Desugar a branch of a case expression.
desugarBranch :: ABranch -> DSM DBranch
desugarBranch b = do
  (ags, at) <- unbind b
  dgs <- desugarGuards ags
  d   <- desugarTerm at
  return $ bind dgs d

-- | Desugar the list of guards in one branch of a case expression.
--   Pattern guards essentially remain as they are; boolean guards get
--   turned into pattern guards which match against @true@.
desugarGuards :: Telescope AGuard -> DSM (Telescope DGuard)
desugarGuards gs = toTelescope <$> mapM desugarGuard (fromTelescope gs)
  where
    desugarGuard :: AGuard -> DSM DGuard
    -- A Boolean guard is desugared to a pattern-match on @true@.
    desugarGuard (AGBool (unembed -> at)) = do
      dt <- desugarTerm at
      return $ DGPat (embed dt) (DPBool True)
    desugarGuard (AGPat (unembed -> at) p) = do
      dt <- desugarTerm at
      return $ DGPat (embed dt) (desugarPattern p)
    desugarGuard (AGLet (ABinding _ x (unembed -> at))) = do
      dt <- desugarTerm at
      return $ DGPat (embed dt) (DPVar (getType dt) (coerce x))

desugarBinding :: ABinding -> DSM DBinding
desugarBinding (ABinding mty x (unembed -> t))
  = DBinding mty (coerce x) <$> (embed <$> desugarTerm t)

-- | Desugar a container literal such as @[1,2,3]@ or @{1,2,3}@.
desugarContainer :: Type -> Container -> [ATerm] -> Maybe (Ellipsis ATerm) -> DSM DTerm

-- Literal list containers desugar to nested applications of cons.
desugarContainer ty ListContainer es Nothing =
  foldr (DTBin ty Cons) (DTNil ty) <$> mapM desugarTerm es

-- All others simply turn into a @DTContainer@ constructor, though we
-- might in the future consider replacing DTContainer by more
-- primitive things. For example, we could add a primitive set
-- insertion function and desugar to repeated applications of it.

desugarContainer ty c es mell =
  DTContainer ty c <$> mapM desugarTerm es <*> (traverse . traverse) desugarTerm mell

-- | Desugar a pattern.
desugarPattern :: APattern -> DPattern
desugarPattern (APVar ty x)      = DPVar ty (coerce x)
desugarPattern (APWild ty)       = DPWild ty
desugarPattern APUnit            = DPUnit
desugarPattern (APBool b)        = DPBool b
desugarPattern (APChar c)        = DPChar c
desugarPattern (APString s)      = desugarPattern (APList (TyList TyC) (map APChar s))
desugarPattern (APTup _ p)       = desugarTuplePats p
desugarPattern (APInj ty s p)    = DPInj ty s (desugarPattern p)
desugarPattern (APNat ty n)      = DPNat ty n
desugarPattern (APSucc p)        = DPSucc (desugarPattern p)
desugarPattern (APCons ty p1 p2) = DPCons ty (desugarPattern p1) (desugarPattern p2)
desugarPattern (APList ty ps)    = foldr (DPCons eltTy . desugarPattern) (DPNil ty) ps
  where
    eltTy = case ty of
      TyList e -> e
      _        -> error $ "Impossible! APList with non-TyList " ++ show ty

-- | Desugar a tuple pattern into nested pair patterns.  For example,
--   the pattern @(a,b,c,d)@ turns into @(a,(b,(c,d)))@.
desugarTuplePats :: [APattern] -> DPattern
desugarTuplePats []     = error "Impossible! desugarTuplePats []"
desugarTuplePats [p]    = desugarPattern p
desugarTuplePats (p:ps) = mkDPPair (desugarPattern p) (desugarTuplePats ps)
  where
    mkDPPair p1 p2 = DPPair (TyPair (getType p1) (getType p2)) p1 p2

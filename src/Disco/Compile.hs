{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Compile
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Compiling the typechecked, desugared AST to the untyped core
-- language.
--
-----------------------------------------------------------------------------

module Disco.Compile
       where

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Core
import           Disco.AST.Desugared
import           Disco.AST.Generic
import           Disco.AST.Typed
import           Disco.Desugar
import           Disco.Module
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Types
import           Disco.Util

import           Data.Coerce
import           Data.Map                         ((!))
import qualified Data.Map                         as M
import           Data.Ratio

------------------------------------------------------------
-- Convenience operations
------------------------------------------------------------

-- | Compile a typechecked term ('ATerm') directly to a 'Core' term,
--   by desugaring and then compiling.
compileTerm :: ATerm -> Core
compileTerm = runFreshM . compileDTerm . runDSM . desugarTerm

-- | Compile a typechecked definition ('Defn') directly to a 'Core' term,
--   by desugaring and then compiling.
compileDefn :: Defn -> Core
compileDefn = runFreshM . compileDTerm . runDSM . desugarDefn

------------------------------------------------------------
-- Compiling terms
------------------------------------------------------------

-- | Compile a typechecked, desugared 'DTerm' to an untyped 'Core'
--   term.
compileDTerm :: DTerm -> FreshM Core
compileDTerm (DTVar _ x)   = return $ CVar (coerce x)
compileDTerm (DTPrim ty x) = compilePrim ty x
compileDTerm DTUnit        = return $ CCons 0 []
compileDTerm (DTBool b)    = return $ CCons (fromEnum b) []
compileDTerm (DTChar c)    = return $ CNum Fraction ((toInteger $ fromEnum c) % 1)
compileDTerm (DTNat ty n)  = compileNat ty n
compileDTerm (DTRat r)     = return $ CNum Decimal r

compileDTerm (DTLam _ l) = do
  (x,body) <- unbind l
  c <- compileDTerm body
  return $ CAbs (bind [coerce x] c)   -- XXX collect up nested DTLam into a single CAbs?

compileDTerm (DTApp _ t1 t2)
  = appChain t1 [t2]
  where
    appChain (DTApp _ t1' t2') ts = appChain t1' (t2':ts)
    appChain t1' ts               = CApp <$> compileDTerm t1' <*> mapM compileArg ts

compileDTerm (DTPair _ t1 t2)
  = CCons 0 <$> mapM compileDTerm [t1,t2]

compileDTerm (DTInj _ s t)
  = CCons (fromEnum s) <$> mapM compileDTerm [t]

compileDTerm (DTCase _ bs)
  = CCase <$> mapM compileBranch bs

compileDTerm (DTUn _ op t)
  = CApp (compileUOp (getType t) op) <$> mapM compileArg [t]

-- Special case for Cons, which compiles to a constructor application
-- rather than a function application.
compileDTerm (DTBin _ Cons t1 t2)
  = CCons 1 <$> mapM compileDTerm [t1, t2]

compileDTerm (DTBin ty op t1 t2)
  = CApp (compileBOp (getType t1) (getType t2) ty op) <$> mapM compileArg [t1, t2]

compileDTerm (DTTyOp _ op ty) = return $ CApp (CConst (tyOps ! op)) [(Strict, CType ty)]
  where
    tyOps = M.fromList
      [ Enumerate ==> OEnum
      , Count     ==> OCount
      ]

compileDTerm (DTNil _)        = return $ CCons 0 []

------------------------------------------------------------

-- | Compile a natural number. A separate function is needed in
--   case the number is of a finite type, in which case we must
--   mod it by its type.
compileNat :: Type -> Integer -> FreshM Core
compileNat (TyFin n) x = return $ CNum Fraction ((x `mod` n) % 1)
compileNat _         x = return $ CNum Fraction (x % 1)

------------------------------------------------------------

-- | Compile a DTerm which will be an argument to a function,
--   packaging it up along with the strictness of its type.
compileArg :: DTerm -> FreshM (Strictness, Core)
compileArg dt = (strictness (getType dt),) <$> compileDTerm dt

-- | Compile a primitive.  Typically primitives turn into a
--   corresponding function constant in the core language, but
--   sometimes the particular constant it turns into may depend on the
--   type.
compilePrim :: Type -> Prim -> FreshM Core
compilePrim (TySet _ :->: _)  PrimList = return $ CConst OSetToList
compilePrim (TyBag _ :->: _)  PrimSet  = return $ CConst OBagToSet
compilePrim (TyBag _ :->: _)  PrimList = return $ CConst OBagToList
compilePrim (TyList a :->: _) PrimSet  = return $ CConst (OListToSet a)
compilePrim (TyList a :->: _) PrimBag  = return $ CConst (OListToBag a)
compilePrim _ p | p `elem` [PrimList, PrimBag, PrimSet] = return $ CConst OId

compilePrim ty PrimList = compilePrimErr PrimList ty
compilePrim ty PrimBag  = compilePrimErr PrimBag ty
compilePrim ty PrimSet  = compilePrimErr PrimSet ty

compilePrim (_ :->: TyList _ :->: _)          PrimMap = return $ CConst OMapList
compilePrim (_ :->: TyBag _ :->: TyBag outTy) PrimMap = return $ CConst (OMapBag outTy)
compilePrim (_ :->: TySet _ :->: TySet outTy) PrimMap = return $ CConst (OMapSet outTy)
compilePrim ty                                PrimMap = compilePrimErr PrimMap ty

compilePrim (_ :->: _ :->: TyList _ :->: _) PrimReduce = return $ CConst OReduceList
compilePrim (_ :->: _ :->: TyBag  _ :->: _) PrimReduce = return $ CConst OReduceBag
compilePrim (_ :->: _ :->: TySet  _ :->: _) PrimReduce = return $ CConst OReduceBag
compilePrim ty                              PrimReduce = compilePrimErr PrimReduce ty

compilePrim (_ :->: TyList _ :->: _) PrimFilter = return $ CConst OFilterList
compilePrim (_ :->: TyBag  _ :->: _) PrimFilter = return $ CConst OFilterBag
compilePrim (_ :->: TySet  _ :->: _) PrimFilter = return $ CConst OFilterBag
compilePrim ty                       PrimFilter = compilePrimErr PrimFilter ty

compilePrim (_ :->: TyList _) PrimJoin = return $ CConst OConcat
compilePrim (_ :->: TyBag  a) PrimJoin = return $ CConst (OBagUnions a)
compilePrim (_ :->: TySet  a) PrimJoin = return $ CConst (OUnions a)
compilePrim ty                PrimJoin = compilePrimErr PrimJoin ty

compilePrim (_ :->: TyBag a :->: _ :->: _) PrimMerge = return $ CConst (OMerge a)
compilePrim (_ :->: TySet a :->: _ :->: _) PrimMerge = return $ CConst (OMerge a)
compilePrim ty                             PrimMerge = compilePrimErr PrimMerge ty

compilePrim _ PrimIsPrime = return $ CConst OIsPrime
compilePrim _ PrimFactor  = return $ CConst OFactor

compilePrim _ PrimCrash   = return $ CConst OCrash

compilePrim _ PrimForever = return $ CConst OForever
compilePrim _ PrimUntil   = return $ CConst OUntil

compilePrimErr :: Prim -> Type -> a
compilePrimErr p ty = error $ "Impossible! compilePrim " ++ show p ++ " on bad type " ++ show ty

------------------------------------------------------------
-- Case expressions
------------------------------------------------------------

-- | Compile a desugared branch.  This does very little actual work, just
--   translating directly from one AST to another.
compileBranch :: DBranch -> FreshM CBranch
compileBranch b = do
  (gs, d) <- unbind b
  bind <$> traverseTelescope compileGuard gs <*> compileDTerm d

-- | Compile a desugared guard.
compileGuard :: DGuard -> FreshM (Embed Core, CPattern)
compileGuard (DGPat (unembed -> d) dpat) =
  (,)
    <$> (embed <$> compileDTerm d)
    <*> pure (compilePattern dpat)

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

-- | Compile a desugared pattern.  Turns all constructors into CPCons.
compilePattern :: DPattern -> CPattern
compilePattern (DPVar _ x)      = CPVar (coerce x)
compilePattern (DPWild _)       = CPWild
compilePattern DPUnit           = CPCons 0 []
compilePattern (DPBool b)       = CPCons (fromEnum b) []
compilePattern (DPChar c)       = CPNat (toInteger $ fromEnum c)
compilePattern (DPPair _ x1 x2) = CPCons 0 (map coerce [x1,x2])
compilePattern (DPInj _ s x)    = CPCons (fromEnum s) [coerce x]
compilePattern (DPNat _ n)      = CPNat n
compilePattern (DPFrac _ x1 x2) = CPFrac (coerce x1) (coerce x2)
compilePattern (DPNil _)        = CPCons 0 []
compilePattern (DPCons _ x1 x2) = CPCons 1 (map coerce [x1, x2])

------------------------------------------------------------
-- Unary and binary operators
------------------------------------------------------------

-- | Compile a unary operator.
compileUOp
  :: Type   -- ^ Type of the operator argument
  -> UOp
  -> Core

compileUOp (TySet a) PowerSet = CConst (OPowerSet a)

compileUOp _ op = CConst (coreUOps ! op)
  where
    -- Just look up the corresponding core operator.
    coreUOps = M.fromList $
      [ Neg      ==> ONeg
      , Fact     ==> OFact
      , Sqrt     ==> OSqrt
      , Lg       ==> OLg
      , Floor    ==> OFloor
      , Ceil     ==> OCeil
      , Abs      ==> OAbs
      ]

-- | Compile a binary operator.  This function needs to know the types
--   of the arguments and result since some operators are overloaded
--   and compile to different code depending on their type.
--
--  @arg1 ty -> arg2 ty -> result ty -> op -> result@
compileBOp :: Type -> Type -> Type -> BOp -> Core

-- First, compile some operators specially for modular arithmetic.
-- Most operators on TyFun (add, mul, sub, etc.) have already been
-- desugared to an operation followed by a mod.  The only operators
-- here are the ones that have a special runtime behavior for Zn that
-- can't be implemented in terms of other, existing operators:
--
--   - Division on Zn needs to find modular inverses.
--   - Divisibility testing on Zn similarly needs to find a gcd etc.
--   - Exponentiation on Zn could in theory be implemented as a normal
--     exponentiation on naturals followed by a mod, but that would be
--     silly and inefficient.  Instead we compile to a special modular
--     exponentiation operator which takes mods along the way.  Also,
--     negative powers have similar requirements to division.
--
-- We match on the type of arg1 because that is the only one which
-- will consistently be TyFin in the case of Div, Exp, and Divides.
compileBOp (TyFin n) _ _ op
  | op `elem` [Div, Exp, Divides]
  = CConst ((omOps ! op) n)
  where
    omOps = M.fromList
      [ Div     ==> OMDiv
      , Exp     ==> OMExp
      , Divides ==> OMDivides
      ]

-- Some regular arithmetic operations that just translate straightforwardly.
compileBOp _ _ _ op
  | op `elem` [Add, Mul, Div, Exp, Mod, Divides, Choose]
  = CConst (regularOps ! op)
  where
    regularOps = M.fromList
      [ Add     ==> OAdd
      , Mul     ==> OMul
      , Div     ==> ODiv
      , Exp     ==> OExp
      , Mod     ==> OMod
      , Divides ==> ODivides
      , Choose  ==> OMultinom
      ]

-- Eq and Lt need to remember the type of the things being compared so
-- it can work properly at runtime. (Eq and Lt are overloaded:
-- essentially instead of storing a dictionary here as Haskell might,
-- we just store the type itself, and compute the comparison function
-- in a type-directed way; see Disco.Interpret.Core.decideEqFor and
-- decideOrdFor.)
compileBOp ty _ _ Eq = CConst (OEq ty)
compileBOp ty _ _ Lt = CConst (OLt ty)

-- Operations on sets compile straightforwardly, except that they also
-- need to store the element type so they can compare the elements
-- appropriately.
compileBOp (TySet ty) _ _ op
  | op `elem` [Union, Inter, Diff, Subset]
  = CConst ((setOps ! op) ty)
  where
    setOps = M.fromList
      [ Union  ==> OUnion
      , Inter  ==> OInter
      , Diff   ==> ODiff
      , Subset ==> OSubset
      ]

compileBOp _ _ _ Rep = CConst ORep

compileBOp ty1 ty2 resTy op
  = error $ "Impossible! missing case in compileBOp: " ++ show (ty1, ty2, resTy, op)

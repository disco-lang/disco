{-# LANGUAGE DataKinds     #-}
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

import           Data.Bool                        (bool)
import           Data.Coerce
import           Data.Map                         ((!))
import qualified Data.Map                         as M
import           Data.Ratio

import           Disco.Effects.Fresh
import           Polysemy                         (Member, Sem)
import           Unbound.Generics.LocallyNameless (Embed, Name, bind, embed,
                                                   string2Name, unembed)

------------------------------------------------------------
-- Convenience operations
------------------------------------------------------------

-- | Compile a typechecked term ('ATerm') directly to a 'Core' term,
--   by desugaring and then compiling.
compileTerm :: ATerm -> Core
compileTerm = runFresh . compileDTerm . runDesugar . desugarTerm

-- | Compile a typechecked definition ('Defn') directly to a 'Core' term,
--   by desugaring and then compiling.
compileDefn :: Defn -> Core
compileDefn = runFresh . compileDTerm . runDesugar . desugarDefn

-- | Compile a typechecked property ('AProperty') directly to a 'Core' term,
--   by desugaring and then compilling.
compileProperty :: AProperty -> Core
compileProperty = runFresh . compileDTerm . runDesugar . desugarProperty

------------------------------------------------------------
-- Compiling terms
------------------------------------------------------------

-- | Compile a typechecked, desugared 'DTerm' to an untyped 'Core'
--   term.
compileDTerm :: Member Fresh r => DTerm -> Sem r Core
compileDTerm (DTVar _ x)   = return $ CVar (coerce x)
compileDTerm (DTPrim ty x) = compilePrim ty x
compileDTerm DTUnit        = return CUnit
compileDTerm (DTBool _ b)  = return $ CInj (bool L R b) CUnit
compileDTerm (DTChar c)    = return $ CNum Fraction (toInteger (fromEnum c) % 1)
compileDTerm (DTNat _ n)   = return $ CNum Fraction (n % 1)   -- compileNat ty n
compileDTerm (DTRat r)     = return $ CNum Decimal r

compileDTerm term@(DTAbs q _ _) = do
  (xs, tys, body) <- unbindDeep term
  cbody <- compileDTerm body
  case q of
    Lam -> return $ abstract xs cbody
    Ex  -> return $ quantify (OExists tys) (abstract xs cbody)
    All -> return $ quantify (OForall tys) (abstract xs cbody)

  where
    -- Gather nested abstractions with the same quantifier.
    unbindDeep :: Member Fresh r => DTerm -> Sem r ([Name DTerm], [Type], DTerm)
    unbindDeep (DTAbs q' ty l) | q == q' = do
      (name, inner) <- unbind l
      (names, tys, body) <- unbindDeep inner
      return (name:names, ty:tys, body)
    unbindDeep t                         = return ([], [], t)

    abstract :: [Name DTerm] -> Core -> Core
    abstract xs body = CAbs (bind (map coerce xs) body)

    quantify :: Op -> Core -> Core
    quantify op f = CApp (CConst op) [(Lazy, f)]

-- Special case for Cons, which compiles to a constructor application
-- rather than a function application.
compileDTerm (DTApp _ (DTPrim _ (PrimBOp Cons)) (DTPair _ t1 t2))
  = CInj R <$> (CPair <$> compileDTerm t1 <*> compileDTerm t2)

-- Special cases for left and right, which also compile to constructor applications.
compileDTerm (DTApp _ (DTPrim _ PrimLeft) t)
  = CInj L <$> compileDTerm t

compileDTerm (DTApp _ (DTPrim _ PrimRight) t)
  = CInj R <$> compileDTerm t

compileDTerm (DTApp _ t1 t2)
  = appChain t1 [t2]
  where
    appChain (DTApp _ t1' t2') ts = appChain t1' (t2':ts)
    appChain t1' ts               = CApp <$> compileDTerm t1' <*> mapM compileArg ts

compileDTerm (DTPair _ t1 t2)
  = CPair <$> compileDTerm t1 <*> compileDTerm t2

compileDTerm (DTCase _ bs)
  = CCase <$> mapM compileBranch bs

compileDTerm (DTTyOp _ op ty) = return $ CApp (CConst (tyOps ! op)) [(Strict, CType ty)]
  where
    tyOps = M.fromList
      [ Enumerate ==> OEnum
      , Count     ==> OCount
      ]

compileDTerm (DTNil _)        = return $ CInj L CUnit

compileDTerm (DTTest info t)  = CTest (coerce info) <$> compileDTerm t

------------------------------------------------------------

-- | Compile a natural number. A separate function is needed in
--   case the number is of a finite type, in which case we must
--   mod it by its type.
-- compileNat :: Member Fresh r => Type -> Integer -> Sem r Core
-- compileNat (TyFin n) x = return $ CNum Fraction ((x `mod` n) % 1)
-- compileNat _         x = return $ CNum Fraction (x % 1)

------------------------------------------------------------

-- | Compile a DTerm which will be an argument to a function,
--   packaging it up along with the strictness of its type.
compileArg :: Member Fresh r => DTerm -> Sem r (Strictness, Core)
compileArg dt = (strictness (getType dt),) <$> compileDTerm dt

-- | Compile a primitive.  Typically primitives turn into a
--   corresponding function constant in the core language, but
--   sometimes the particular constant it turns into may depend on the
--   type.
compilePrim :: Member Fresh r => Type -> Prim -> Sem r Core

compilePrim (argTy :->: _) (PrimUOp uop) = return $ compileUOp argTy uop
compilePrim ty p@(PrimUOp _) = compilePrimErr p ty

-- This special case for Cons only triggers if we didn't hit the case
-- for fully saturated Cons; just fall back to generating a lambda.  Have to
-- do it here, not in compileBOp, since we need to generate fresh names.
compilePrim _ (PrimBOp Cons) = do
  hd <- fresh (string2Name "hd")
  tl <- fresh (string2Name "tl")
  return $ CAbs $ bind [hd, tl] $ CInj R (CPair (CVar hd) (CVar tl))

compilePrim _ PrimLeft = do
  a <- fresh (string2Name "a")
  return $ CAbs $ bind [a] $ CInj L (CVar a)

compilePrim _ PrimRight = do
  a <- fresh (string2Name "a")
  return $ CAbs $ bind [a] $ CInj R (CVar a)

compilePrim (ty1 :*: ty2 :->: resTy) (PrimBOp bop) = return $ compileBOp ty1 ty2 resTy bop
compilePrim ty p@(PrimBOp _) = compilePrimErr p ty

compilePrim _ PrimSqrt  = return $ CConst OSqrt
compilePrim _ PrimFloor = return $ CConst OFloor
compilePrim _ PrimCeil  = return $ CConst OCeil
compilePrim _ PrimAbs   = return $ CConst OAbs
compilePrim _ PrimSize  = return $ CConst OSize

compilePrim (TySet a :->: _) PrimPower = return $ CConst (OPower a)
compilePrim (TyBag a :->: _) PrimPower = return $ CConst (OPower a)
compilePrim ty               PrimPower = compilePrimErr PrimPower ty

compilePrim (TySet _ :->: _)  PrimList = return $ CConst OSetToList
compilePrim (TyBag _ :->: _)  PrimSet  = return $ CConst OBagToSet
compilePrim (TyBag _ :->: _)  PrimList = return $ CConst OBagToList
compilePrim (TyList a :->: _) PrimSet  = return $ CConst (OListToSet a)
compilePrim (TyList a :->: _) PrimBag  = return $ CConst (OListToBag a)
compilePrim _ p | p `elem` [PrimList, PrimBag, PrimSet] = return $ CConst OId

compilePrim ty PrimList = compilePrimErr PrimList ty
compilePrim ty PrimBag  = compilePrimErr PrimBag ty
compilePrim ty PrimSet  = compilePrimErr PrimSet ty

compilePrim _ PrimB2C                 = return $ CConst OBagToCounts
compilePrim (_ :->: TyBag ty) PrimC2B = return $ CConst (OCountsToBag ty)
compilePrim ty PrimC2B                = compilePrimErr PrimC2B ty

compilePrim (TyMap k v :->: _) PrimMapToSet = return $ CConst (OMapToSet k v)
compilePrim (_ :->: TyMap _ _) PrimSetToMap = return $ CConst OSetToMap

compilePrim ty PrimMapToSet = compilePrimErr PrimMapToSet ty
compilePrim ty PrimSetToMap = compilePrimErr PrimSetToMap ty

compilePrim _     PrimSummary = return $ CConst OSummary
compilePrim (_ :->: TyGraph ty) PrimVertex     = return $ CConst $ OVertex ty
compilePrim (TyGraph ty)        PrimEmptyGraph = return $ CConst $ OEmptyGraph ty
compilePrim (_ :->: TyGraph ty) PrimOverlay    = return $ CConst $ OOverlay ty
compilePrim (_ :->: TyGraph ty) PrimConnect    = return $ CConst $ OConnect ty

compilePrim ty PrimVertex     = compilePrimErr PrimVertex ty
compilePrim ty PrimEmptyGraph = compilePrimErr PrimEmptyGraph ty
compilePrim ty PrimOverlay    = compilePrimErr PrimOverlay ty
compilePrim ty PrimConnect    = compilePrimErr PrimConnect ty

compilePrim _  PrimEmptyMap   = return $ CConst OEmptyMap
compilePrim _  PrimInsert     = return $ CConst OInsert
compilePrim _  PrimLookup     = return $ CConst OLookup

compilePrim (_ :*: TyList _ :->: _)          PrimEach = return $ CConst OEachList
compilePrim (_ :*: TyBag _ :->: TyBag outTy) PrimEach = return $ CConst (OEachBag outTy)
compilePrim (_ :*: TySet _ :->: TySet outTy) PrimEach = return $ CConst (OEachSet outTy)
compilePrim ty                               PrimEach = compilePrimErr PrimEach ty

compilePrim (_ :*: _ :*: TyList _ :->: _) PrimReduce = return $ CConst OReduceList
compilePrim (_ :*: _ :*: TyBag  _ :->: _) PrimReduce = return $ CConst OReduceBag
compilePrim (_ :*: _ :*: TySet  _ :->: _) PrimReduce = return $ CConst OReduceBag
compilePrim ty                            PrimReduce = compilePrimErr PrimReduce ty

compilePrim (_ :*: TyList _ :->: _) PrimFilter = return $ CConst OFilterList
compilePrim (_ :*: TyBag  _ :->: _) PrimFilter = return $ CConst OFilterBag
compilePrim (_ :*: TySet  _ :->: _) PrimFilter = return $ CConst OFilterBag
compilePrim ty                      PrimFilter = compilePrimErr PrimFilter ty

compilePrim (_ :->: TyList _) PrimJoin = return $ CConst OConcat
compilePrim (_ :->: TyBag  a) PrimJoin = return $ CConst (OBagUnions a)
compilePrim (_ :->: TySet  a) PrimJoin = return $ CConst (OUnions a)
compilePrim ty                PrimJoin = compilePrimErr PrimJoin ty

compilePrim (_ :*: TyBag a :*: _ :->: _) PrimMerge = return $ CConst (OMerge a)
compilePrim (_ :*: TySet a :*: _ :->: _) PrimMerge = return $ CConst (OMerge a)
compilePrim ty                           PrimMerge = compilePrimErr PrimMerge ty

compilePrim _ PrimIsPrime = return $ CConst OIsPrime
compilePrim _ PrimFactor  = return $ CConst OFactor

compilePrim _ PrimCrash   = return $ CConst OCrash

compilePrim _ PrimForever = return $ CConst OForever
compilePrim _ PrimUntil   = return $ CConst OUntil

compilePrim _ PrimHolds   = return $ CConst OHolds

compilePrim _ PrimLookupSeq   = return $ CConst OLookupSeq
compilePrim _ PrimExtendSeq   = return $ CConst OExtendSeq

compilePrimErr :: Prim -> Type -> a
compilePrimErr p ty = error $ "Impossible! compilePrim " ++ show p ++ " on bad type " ++ show ty

------------------------------------------------------------
-- Case expressions
------------------------------------------------------------

-- | Compile a desugared branch.  This does very little actual work, just
--   translating directly from one AST to another.
compileBranch :: Member Fresh r => DBranch -> Sem r CBranch
compileBranch b = do
  (gs, d) <- unbind b
  bind <$> traverseTelescope compileGuard gs <*> compileDTerm d

-- | Compile a desugared guard.
compileGuard :: Member Fresh r => DGuard -> Sem r (Embed Core, CPattern)
compileGuard (DGPat (unembed -> d) dpat) =
  (,)
    <$> (embed <$> compileDTerm d)
    <*> pure (compilePattern dpat)

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

-- | Compile a desugared pattern.
compilePattern :: DPattern -> CPattern
compilePattern (DPVar _ x)      = CPVar (coerce x)
compilePattern (DPWild _)       = CPWild
compilePattern DPUnit           = CPUnit
compilePattern (DPBool b)       = CPTag (toEnum . fromEnum $ b)
compilePattern (DPChar c)       = CPNat (toInteger $ fromEnum c)
compilePattern (DPPair _ x1 x2) = CPPair (coerce x1) (coerce x2)
compilePattern (DPInj _ s x)    = CPInj (toEnum . fromEnum $ s) (coerce x)
compilePattern (DPNat _ n)      = CPNat n
compilePattern (DPFrac _ x1 x2) = CPFrac (coerce x1) (coerce x2)
compilePattern (DPNil _)        = CPTag L

------------------------------------------------------------
-- Unary and binary operators
------------------------------------------------------------

-- | Compile a unary operator.
compileUOp
  :: Type   -- ^ Type of the operator argument
  -> UOp
  -> Core

compileUOp _ op = CConst (coreUOps ! op)
  where
    -- Just look up the corresponding core operator.
    coreUOps = M.fromList
      [ Neg      ==> ONeg
      , Fact     ==> OFact
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
-- compileBOp (TyFin n) _ _ op
--   | op `elem` [Div, Exp, Divides]
--   = CConst ((omOps ! op) n)
--   where
--     omOps = M.fromList
--       [ Div     ==> OMDiv
--       , Exp     ==> OMExp
--       , Divides ==> OMDivides
--       ]

-- Graph operations are separate, but use the same syntax, as traditional
-- addition and multiplication.
compileBOp (TyGraph _) (TyGraph _) (TyGraph a) op
  | op `elem` [Add, Mul]
  = CConst (regularOps ! op)
  where
    regularOps = M.fromList
      [ Add     ==> OOverlay a
      , Mul     ==> OConnect a
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

-- Likewise, ShouldEq also needs to know the type at which the
-- comparison is occurring.
compileBOp ty _ _ ShouldEq = CConst (OShouldEq ty)

compileBOp ty (TyList _) _ Elem = CConst (OListElem ty)
compileBOp ty _ _          Elem = CConst (OBagElem  ty)

compileBOp ty1 ty2 resTy op
  = error $ "Impossible! missing case in compileBOp: " ++ show (ty1, ty2, resTy, op)

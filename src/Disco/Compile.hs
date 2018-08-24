{-# LANGUAGE ViewPatterns #-}
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
import           Disco.Syntax.Operators
import           Disco.Typecheck.Monad
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
compileDTerm (DTVar _ x)  = return $ CVar (coerce x)
compileDTerm DTUnit       = return $ CCons 0 []
compileDTerm (DTBool b)   = return $ CCons (fromEnum b) []
compileDTerm (DTChar c)   = return $ CNum Fraction ((toInteger $ fromEnum c) % 1)
compileDTerm (DTNat ty n) = compileNat ty n
compileDTerm (DTRat r)    = return $ CNum Decimal r

compileDTerm (DTLam _ l) = do
  (x,body) <- unbind l
  c <- compileDTerm body
  return $ CAbs (bind (coerce x) c)

compileDTerm (DTApp ty t1 t2)
  = CApp (strictness ty) <$> compileDTerm t1 <*> compileDTerm t2

compileDTerm (DTPair _ t1 t2)
  = CCons 0 <$> mapM compileDTerm [t1,t2]

compileDTerm (DTInj _ s t)
  = CCons (fromEnum s) <$> mapM compileDTerm [t]

compileDTerm (DTCase _ bs)
  = CCase <$> mapM compileBranch bs

compileDTerm (DTUn _ op t)
  = compileUOp op <$> compileDTerm t

compileDTerm (DTBin ty op t1 t2)
  = compileBOp (getType t1) (getType t2) ty op <$> compileDTerm t1 <*> compileDTerm t2

compileDTerm (DTTyOp _ op ty) = return $ COp (tyOps ! op) [CType ty]
  where
    tyOps = M.fromList
      [ Enumerate ==> OEnum
      , Count     ==> OCount
      ]

compileDTerm (DTNil _)        = return $ CCons 0 []

compileDTerm (DTContainer _ ListContainer ds (Just ell))
  = CEllipsis <$> mapM compileDTerm ds <*> traverse compileDTerm ell

compileDTerm (DTContainer _ ListContainer _ Nothing)
  = error $ unlines
      [ "Impossible: compileDTerm on DTContainer ListContainer with no ellipsis"
      , "(should have already been desugared)"
      ]

compileDTerm (DTContainer (TySet eltTy) SetContainer ds Nothing)
  = CoreSet eltTy <$> mapM compileDTerm ds

compileDTerm (DTContainer (TySet _) SetContainer _ (Just _))
  = error $ "compileDTerm DTContainer SetContainer with ellipsis: unimplemented"

compileDTerm (DTContainer ty c _ _)
  = error $ "Impossible: compileDTerm on DTContiner " ++ show c ++ " with non-Set type " ++ show ty

------------------------------------------------------------

-- | Compile a natural number. A separate function is needed in
--   case the number is of a finite type, in which case we must
--   mod it by its type.
compileNat :: Type -> Integer -> FreshM Core
compileNat (TyFin n) x = return $ CNum Fraction ((x `mod` n) % 1)
compileNat _         x = return $ CNum Fraction (x % 1)

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

-- | Compile a unary operator application.
compileUOp :: UOp -> Core -> Core
compileUOp op c = COp (coreUOps ! op) [c]
  where
    -- Just look up the corresponding core operator.
    coreUOps = M.fromList $
      [ Neg   ==> ONeg
      , Fact  ==> OFact
      , Sqrt  ==> OSqrt
      , Lg    ==> OLg
      , Floor ==> OFloor
      , Ceil  ==> OCeil
      , Abs   ==> OAbs
      ]

-- | Compile a binary operator application.  This function needs to
--   know the types of the arguments and result since some operators
--   are overloaded and compile to different code depending on their
--   type.
--
--  @arg1 ty -> arg2 ty -> result ty -> op -> arg1 -> arg2 -> result@
compileBOp :: Type -> Type -> Type -> BOp -> Core -> Core -> Core

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
compileBOp (TyFin n) _ _ op c1 c2
  | op `elem` [Div, Exp, Divides]
  = COp ((omOps ! op) n) [c1, c2]
  where
    omOps = M.fromList
      [ Div     ==> OMDiv
      , Exp     ==> OMExp
      , Divides ==> OMDivides
      ]

-- Some regular arithmetic operations that just translate straightforwardly.
compileBOp _ _ _ op c1 c2
  | op `elem` [Add, Mul, Div, Exp, Mod, Divides, Choose]
  = COp (regularOps ! op) [c1, c2]
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
compileBOp ty _ _ Eq c1 c2 = COp (OEq ty) [c1, c2]
compileBOp ty _ _ Lt c1 c2 = COp (OLt ty) [c1, c2]

-- The Cons binary operator compiles to an application of a
-- constructor.
compileBOp _ _ _ Cons c1 c2 = CCons 1 [c1, c2]

-- Operations on sets compile straightforwardly, except that they also
-- need to store the element type so they can compare the elements
-- appropriately.
compileBOp (TySet ty) _ _ op c1 c2
  | op `elem` [Union, Intersection, Difference, Subset]
  = COp ((setOps ! op) ty) [c1, c2]
  where
    setOps = M.fromList
      [ Union        ==> OUnion
      , Intersection ==> OIntersection
      , Difference   ==> ODifference
      , Subset       ==> OSubset
      ]

compileBOp ty1 ty2 resTy op c1 c2
  = error $ "Impossible! missing case in compileBOp: " ++ show (ty1, ty2, resTy, op, c1, c2)

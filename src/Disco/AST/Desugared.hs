{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Desugared
-- Copyright   :  (c) 2018 disco team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Typed abstract syntax trees representing the typechecked, desugared
-- Disco language.
--
-----------------------------------------------------------------------------

module Disco.AST.Desugared
       ( -- * Desugared, type-annotated terms
       DTerm
       , pattern DTVar
       , pattern DTUnit
       , pattern DTBool
       , pattern DTNat
       , pattern DTRat
       , pattern DTLam
       , pattern DTApp
       , pattern DTPair
       , pattern DTInj
       , pattern DTCase
       , pattern DTUn
       , pattern DTBin
       , pattern DTTyOp
       , pattern DTContainer

       , Container(..)
       , DBinding
       , pattern DBinding
         -- * Branches and guards
       , DBranch

       , DGuard
       , pattern DGBool
       , pattern DGPat

       , DPattern
       , pattern DPVar
       , pattern DPWild
       , pattern DPUnit
       , pattern DPBool
       , pattern DPPair
       , pattern DPInj
       , pattern DPNat
       , pattern DPSucc
       , pattern DPCons
       , pattern DPNil

       , DProperty
       )
       where

import           Data.Void
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Generic
import           Disco.Syntax.Operators
import           Disco.Types

data DS

type DProperty = Property_ DS

-- | A @DTerm@ is a term which has been typechecked and desugared, so
--   it has fewer constructors and complex features than 'ATerm', but
--   still retains typing information.

type DTerm = Term_ DS

type instance X_TVar DS           = Type
type instance X_TLet DS           = Void -- Let gets translated to lambda
type instance X_TUnit DS          = ()
type instance X_TBool DS          = ()
type instance X_TNat DS           = Type
type instance X_TRat DS           = ()
type instance X_TAbs DS           = Void -- TAbs represents lambdas with multiple args;
                                         -- see TLam
type instance X_TApp DS           = Type
type instance X_TInj DS           = Type
type instance X_TCase DS          = Type
type instance X_TUn DS            = Type
type instance X_TBin DS           = Type
type instance X_TChain DS         = Void -- Chains are translated into conjunctions of
                                         -- binary comparisons
type instance X_TTyOp DS          = Type
type instance X_TContainer DS     = Type
type instance X_TContainerComp DS = Void -- Container comprehensions are translated
                                         -- into monadic chains
type instance X_TAscr DS          = Void -- No type ascriptions
type instance X_TTup DS           = Void -- No tuples, only pairs
type instance X_TParens DS        = Void -- No explicit parens

-- Extra constructors
type instance X_Term DS = Either
                            (DTerm, DTerm)                   -- TPair
                            (Type, Bind (Name DTerm) DTerm)  -- TLam

pattern DTVar :: Type -> Name DTerm -> DTerm
pattern DTVar ty name = TVar_ ty name

pattern DTUn :: Type -> UOp -> DTerm -> DTerm
pattern DTUn ty uop term = TUn_ ty uop term

pattern DTUnit :: DTerm
pattern DTUnit = TUnit_ ()

pattern DTBool :: Bool -> DTerm
pattern DTBool bool = TBool_ () bool

pattern DTNat  :: Type -> Integer -> DTerm
pattern DTNat ty int = TNat_ ty int

pattern DTRat :: Rational -> DTerm
pattern DTRat rat = TRat_ () rat

pattern DTLam :: Type -> Bind (Name DTerm) DTerm -> DTerm
pattern DTLam ty lam = XTerm_ (Right (ty, lam))

pattern DTApp  :: Type -> DTerm -> DTerm -> DTerm
pattern DTApp ty term1 term2 = TApp_ ty term1 term2

pattern DTPair :: DTerm -> DTerm -> DTerm
pattern DTPair t1 t2 = XTerm_ (Left (t1, t2))

pattern DTInj :: Type -> Side -> DTerm -> DTerm
pattern DTInj ty side term = TInj_ ty side term

pattern DTCase :: Type -> [DBranch] -> DTerm
pattern DTCase ty branch = TCase_ ty branch

pattern DTBin :: Type -> BOp -> DTerm -> DTerm -> DTerm
pattern DTBin ty bop term1 term2 = TBin_ ty bop term1 term2

pattern DTTyOp :: Type -> TyOp -> Type -> DTerm
pattern DTTyOp ty1 tyop ty2 = TTyOp_ ty1 tyop ty2

pattern DTContainer :: Type -> Container -> [DTerm] -> Maybe (Ellipsis DTerm) -> DTerm
pattern DTContainer ty c tl mets = TContainer_ ty c tl mets

{-# COMPLETE DTVar, DTUn, DTUnit, DTBool, DTNat, DTRat,
             DTLam, DTApp, DTPair, DTInj, DTCase, DTBin, DTTyOp,
             DTContainer #-}

type instance X_TLink DS = Void

type DBinding = Binding_ DS

pattern DBinding :: Maybe (Embed Sigma) -> Name DTerm -> Embed DTerm -> DBinding
pattern DBinding m b n = Binding_ m b n

{-# COMPLETE DBinding #-}

type DBranch = Bind (Telescope DGuard) DTerm

type DGuard = Guard_ DS

type instance X_GBool DS = ()
type instance X_GPat DS = ()

pattern DGBool :: Embed DTerm -> DGuard
pattern DGBool embedt = GBool_ () embedt

pattern DGPat :: Embed DTerm -> DPattern -> DGuard
pattern DGPat embedt pat = GPat_ () embedt pat

{-# COMPLETE DGBool, DGPat #-}

type DPattern = Pattern_ DS

type instance X_PVar     DS = ()
type instance X_PWild    DS = ()
type instance X_PUnit    DS = ()
type instance X_PBool    DS = ()
type instance X_PTup     DS = Void
type instance X_PInj     DS = ()
type instance X_PNat     DS = ()
type instance X_PSucc    DS = ()
type instance X_PCons    DS = ()
type instance X_PList    DS = Void

type instance X_Pattern  DS =
  Either
    (DPattern, DPattern)   -- DPair
    ()                     -- DNil

pattern DPVar :: Name DTerm -> DPattern
pattern DPVar name = PVar_ () name

pattern DPWild :: DPattern
pattern DPWild = PWild_ ()

pattern DPUnit :: DPattern
pattern DPUnit = PUnit_ ()

pattern DPBool :: Bool -> DPattern
pattern DPBool  b = PBool_ () b

pattern DPPair  :: DPattern -> DPattern -> DPattern
pattern DPPair p1 p2 = XPattern_ (Left (p1, p2))

pattern DPInj  :: Side -> DPattern -> DPattern
pattern DPInj s p = PInj_ () s p

pattern DPNat  :: Integer -> DPattern
pattern DPNat n = PNat_ () n

pattern DPSucc :: DPattern -> DPattern
pattern DPSucc p = PSucc_ () p

pattern DPCons :: DPattern -> DPattern -> DPattern
pattern DPCons  p1 p2 = PCons_ () p1 p2

pattern DPNil :: DPattern
pattern DPNil = XPattern_ (Right ())

{-# COMPLETE DPVar, DPWild, DPUnit, DPBool, DPPair, DPInj, DPNat,
    DPSucc, DPNil, DPCons #-}

------------------------------------------------------------
-- getType
------------------------------------------------------------

-- -- | Get the type at the root of a 'DTerm'.
-- getType :: DTerm -> Type
-- getType (ATVar ty _)             = ty
-- getType ATUnit                   = TyUnit
-- getType (ATBool _)               = TyBool
-- getType (ATNat ty _)             = ty
-- getType (ATRat _)                = TyF
-- getType (ATAbs ty _)             = ty
-- getType (ATApp ty _ _)           = ty
-- getType (ATTup ty _)             = ty
-- getType (ATInj ty _ _)           = ty
-- getType (ATUn ty _ _)            = ty
-- getType (ATBin ty _ _ _)         = ty
-- getType (ATTyOp ty _ _)          = ty
-- getType (ATChain ty _ _)         = ty
-- getType (ATContainer ty _ _ _)   = ty
-- getType (ATContainerComp ty _ _) = ty
-- getType (ATLet ty _)             = ty
-- getType (ATCase ty _)            = ty

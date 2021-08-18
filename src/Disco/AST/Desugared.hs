{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.AST.Desugared
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Typed abstract syntax trees representing the typechecked, desugared
-- Disco language.
--
-----------------------------------------------------------------------------

module Disco.AST.Desugared
       ( -- * Desugared, type-annotated terms
       DTerm
       , pattern DTVar
       , pattern DTPrim
       , pattern DTUnit
       , pattern DTBool
       , pattern DTChar
       , pattern DTNat
       , pattern DTRat
       , pattern DTAbs
       , pattern DTApp
       , pattern DTPair
       , pattern DTCase
       , pattern DTTyOp
       , pattern DTNil
       , pattern DTTest

       , Container(..)
       , DBinding
       , pattern DBinding
         -- * Branches and guards
       , DBranch

       , DGuard
       , pattern DGPat

       , DPattern
       , pattern DPVar
       , pattern DPWild
       , pattern DPUnit
       , pattern DPPair
       , pattern DPInj
       , pattern DPFrac
       , pattern DPNil

       , DProperty
       )
       where

import           GHC.Generics

import           Data.Void
import           Unbound.Generics.LocallyNameless

import           Disco.AST.Generic
import           Disco.Syntax.Operators
import           Disco.Syntax.Prims
import           Disco.Types

data DS

type DProperty = Property_ DS

-- | A @DTerm@ is a term which has been typechecked and desugared, so
--   it has fewer constructors and complex features than 'ATerm', but
--   still retains typing information.

type DTerm = Term_ DS

type instance X_Binder DS         = Name DTerm

type instance X_TVar DS           = Type
type instance X_TPrim DS          = Type
type instance X_TLet DS           = Void -- Let gets translated to lambda
type instance X_TUnit DS          = ()
type instance X_TBool DS          = Type
type instance X_TChar DS          = ()
type instance X_TString DS        = Void
type instance X_TNat DS           = Type
type instance X_TRat DS           = ()
type instance X_TAbs DS           = Type -- For lambas this is the function type but
                                         -- for forall/exists it's the argument type
type instance X_TApp DS           = Type
type instance X_TCase DS          = Type
type instance X_TChain DS         = Void -- Chains are translated into conjunctions of
                                         -- binary comparisons
type instance X_TTyOp DS          = Type
type instance X_TContainer DS     = Void -- Literal containers are desugared into
                                         -- conversion functions applied to list literals

type instance X_TContainerComp DS = Void -- Container comprehensions are translated
                                         -- into monadic chains

type instance X_TAscr DS          = Void -- No type ascriptions
type instance X_TTup DS           = Void -- No tuples, only pairs
type instance X_TParens DS        = Void -- No explicit parens

-- Extra constructors
type instance X_Term DS = X_DTerm

data X_DTerm
  = DTPair_ Type DTerm DTerm
  | DTNil_ Type
  | DTTest_ [(String, Type, Name DTerm)] DTerm
  deriving (Show, Generic)

instance Subst Type X_DTerm
instance Alpha X_DTerm

pattern DTVar :: Type -> Name DTerm -> DTerm
pattern DTVar ty name = TVar_ ty name

pattern DTPrim :: Type -> Prim -> DTerm
pattern DTPrim ty name = TPrim_ ty name

pattern DTUnit :: DTerm
pattern DTUnit = TUnit_ ()

pattern DTBool :: Type -> Bool -> DTerm
pattern DTBool ty bool = TBool_ ty bool

pattern DTNat  :: Type -> Integer -> DTerm
pattern DTNat ty int = TNat_ ty int

pattern DTRat :: Rational -> DTerm
pattern DTRat rat = TRat_ () rat

pattern DTChar :: Char -> DTerm
pattern DTChar c = TChar_ () c

pattern DTAbs :: Quantifier -> Type -> Bind (Name DTerm) DTerm -> DTerm
pattern DTAbs q ty lam = TAbs_ q ty lam

pattern DTApp  :: Type -> DTerm -> DTerm -> DTerm
pattern DTApp ty term1 term2 = TApp_ ty term1 term2

pattern DTPair :: Type -> DTerm -> DTerm -> DTerm
pattern DTPair ty t1 t2 = XTerm_ (DTPair_ ty t1 t2)

pattern DTCase :: Type -> [DBranch] -> DTerm
pattern DTCase ty branch = TCase_ ty branch

pattern DTTyOp :: Type -> TyOp -> Type -> DTerm
pattern DTTyOp ty1 tyop ty2 = TTyOp_ ty1 tyop ty2

pattern DTNil :: Type -> DTerm
pattern DTNil ty = XTerm_ (DTNil_ ty)

-- | A test frame, recording a collection of variables with their types and
--   their original user-facing names. Used for legible reporting of test
--   failures inside the enclosed term.
pattern DTTest :: [(String, Type, Name DTerm)] -> DTerm -> DTerm
pattern DTTest ns t = XTerm_ (DTTest_ ns t)

{-# COMPLETE DTVar, DTPrim, DTUnit, DTBool, DTChar, DTNat, DTRat,
             DTAbs, DTApp, DTPair, DTCase, DTTyOp,
             DTNil, DTTest #-}

type instance X_TLink DS = Void

type DBinding = Binding_ DS

pattern DBinding :: Maybe (Embed PolyType) -> Name DTerm -> Embed DTerm -> DBinding
pattern DBinding m b n = Binding_ m b n

{-# COMPLETE DBinding #-}

type DBranch = Bind (Telescope DGuard) DTerm

type DGuard = Guard_ DS

type instance X_GBool DS = Void   -- Boolean guards get desugared to pattern-matching
type instance X_GPat  DS = ()
type instance X_GLet  DS = Void   -- Let gets desugared to 'when' with a variable

pattern DGPat :: Embed DTerm -> DPattern -> DGuard
pattern DGPat embedt pat = GPat_ () embedt pat

{-# COMPLETE DGPat #-}

type DPattern = Pattern_ DS

type instance X_PVar     DS = Embed Type
type instance X_PWild    DS = Embed Type
type instance X_PAscr    DS = Void
type instance X_PUnit    DS = ()
type instance X_PBool    DS = Void
type instance X_PChar    DS = Void
type instance X_PString  DS = Void
type instance X_PTup     DS = Void
type instance X_PInj     DS = Void
type instance X_PNat     DS = Void
type instance X_PCons    DS = Void
type instance X_PList    DS = Void
type instance X_PAdd     DS = Void
type instance X_PMul     DS = Void
type instance X_PSub     DS = Void
type instance X_PNeg     DS = Void
type instance X_PFrac    DS = Void

-- In the desugared language, constructor patterns (DPPair, DPInj) can
-- only contain variables, not nested patterns.  This means that the
-- desugaring phase has to make explicit the order of matching by
-- exploding nested patterns into sequential guards, which makes the
-- interpreter simpler.

type instance X_Pattern  DS =
  Either
    (Embed Type, Name DTerm, Name DTerm)     -- DPPair
    (Either
      (Embed Type, Side, Name DTerm)         -- DPInj
      (Either
        (Embed Type, Name DTerm, Name DTerm) -- DPFrac
        (Embed Type)                         -- DNil
      )
    )

pattern DPVar :: Type -> Name DTerm -> DPattern
pattern DPVar ty name <- PVar_ (unembed -> ty) name
  where
    DPVar ty name = PVar_ (embed ty) name

pattern DPWild :: Type -> DPattern
pattern DPWild ty <- PWild_ (unembed -> ty)
  where
    DPWild ty = PWild_ (embed ty)

pattern DPUnit :: DPattern
pattern DPUnit = PUnit_ ()

pattern DPPair  :: Type -> Name DTerm -> Name DTerm -> DPattern
pattern DPPair ty x1 x2 <- XPattern_ (Left (unembed -> ty, x1, x2))
  where
    DPPair ty x1 x2 = XPattern_ (Left (embed ty, x1, x2))

pattern DPInj  :: Type -> Side -> Name DTerm -> DPattern
pattern DPInj ty s x <- XPattern_ (Right (Left (unembed -> ty, s, x)))
  where
    DPInj ty s x = XPattern_ (Right (Left (embed ty, s, x)))

pattern DPFrac :: Type -> Name DTerm -> Name DTerm -> DPattern
pattern DPFrac ty x1 x2 <- XPattern_ (Right (Right (Left (unembed -> ty, x1, x2))))
  where
    DPFrac ty x1 x2 = XPattern_ (Right (Right (Left (embed ty, x1, x2))))

pattern DPNil :: Type -> DPattern
pattern DPNil ty <- XPattern_ (Right (Right (Right (unembed -> ty))))
  where
    DPNil ty = XPattern_ (Right (Right (Right (embed ty))))

{-# COMPLETE DPVar, DPWild, DPUnit, DPPair, DPInj, DPFrac, DPNil #-}

type instance X_QBind  DS = Void
type instance X_QGuard DS = Void

------------------------------------------------------------
-- getType
------------------------------------------------------------

instance HasType DTerm where
  getType (DTVar ty _)     = ty
  getType (DTPrim ty _)    = ty
  getType DTUnit           = TyUnit
  getType (DTBool ty _)    = ty
  getType (DTChar _)       = TyC
  getType (DTNat ty _)     = ty
  getType (DTRat _)        = TyF
  getType (DTAbs Lam ty _) = ty
  getType DTAbs{}          = TyProp
  getType (DTApp ty _ _)   = ty
  getType (DTPair ty _ _)  = ty
  getType (DTCase ty _)    = ty
  getType (DTTyOp ty _ _)  = ty
  getType (DTNil ty)       = ty
  getType (DTTest _ _)     = TyProp

instance HasType DPattern where
  getType (DPVar ty _)    = ty
  getType (DPWild ty)     = ty
  getType DPUnit          = TyUnit
  getType (DPPair ty _ _) = ty
  getType (DPInj ty _ _)  = ty
  getType (DPFrac ty _ _) = ty
  getType (DPNil ty)      = ty

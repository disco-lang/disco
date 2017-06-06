{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- A Haskell version of Typecheck.agda.  We have no dependent types
-- here to ensure we are doing it properly, which is why it's helpful
-- to model it on the Agda version.

module Typecheck where

import qualified Data.Map as M

------------------------------------------------------------
-- Types and type equality
------------------------------------------------------------

data Type where
  Nat  :: Type
  (:=>) :: Type -> Type -> Type

infixr 0 :=>

-- In general, (m :: Mismatch) proves that (mismatchL m) ~/~ (mismatchR m)
data Mismatch where
  -- NatArr t1 t2 means "Nat ~/~ (t1 :=> t2)"
  NatArr :: Type -> Type -> Mismatch

  -- ArrL m t3 t4 means (t1 :=> t3) ~/~ (t2 :=> t4) where m proves t1 ~/~ t2
  ArrL   :: Mismatch -> Type -> Type -> Mismatch

  -- ArrR t1 t2 m  means (t1 :=> t3) ~/~ (t2 :=> t4) where m proves t3 ~/~ t4
  ArrR   :: Type -> Type -> Mismatch -> Mismatch

  -- Sym m  means t2 ~/~ t1  where m proves  t1 ~/~ t2
  Sym    :: Mismatch -> Mismatch

mismatchL :: Mismatch -> Type
mismatchL (NatArr _ _)   = Nat
mismatchL (ArrL m t3 _ ) = mismatchL m :=> t3
mismatchL (ArrR t1 _  m) = t1 :=> mismatchL m
mismatchL (Sym m)        = mismatchR m

mismatchR :: Mismatch -> Type
mismatchR (NatArr t1 t2) = t1 :=> t2
mismatchR (ArrL m _  t4) = mismatchR m :=> t4
mismatchR (ArrR _  t2 m) = t2 :=> mismatchR m
mismatchR (Sym m)        = mismatchL m

-- Nothing means the types match.
(~?) :: Type -> Type -> Maybe Mismatch
Nat ~? Nat = Nothing
Nat ~? (t1 :=> t2) = Just (NatArr t1 t2)
(t1 :=> t2) ~? Nat = Just $ Sym (NatArr t1 t2)
(t1 :=> t2) ~? (t3 :=> t4) =
  case (t1 ~? t3, t2 ~? t4) of
    (Nothing, Nothing) -> Nothing
    (Just m, _)        -> Just (ArrL m t3 t4)
    (_, Just m)        -> Just (ArrR t1 t2 m)

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

data Expr where
  Lit  :: Int -> Expr
  Plus :: Expr -> Expr -> Expr
  Var  :: String -> Expr
  Lam  :: String -> Type -> Expr -> Expr
  App  :: Expr -> Expr -> Expr

type Ctx = M.Map String Type

------------------------------------------------------------
-- Typing derivations
------------------------------------------------------------

-- This corresponds to
--   data _⊢_∶_ : ∀ {n} → Ctx n → Expr n → Type → Set
-- in Agda.  As a general rule, value indices in Agda are translated
-- into projection functions.  That is, given a type (T :: I -> Set)
-- in Agda, we translate it into an unindexed type T in Haskell along
-- with a function getI :: T -> I such that (t :: T i) in Agda
-- corresponds to some value (t :: T) in Haskell such that getI t ==
-- i.

-- | A typing derivation.  d :: Typing is a constructive proof that
--   @typingCtx d ⊢ typingExpr d : typingType d@.
data Typing where

  -- Γ ⊢ lit m : Nat
  TLit :: Ctx -> Int -> Typing

  -- D1 = Γ ⊢ t1 : Nat     D2 = Γ ⊢ t2 : Nat
  -- ---------------------------------------
  --      Γ ⊢ (t1 + t2) : Nat
  --
  -- TPlus Γ D1 D2
  TPlus :: Ctx -> Typing -> Typing -> Typing

  -- x : τ ∈ Γ
  -- ---------
  -- Γ ⊢ x : τ
  --
  -- Invariant (of course): the String is a valid key of the context
  --
  -- TVar Γ x
  TVar :: Ctx -> String -> Typing

  --    D = Γ, x:τ₁ ⊢ t : τ₂
  -- -------------------------
  -- Γ ⊢ λ x:τ₁ . t : τ₁ -> τ₂
  --
  -- TLam Γ x τ₁ D
  TLam :: Ctx -> String -> Type -> Typing -> Typing

  -- D1 = Γ ⊢ t₁ : τ₁ → τ₂     D2 = Γ ⊢ t₂ : τ₁
  -- ------------------------------------------
  --              Γ ⊢ t₁ t₂ : τ₂
  --
  -- TApp Γ D1 D2 τ₂
  TApp :: Ctx -> Typing -> Typing -> Type -> Typing

-- | Extract the context from a typing derivation.
typingCtx :: Typing -> Ctx
typingCtx (TLit g _)     = g
typingCtx (TPlus g _ _)  = g
typingCtx (TVar g _)     = g
typingCtx (TLam g _ _ _) = g
typingCtx (TApp g _ _ _) = g

-- | Extract the expression from a typing derivation.
typingExpr :: Typing -> Expr
typingExpr (TLit _ m)       = Lit m
typingExpr (TPlus _ t1 t2)  = Plus (typingExpr t1) (typingExpr t2)
typingExpr (TVar _ x)       = Var x
typingExpr (TLam _ x ty1 d) = Lam x ty1 (typingExpr d)
typingExpr (TApp _ d1 d2 _) = App (typingExpr d1) (typingExpr d2)

-- | Extract the type from a typing derivation.
typingType :: Typing -> Type
typingType (TLit _ _)       = Nat
typingType (TPlus _ _ _)    = Nat
typingType (TVar g x)       = g M.! x
typingType (TLam _ _ ty1 d) = ty1 :=> typingType d
typingType (TApp _ _ _ ty)  = ty

------------------------------------------------------------
-- Untyping derivations
------------------------------------------------------------

-- | A proof of untypability.  A value u :: Untyping is a constructive
--   proof that  @untypingCtx u ⊬ untypingExpr u : untypingType u@.
data Untyping where

  -- D = Γ ⊢ t : τ₁    M = τ₁ ≁ τ₂
  -- -----------------------------
  --          Γ ⊬ t : τ₂
  --
  -- UMismatch Γ D M
  UMismatch :: Ctx -> Typing -> Mismatch -> Untyping

  -- D = Γ ⊬ t₁ : Nat
  -- ----------------
  -- Γ ⊬ t₁ + t₂ : τ
  --
  -- UPlusL Γ D t₂ τ
  UPlusL :: Ctx -> Untyping -> Expr -> Type -> Untyping

  -- D = Γ ⊬ t₂ : Nat
  -- ----------------
  -- Γ ⊬ t₁ + t₂ : τ
  --
  -- UPlusL Γ t₁ D τ
  UPlusR :: Ctx -> Expr -> Untyping -> Type -> Untyping

  --   M = τ ≁ Nat
  -- ---------------
  -- Γ ⊬ t₁ + t₂ : τ
  --
  -- UPlusNat Γ M t₁ t₂
  UPlusNat :: Ctx -> Mismatch -> Expr -> Expr -> Untyping

  -- M : ∀ τ₂. τ ≁ (τ₁ → τ₂)
  -- -----------------------
  --    Γ ⊬ λx:τ₁. t : τ
  --
  -- ULamFunTy Γ M x τ₁ t
  ULamFunTy :: Ctx -> (Type -> Mismatch) -> String -> Type -> Expr -> Untyping

  --  U = Γ, x:τ₁ ⊬ t : τ₂
  -- ----------------------
  -- Γ ⊬ λx:τ₁. t : τ₁ → τ₂
  --
  -- ULam Γ U x τ₁
  ULam :: Ctx -> Untyping -> String -> Type -> Untyping

  -- U = ∀ τ₁. Γ ⊬ t₁ : τ₁ → τ₂
  -- --------------------------
  --      Γ ⊬ t₁ t₂ : τ₂
  --
  -- UAppFun Γ U t₂ τ₂
  UAppFun :: Ctx -> (Type -> Untyping) -> Expr -> Type -> Untyping

  -- D = Γ ⊢ t₁ : τ₁ → τ₂     U = Γ ⊬ t₂ : τ₁
  -- ----------------------------------------
  --            Γ ⊬ t₁ t₂ : τ₂
  -- UAppArg Γ D U τ₂
  UAppArg :: Ctx -> Typing -> Untyping -> Type -> Untyping

untypingCtx :: Untyping -> Ctx
untypingCtx (UMismatch g _ _)     = g
untypingCtx (UPlusL g _ _ _)      = g
untypingCtx (UPlusR g _ _ _)      = g
untypingCtx (UPlusNat g _ _ _)    = g
untypingCtx (ULamFunTy g _ _ _ _) = g
untypingCtx (ULam g _ _ _)        = g
untypingCtx (UAppFun g _ _ _)     = g
untypingCtx (UAppArg g _ _ _)     = g

untypingExpr :: Untyping -> Expr
untypingExpr (UMismatch _ d _)       = typingExpr d
untypingExpr (UPlusL _ d t2 _)       = Plus (untypingExpr d) t2
untypingExpr (UPlusR _ t1 d _)       = Plus t1 (untypingExpr d)
untypingExpr (UPlusNat _ _ t1 t2)    = Plus t1 t2
untypingExpr (ULamFunTy _ _ x ty1 t) = Lam x ty1 t
untypingExpr (ULam _ u x ty1)        = Lam x ty1 (untypingExpr u)
untypingExpr (UAppFun _ u t2 _)      = App (untypingExpr (u Nat)) t2
untypingExpr (UAppArg _ d u _)       = App (typingExpr d) (untypingExpr u)

untypingType :: Untyping -> Type
untypingType (UMismatch _ _ m)     = mismatchR m
untypingType (UPlusL _ _ _ ty)     = ty
untypingType (UPlusR _ _ _ ty)     = ty
untypingType (UPlusNat _ m _ _)    = mismatchL m
untypingType (ULamFunTy _ m _ _ _) = mismatchL (m Nat)
untypingType (ULam _ u _ ty1)      = ty1 :=> untypingType u
untypingType (UAppFun _ _ _ ty2)   = ty2
untypingType (UAppArg _ _ _ ty2)   = ty2

------------------------------------------------------------
-- Type inference
------------------------------------------------------------

infer :: Ctx -> Expr -> Either (Type -> Untyping) (Type, Typing)
infer g t@(Lit n)    = Right (Nat, TLit g n)
infer g (Plus t1 t2) =
  case (infer g t1, infer g t2) of
    (Right (Nat, d1), Right (Nat, d2)) -> Right (Nat, TPlus g d1 d2)


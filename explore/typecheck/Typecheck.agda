module Typecheck where

open import Relation.Binary.PropositionalEquality

open import Function using (id)
open import Data.Empty
open import Relation.Nullary
open import Data.Nat
open import Data.Fin
open import Data.Vec
open import Data.Sum
open import Data.Product

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- A universe of types for STLC + natural numbers.
data Type : Set where
  Nat : Type
  _⇒_ : Type → Type → Type

infixr 80 _⇒_

-- The function type constructor is injective.
⇒-inj : ∀ {τ₁ τ₂ τ₃ τ₄} → (τ₁ ⇒ τ₂ ≡ τ₃ ⇒ τ₄) → (τ₁ ≡ τ₃) × (τ₂ ≡ τ₄)
⇒-inj refl = refl , refl

-- Equality of types is decidable.
_≡?_ : (τ₁ τ₂ : Type) → (τ₁ ≡ τ₂) ⊎ (τ₁ ≢ τ₂)
Nat ≡? Nat = inj₁ refl
Nat ≡? (τ₂ ⇒ τ₃) = inj₂ (λ ())
(τ₁ ⇒ τ₂) ≡? Nat = inj₂ (λ ())
(τ₁ ⇒ τ₂) ≡? (τ₃ ⇒ τ₄) with τ₁ ≡? τ₃ | τ₂ ≡? τ₄
(τ₁ ⇒ τ₂) ≡? (.τ₁ ⇒ .τ₂) | inj₁ refl | inj₁ refl = inj₁ refl
(τ₁ ⇒ τ₂) ≡? (τ₃ ⇒ τ₄) | inj₂ τ₁≢τ₃ | _ = inj₂ (λ eq → τ₁≢τ₃ (proj₁ (⇒-inj eq)))
(τ₁ ⇒ τ₂) ≡? (τ₃ ⇒ τ₄) | _ | inj₂ τ₂≢τ₄ = inj₂ (λ eq → τ₂≢τ₄ (proj₂ (⇒-inj eq)))

≢-cong-⇒ : ∀ {τ₁ τ₂ τ₃ τ₄} → (τ₁ ⇒ τ₂ ≢ τ₃ ⇒ τ₄) → (τ₁ ≢ τ₃) ⊎ (τ₂ ≢ τ₄)
≢-cong-⇒ {τ₁} {τ₂} {τ₃} {τ₄} neq with τ₁ ≡? τ₃ | τ₂ ≡? τ₄
≢-cong-⇒ neq | inj₂ τ₁≢τ₃ | _ = inj₁ τ₁≢τ₃
≢-cong-⇒ neq | _ | inj₂ τ₂≢τ₄ = inj₂ τ₂≢τ₄
≢-cong-⇒ neq | inj₁ refl | inj₁ refl = ⊥-elim (neq refl)

-- A type of explicit evidence explaining *why* two types are unequal.
data _≁_ : Type → Type → Set where
  Nat≁⇒ : ∀ {τ₁ τ₂} → Nat ≁ (τ₁ ⇒ τ₂)
  ⇒ˡ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₁ ≁ τ₂ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)
  ⇒ʳ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₃ ≁ τ₄ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)

  ≁-sym   : ∀ {τ₁ τ₂} → τ₁ ≁ τ₂ → τ₂ ≁ τ₁

-- Given such a proof we can show the types are unequal.
≁-≢ : ∀ {τ₁ τ₂} → (τ₁ ≁ τ₂) → (τ₁ ≢ τ₂)
≁-≢ Nat≁⇒ = λ ()
≁-≢ (⇒ˡ-≁ τ₁≁τ₂)  refl = ≁-≢ τ₁≁τ₂ refl
≁-≢ (⇒ʳ-≁ τ₃≁τ₄)  refl = ≁-≢ τ₃≁τ₄ refl
≁-≢ (≁-sym τ₂≁τ₁) refl = ≁-≢ τ₂≁τ₁ refl

-- Since our universe of types is closed, we can actually go the other
-- way too. That is, ≢ is equivalent to ≁ ; the point is that the
-- latter is more immediately informative (by pattern-matching etc.)
-- which can be used to produce error messages and so on.
--
-- Note, however, that there might be *multiple* terms of type τ₁ ≁
-- τ₂: each corresponds to a different explanation of why the types
-- are not equal.  We might actually care which one we have.
-- Round-tripping through (τ₁ ≢ τ₂) is not the identity.

≢-≁ : ∀ {τ₁ τ₂} → (τ₁ ≢ τ₂) → (τ₁ ≁ τ₂)
≢-≁ {Nat} {Nat} τ₁≢τ₂ with τ₁≢τ₂ refl
... | ()
≢-≁ {Nat} {τ₂ ⇒ τ₃} _ = Nat≁⇒
≢-≁ {τ₁ ⇒ τ₂} {Nat} _ = ≁-sym Nat≁⇒
≢-≁ {τ₁ ⇒ τ₂} {τ₃ ⇒ τ₄} τ₁⇒τ₂≢τ₃⇒τ₄ with ≢-cong-⇒ τ₁⇒τ₂≢τ₃⇒τ₄
≢-≁ {τ₁ ⇒ τ₂} {τ₃ ⇒ τ₄} τ₁⇒τ₂≢τ₃⇒τ₄ | inj₁ τ₁≢τ₃ = ⇒ˡ-≁ (≢-≁ τ₁≢τ₃)
≢-≁ {τ₁ ⇒ τ₂} {τ₃ ⇒ τ₄} τ₁⇒τ₂≢τ₃⇒τ₄ | inj₂ τ₂≢τ₄ = ⇒ʳ-≁ (≢-≁ τ₂≢τ₄)

-- Sometimes it's convenient to decide equality of types using ≁ in place of ≢.
_∼?_ : (τ₁ τ₂ : Type) → (τ₁ ≡ τ₂) ⊎ (τ₁ ≁ τ₂)
τ₁ ∼? τ₂ = Data.Sum.map id ≢-≁ (τ₁ ≡? τ₂)

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- (Untyped) expressions of STLC + arithmetic.
data Expr (n : ℕ) : Set where
  lit : ℕ → Expr n
  _⊕_ : Expr n → Expr n → Expr n

  var : Fin n → Expr n
  ƛ   : Type → Expr (suc n) → Expr n
  _·_ : Expr n → Expr n → Expr n

Ctx : ℕ → Set
Ctx n = Vec Type n

------------------------------------------------------------
-- Typing
------------------------------------------------------------

-- Typing derivations.
data _⊢_∶_ : ∀ {n} → Ctx n → Expr n → Type → Set where
  lit  : ∀ {n} {Γ : Ctx n} {m}
       → Γ ⊢ lit m ∶ Nat
  _⊕_  : ∀ {n} {Γ : Ctx n} {t₁ t₂}
       → Γ ⊢ t₁        ∶ Nat
       → Γ ⊢ t₂        ∶ Nat
       → Γ ⊢ (t₁ ⊕ t₂) ∶ Nat
  var  : ∀ {n} {Γ : Ctx n} {i}
       → Γ ⊢ var i ∶ lookup i Γ
  ƛ    : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
       → (τ₁ ∷ Γ) ⊢ t ∶ τ₂
       → Γ ⊢ ƛ τ₁ t ∶ (τ₁ ⇒ τ₂)
  _·_  : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ₁ τ₂}
       → Γ ⊢ t₁ ∶ τ₁ ⇒ τ₂
       → Γ ⊢ t₂ ∶ τ₁
       → Γ ⊢ t₁ · t₂ ∶ τ₂

-- Explicit, constructive evidence for the *untypability* of a term.
data _⊬_∶_ : ∀ {n} → Ctx n → Expr n → Type → Set where

  -- Explicitly build in uniqueness of typing as an axiom.  t is not
  -- typeable at type τ₂ if t is typeable at some different type.
  mismatch : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
             → Γ ⊢ t ∶ τ₁
             → τ₁ ≁ τ₂
             → Γ ⊬ t ∶ τ₂

  -- There are three ways for a + term to fail to have a given type τ:
  -- either the left or right sides do not have type Nat, or the type
  -- τ itself is not Nat.
  ⊕ˡ       : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ}
             → Γ ⊬ t₁ ∶ Nat
             → Γ ⊬ (t₁ ⊕ t₂) ∶ τ
  ⊕ʳ       : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ}
             → Γ ⊬ t₂ ∶ Nat
             → Γ ⊬ (t₁ ⊕ t₂) ∶ τ
  ⊕≁Nat    : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ}
             → τ ≁ Nat → Γ ⊬ (t₁ ⊕ t₂) ∶ τ

  -- ƛ-funty holds if τ is not a function type at all, or if it is a
  -- function type whose input type is not τ₁.
  ƛ-funty  : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ}
             → (∀ {τ₂} → τ ≁ τ₁ ⇒ τ₂)
             → Γ ⊬ ƛ τ₁ t ∶ τ

  -- Otherwise, τ is of the form (τ₁ ⇒ τ₂) but the body t does not
  -- have type τ₂.  Note this could be either because t is not typable
  -- at all, or because it has some type other than τ₂.
  ƛ        : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
             → (τ₁ ∷ Γ) ⊬ t ∶ τ₂
             → Γ ⊬ ƛ τ₁ t ∶ (τ₁ ⇒ τ₂)

  -- Had this ƛ-resty constructor, but it turns out we don't need it:
  -- it is not used in inference or checking, and isn't needed to
  -- prove equivalence of ⊬ and ¬ ⊢ .  It handles *only* the case
  -- where t is typable but has a type different than the output type
  -- of the whole expression; but the ƛ constructor handles this case
  -- as well as the case where t is not typeable at all.

  -- ƛ-resty  : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂ τ₃}
  --            → (τ₁ ∷ Γ) ⊢ t ∶ τ₂
  --            → τ₂ ≁ τ₃
  --            → Γ ⊬ ƛ τ₁ t ∶ τ₁ ⇒ τ₃

  -- Finally, there are two cases when an application is not typeable.
  -- Either the function does not have an appropriate function type,
  -- or the argument does not have a type that matches the function's
  -- input type.
  ·-fun    : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ₂}
             → (∀ {τ₁} → Γ ⊬ t₁ ∶ τ₁ ⇒ τ₂)
             → Γ ⊬ t₁ · t₂ ∶ τ₂
  ·-arg    : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ₁ τ₂}
             → Γ ⊢ t₁ ∶ τ₁ ⇒ τ₂
             → Γ ⊬ t₂ ∶ τ₁
             → Γ ⊬ t₁ · t₂ ∶ τ₂

------------------------------------------------------------
-- Type inference and checking
------------------------------------------------------------

-- Type inference for a term in a given context returns either a type
-- and a valid typing derivation, or a constructive proof that the
-- term has no type.  Note that in this system, ALL terms can be
-- inferred.  In a bidirectional system we would have to restrict this
-- to only take inferrable terms as inputs.
infer : ∀ {n} → (Γ : Ctx n) → (t : Expr n) → (∃ λ τ → Γ ⊢ t ∶ τ) ⊎ (∀ τ → Γ ⊬ t ∶ τ)
infer Γ (lit n)   = inj₁ (Nat , lit)
infer Γ (t₁ ⊕ t₂) with infer Γ t₁ | infer Γ t₂
infer Γ (t₁ ⊕ t₂) | inj₁ (Nat , Γ⊢t₁∶Nat) | inj₁ (Nat , Γ⊢t₂∶Nat) = inj₁ (Nat , (Γ⊢t₁∶Nat ⊕ Γ⊢t₂∶Nat))
infer Γ (t₁ ⊕ t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₁ _ = inj₂ (λ _ → ⊕ˡ (mismatch Γ⊢t₁∶τ₁⇒τ₂ (≁-sym Nat≁⇒)))
infer Γ (t₁ ⊕ t₂) | inj₁ _ | inj₁ (τ₃ ⇒ τ₄ , Γ⊢t₂∶τ₃⇒τ₄) = inj₂ (λ _ → ⊕ʳ (mismatch Γ⊢t₂∶τ₃⇒τ₄ (≁-sym Nat≁⇒)))
infer Γ (t₁ ⊕ t₂) | inj₂ Γ⊬t₁∶ | _ = inj₂ (λ _ → ⊕ˡ (Γ⊬t₁∶ Nat))
infer Γ (t₁ ⊕ t₂) | _ | inj₂ Γ⊬t₂∶ = inj₂ (λ _ → ⊕ʳ (Γ⊬t₂∶ Nat))
infer Γ (var i)   = inj₁ (lookup i Γ , var)
infer Γ (ƛ τ₁ t) with infer (τ₁ ∷ Γ) t
infer Γ (ƛ τ₁ t) | inj₁ (τ₂ , τ₁∷Γ⊢t∶τ₂) = inj₁ (τ₁ ⇒ τ₂ , ƛ τ₁∷Γ⊢t∶τ₂)
infer Γ (ƛ τ₁ t) | inj₂ τ₁∷Γ⊬t∶ = inj₂ lemma
  where
    lemma : (τ : Type) → Γ ⊬ ƛ τ₁ t ∶ τ
    lemma Nat        = ƛ-funty Nat≁⇒
    lemma (τ₁′ ⇒ τ₂) with τ₁′ ∼? τ₁
    lemma (.τ₁ ⇒ τ₂) | inj₁ refl = ƛ (τ₁∷Γ⊬t∶ τ₂)
    lemma (τ₁′ ⇒ τ₂) | inj₂ τ₁′≁τ₁ = ƛ-funty (λ {τ₃} → ⇒ˡ-≁ τ₁′≁τ₁)
infer Γ (t₁ · t₂) with infer Γ t₁ | infer Γ t₂
infer Γ (t₁ · t₂) | inj₁ (Nat , Γ⊢t₁∶Nat) | _ = inj₂ (λ _ → ·-fun (mismatch Γ⊢t₁∶Nat Nat≁⇒))
infer Γ (t₁ · t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₁ (τ₁′ , Γ⊢t₂∶τ₁′) with τ₁ ∼? τ₁′
infer Γ (t₁ · t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₁ (.τ₁ , Γ⊢t₂∶τ₁ ) | inj₁ refl = inj₁ (τ₂ , (Γ⊢t₁∶τ₁⇒τ₂ · Γ⊢t₂∶τ₁))
infer Γ (t₁ · t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₁ (τ₁′ , Γ⊢t₂∶τ₁′) | inj₂ τ₁≁τ₁′ = inj₂ lemma2
  where
    lemma2 : ∀ τ → Γ ⊬ t₁ · t₂ ∶ τ
    lemma2 τ with τ ∼? τ₂
    lemma2 τ | inj₁ τ≡τ₂ rewrite τ≡τ₂ = ·-arg Γ⊢t₁∶τ₁⇒τ₂ (mismatch Γ⊢t₂∶τ₁′ (≁-sym τ₁≁τ₁′))
    lemma2 τ | inj₂ τ≁τ₂ = ·-fun (mismatch Γ⊢t₁∶τ₁⇒τ₂ (⇒ʳ-≁ (≁-sym τ≁τ₂)))
infer Γ (t₁ · t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₂ Γ⊬t₂ = inj₂ lemma3
  where
    lemma3 : ∀ τ → Γ ⊬ t₁ · t₂ ∶ τ
    lemma3 τ with τ ∼? τ₂
    lemma3 τ | inj₁ τ≡τ₂ rewrite τ≡τ₂ = ·-arg Γ⊢t₁∶τ₁⇒τ₂ (Γ⊬t₂ τ₁)
    lemma3 τ | inj₂ τ≁τ₂ = ·-fun (mismatch Γ⊢t₁∶τ₁⇒τ₂ (⇒ʳ-≁ (≁-sym τ≁τ₂)))
infer Γ (t₁ · t₂) | inj₂ Γ⊬t₁∶ | _ = inj₂ (λ τ₂ → ·-fun (λ {τ₁} → Γ⊬t₁∶ (τ₁ ⇒ τ₂)))

-- Check whether a given term has a *given* type.
check : ∀ {n} → (Γ : Ctx n) → (t : Expr n) → (τ : Type) → (Γ ⊢ t ∶ τ) ⊎ (Γ ⊬ t ∶ τ)
check Γ (lit _)   (τ ⇒ τ₁) = inj₂ (mismatch lit Nat≁⇒)
check Γ (lit _)   Nat      = inj₁ lit
check Γ (_  ⊕ _ ) (τ ⇒ τ₁) = inj₂ (⊕≁Nat (≁-sym Nat≁⇒))
check Γ (t₁ ⊕ t₂) Nat with check Γ t₁ Nat | check Γ t₂ Nat
check Γ (t₁ ⊕ t₂) Nat | inj₁ Γ⊢t₁∶Nat | inj₁ Γ⊢t₂∶Nat = inj₁ (Γ⊢t₁∶Nat ⊕ Γ⊢t₂∶Nat)
check Γ (t₁ ⊕ t₂) Nat | inj₂ Γ⊬t₁∶Nat | _             = inj₂ (⊕ˡ Γ⊬t₁∶Nat)
check Γ (t₁ ⊕ t₂) Nat | _             | inj₂ Γ⊬t₂∶Nat = inj₂ (⊕ʳ Γ⊬t₂∶Nat)
check Γ (var i)   τ with τ ∼? lookup i Γ
check Γ (var i) τ | inj₁ τ≡iΓ rewrite τ≡iΓ = inj₁ var
check Γ (var i) τ | inj₂ τ≁iΓ = inj₂ (mismatch var (≁-sym τ≁iΓ))
check Γ (ƛ τ₁ t) Nat = inj₂ (ƛ-funty Nat≁⇒)
check Γ (ƛ τ₁ t) (τ ⇒ τ₂) with τ ∼? τ₁ | check (τ₁ ∷ Γ) t τ₂
check Γ (ƛ τ₁ t) (τ ⇒ τ₂) | inj₂ τ≁τ₁ | _ = inj₂ (ƛ-funty (⇒ˡ-≁ τ≁τ₁))
check Γ (ƛ τ₁ t) (.τ₁ ⇒ τ₂) | inj₁ refl | inj₂ τ₁∷Γ⊬t∶τ₂ = inj₂ (ƛ τ₁∷Γ⊬t∶τ₂)
check Γ (ƛ τ₁ t) (.τ₁ ⇒ τ₂) | inj₁ refl | inj₁ τ₁∷Γ⊢t∶τ₂ = inj₁ (ƛ τ₁∷Γ⊢t∶τ₂)
  --- Note that in order to check an application we have to use type inference on t₁.
check Γ (t₁ · t₂) τ with infer Γ t₁
check Γ (t₁ · t₂) τ | inj₂ Γ⊬t₁∶ = inj₂ (·-fun (λ {τ₁} → Γ⊬t₁∶ (τ₁ ⇒ τ)))
check Γ (t₁ · t₂) τ | inj₁ (Nat , Γ⊢t₁∶τ₁) = inj₂ (·-fun (mismatch Γ⊢t₁∶τ₁ Nat≁⇒))
check Γ (t₁ · t₂) τ | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁) with τ ∼? τ₂
check Γ (t₁ · t₂) τ | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁) | inj₂ τ≁τ₂ = inj₂ (·-fun (mismatch Γ⊢t₁∶τ₁ (⇒ʳ-≁ (≁-sym τ≁τ₂))))
check Γ (t₁ · t₂) τ | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁) | inj₁ τ≡τ₂ rewrite τ≡τ₂ with check Γ t₂ τ₁
check Γ (t₁ · t₂) τ | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁) | inj₁ τ≡τ₂ | inj₂ Γ⊬t₂∶τ₁ = inj₂ (·-arg Γ⊢t₁∶τ₁ Γ⊬t₂∶τ₁)
check Γ (t₁ · t₂) τ | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁) | inj₁ τ≡τ₂ | inj₁ Γ⊢t₂∶τ₁ = inj₁ (Γ⊢t₁∶τ₁ · Γ⊢t₂∶τ₁)

------------------------------------------------------------
-- Correctness
------------------------------------------------------------

-- The whole idea is that ⊬ is a more explicit/constructive, yet
-- equivalent, way to represent the negation of ⊢ .  We can actually
-- prove the equivalence.

-- First, straightforward induction on typing derivations shows that
-- we really do have unique types, as assumed by the 'mismatch'
-- constructor.

⊢-unique : ∀ {n} {Γ : Ctx n} {t : Expr n} {τ₁ τ₂ : Type} → (Γ ⊢ t ∶ τ₁) → (Γ ⊢ t ∶ τ₂) → (τ₁ ≡ τ₂)
⊢-unique lit lit = refl
⊢-unique (_ ⊕ _) (_ ⊕ _) = refl
⊢-unique var var = refl
⊢-unique (ƛ Γ⊢t∶τ₁) (ƛ Γ⊢t∶τ₂) rewrite ⊢-unique Γ⊢t∶τ₁ Γ⊢t∶τ₂ = refl
⊢-unique (Γ⊢t∶τ₁ · _) (Γ⊢t∶τ₂ · _) = proj₂ (⇒-inj (⊢-unique Γ⊢t∶τ₁ Γ⊢t∶τ₂))

-- Now we can do one direction of the equivalence.  This direction is
-- just induction over derivations, making use of uniqueness of
-- typing.
⊬-¬⊢ : ∀ {n} {Γ : Ctx n} {t : Expr n} {τ : Type} → (Γ ⊬ t ∶ τ) → (¬ (Γ ⊢ t ∶ τ))

⊬-¬⊢ (mismatch Γ⊢t∶τ₁ τ₁≁τ) Γ⊢t∶τ = ≁-≢ τ₁≁τ (⊢-unique Γ⊢t∶τ₁ Γ⊢t∶τ)

⊬-¬⊢ (⊕ˡ Γ⊬t₁∶N)     (Γ⊢t₁∶N ⊕ _     ) = ⊬-¬⊢ Γ⊬t₁∶N Γ⊢t₁∶N
⊬-¬⊢ (⊕ʳ Γ⊬t₂∶N)     (_      ⊕ Γ⊢t₂∶N) = ⊬-¬⊢ Γ⊬t₂∶N Γ⊢t₂∶N
⊬-¬⊢ (⊕≁Nat τ≁N)     (_      ⊕ _     ) = ≁-≢ τ≁N refl

⊬-¬⊢ (ƛ-funty τ≁τ₁⇒) (ƛ _)      = ≁-≢ τ≁τ₁⇒ refl
⊬-¬⊢ (ƛ Γ⊬t∶τ₂)      (ƛ Γ⊢t∶τ₂) = ⊬-¬⊢ Γ⊬t∶τ₂ Γ⊢t∶τ₂

⊬-¬⊢ (·-fun Γ⊬t₁) (Γ⊢t₁ · _) = ⊬-¬⊢ Γ⊬t₁ Γ⊢t₁
⊬-¬⊢ (·-arg Γ⊢t₁∶τ₁⇒τ Γ⊬t₂∶τ) (Γ⊢t₁∶τ₂⇒τ · Γ⊢t₂)
  rewrite proj₁ (⇒-inj (⊢-unique Γ⊢t₁∶τ₁⇒τ Γ⊢t₁∶τ₂⇒τ)) = ⊬-¬⊢ Γ⊬t₂∶τ Γ⊢t₂

-- The other direction follows straightforwardly from type checking.
¬⊢-⊬ : ∀ {n} {Γ : Ctx n} {t : Expr n} {τ : Type} → (¬ (Γ ⊢ t ∶ τ)) → (Γ ⊬ t ∶ τ)
¬⊢-⊬ {_} {Γ} {t} {τ} ¬Γ⊢t∶τ with check Γ t τ
¬⊢-⊬ ¬Γ⊢t∶τ | inj₁ Γ⊢t∶τ = ⊥-elim (¬Γ⊢t∶τ Γ⊢t∶τ)
¬⊢-⊬ ¬Γ⊢t∶τ | inj₂ Γ⊬t∶τ = Γ⊬t∶τ

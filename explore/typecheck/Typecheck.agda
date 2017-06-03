module Typecheck where

open import Relation.Binary.PropositionalEquality

open import Data.Nat
open import Data.Fin
open import Data.Vec
open import Data.Sum
open import Data.Product

data Type : Set where
  Nat : Type
  _⇒_ : Type → Type → Type

infixr 80 _⇒_

⟦_⟧ : Type → Set
⟦ Nat ⟧ = ℕ
⟦ τ₁ ⇒ τ₂ ⟧ = ⟦ τ₁ ⟧ → ⟦ τ₂ ⟧

-- A type of explicit evidence explaining *why* two types are unequal.
data _≁_ : Type → Type → Set where
  Nat≁⇒ : ∀ {τ₁ τ₂} → Nat ≁ (τ₁ ⇒ τ₂)
  ⇒ˡ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₁ ≁ τ₂ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)
  ⇒ʳ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₃ ≁ τ₄ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)

  ≁-sym   : ∀ {τ₁ τ₂} → τ₁ ≁ τ₂ → τ₂ ≁ τ₁

-- Given such a proof we can show the types are unequal.  But we can't
-- go the other direction.  The whole point is that τ₁ ≢ τ₂ is
-- nonconstructive, but τ₁ ≁ τ₂ gives us something we can introspect
-- on in order to produce better explanations / error messages.
≁-≢ : ∀ {τ₁ τ₂} → (τ₁ ≁ τ₂) → (τ₁ ≢ τ₂)
≁-≢ Nat≁⇒ = λ ()
≁-≢ (⇒ˡ-≁ τ₁≁τ₂)  refl = ≁-≢ τ₁≁τ₂ refl
≁-≢ (⇒ʳ-≁ τ₃≁τ₄)  refl = ≁-≢ τ₃≁τ₄ refl
≁-≢ (≁-sym τ₂≁τ₁) refl = ≁-≢ τ₂≁τ₁ refl

-- Equality of types is decidable.
_∼?_ : (τ₁ τ₂ : Type) → (τ₁ ≡ τ₂) ⊎ (τ₁ ≁ τ₂)
Nat ∼? Nat = inj₁ refl
Nat ∼? (τ₂ ⇒ τ₃) = inj₂ Nat≁⇒
(τ₁ ⇒ τ₂) ∼? Nat = inj₂ (≁-sym Nat≁⇒)
(τ₁ ⇒ τ₃) ∼? (τ₂ ⇒ τ₄) with τ₁ ∼? τ₂ | τ₃ ∼? τ₄
(τ₁ ⇒ τ₃) ∼? (.τ₁ ⇒ .τ₃) | inj₁ refl | inj₁ refl  = inj₁ refl
(τ₁ ⇒ τ₃) ∼? (τ₂ ⇒ τ₄) | inj₁ _ | inj₂ τ₃≁τ₄ = inj₂ (⇒ʳ-≁ τ₃≁τ₄)
(τ₁ ⇒ τ₃) ∼? (τ₂ ⇒ τ₄) | inj₂ τ₁≁τ₂ | q = inj₂ (⇒ˡ-≁ τ₁≁τ₂)

data Expr (n : ℕ) : Set where
  lit : ℕ → Expr n
  _⊕_ : Expr n → Expr n → Expr n

  var : Fin n → Expr n
  ƛ   : Type → Expr (suc n) → Expr n
  _·_ : Expr n → Expr n → Expr n

Ctx : ℕ → Set
Ctx n = Vec Type n

-- Should we have two types, one for typability and one for
-- untypability, or should we combine them into a single type indexed
-- by Bool?  Let's try separate types first.

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

-- Explicit evidence for the *untypability* of a term.
data _⊬_∶_ : ∀ {n} → Ctx n → Expr n → Type → Set where
  mismatch : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
           → Γ ⊢ t ∶ τ₁
           → τ₁ ≁ τ₂
           → Γ ⊬ t ∶ τ₂
  ⊕ˡ     : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ}
           → Γ ⊬ t₁ ∶ Nat
           → Γ ⊬ (t₁ ⊕ t₂) ∶ τ
  ⊕ʳ     : ∀ {n} {Γ : Ctx n} {t₁ t₂} {τ}
           → Γ ⊬ t₂ ∶ Nat
           → Γ ⊬ (t₁ ⊕ t₂) ∶ τ

  ƛ-fun  : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ}
           → (∀ {τ₂ τ₃} → τ ≁ τ₂ ⇒ τ₃)
           → Γ ⊬ ƛ τ₁ t ∶ τ
  ƛ-cong : ∀ {n} {Γ : Ctx n} {t} {τ₁ τ₂}
         → (τ₁ ∷ Γ) ⊬ t ∶ τ₂
         → Γ ⊬ ƛ τ₁ t ∶ (τ₁ ⇒ τ₂)
  -- ·-fun  : ∀ {n} {Γ : Ctx n}
  --       →

-- Type inference for a term in a given context returns either a type
-- and a valid typing derivation, or a constructive proof that the
-- term has no type.
infer : ∀ {n} → (Γ : Ctx n) → (t : Expr n) → (∃ λ τ → Γ ⊢ t ∶ τ) ⊎ (∀ τ → Γ ⊬ t ∶ τ)
infer Γ (lit n)   = inj₁ (Nat , lit)
infer Γ (t₁ ⊕ t₂) with infer Γ t₁ | infer Γ t₂
infer Γ (t₁ ⊕ t₂) | inj₁ (Nat , Γ⊢t₁∶Nat) | inj₁ (Nat , Γ⊢t₂∶Nat) = inj₁ (Nat , (Γ⊢t₁∶Nat ⊕ Γ⊢t₂∶Nat))
infer Γ (t₁ ⊕ t₂) | inj₁ (τ₁ ⇒ τ₂ , Γ⊢t₁∶τ₁⇒τ₂) | inj₁ _ = inj₂ (λ _ → ⊕ˡ (mismatch Γ⊢t₁∶τ₁⇒τ₂ (≁-sym Nat≁⇒)))
infer Γ (t₁ ⊕ t₂) | inj₁ _ | inj₁ (τ₃ ⇒ τ₄ , Γ⊢t₂∶τ₃⇒τ₄) = inj₂ (λ _ → ⊕ʳ (mismatch Γ⊢t₂∶τ₃⇒τ₄ (≁-sym Nat≁⇒)))
infer Γ (t₁ ⊕ t₂) | inj₂ Γ⊬t₁∶ | _ = inj₂ (λ _ → ⊕ˡ (Γ⊬t₁∶ Nat))
infer Γ (t₁ ⊕ t₂) | _ | inj₂ Γ⊬t₂∶ = inj₂ (λ _ → ⊕ʳ (Γ⊬t₂∶ Nat))
infer Γ (var i)   = inj₁ (lookup i Γ , var)
infer Γ (ƛ x t)   = {!!}
infer Γ (t₁ · t₂) = {!!}

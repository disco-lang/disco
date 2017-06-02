module Typecheck where

open import Relation.Binary.PropositionalEquality

open import Data.Nat
open import Data.Fin
open import Data.Vec
open import Data.Bool
open import Data.Sum

data Type : Set where
  Nat : Type
  _⇒_ : Type → Type → Type

⟦_⟧ : Type → Set
⟦ Nat ⟧ = ℕ
⟦ τ₁ ⇒ τ₂ ⟧ = ⟦ τ₁ ⟧ → ⟦ τ₂ ⟧

data _≁_ : Type → Type → Set where
  Nat≁⇒ : ∀ {τ₁ τ₂} → Nat ≁ (τ₁ ⇒ τ₂)
  ⇒ˡ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₁ ≁ τ₂ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)
  ⇒ʳ-≁  : ∀ {τ₁ τ₂ τ₃ τ₄} → τ₃ ≁ τ₄ → (τ₁ ⇒ τ₃) ≁ (τ₂ ⇒ τ₄)

  ≁-sym   : ∀ {τ₁ τ₂} → τ₁ ≁ τ₂ → τ₂ ≁ τ₁

≁-≢ : ∀ {τ₁ τ₂} → (τ₁ ≁ τ₂) → (τ₁ ≢ τ₂)
≁-≢ Nat≁⇒ = λ ()
≁-≢ (⇒ˡ-≁ pf) = {!!}
≁-≢ (⇒ʳ-≁ pf) = {!!}
≁-≢ (≁-sym pf) = {!!}

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
  _∙_ : Expr n → Expr n → Expr n

Ctx : ℕ → Set
Ctx n = Vec Type n

data _⊢[_]_∶_ : ∀ {n} → Ctx n → Bool → Expr n → Type → Set where
  lit  : ∀ {n} {Γ : Ctx n} {m}
       → Γ ⊢[ true ] lit m ∶ Nat
  ¬lit : ∀ {n} {Γ : Ctx n} {m} {τ}
       → τ ≁ Nat
       → Γ ⊢[ false ] lit m ∶ τ
  add  : ∀ {n} {Γ : Ctx n} {b₁ b₂} {t₁ t₂}
       → Γ ⊢[ b₁ ]      t₁        ∶ Nat
       → Γ ⊢[ b₂ ]      t₂        ∶ Nat
       → Γ ⊢[ b₁ ∧ b₂ ] (t₁ ⊕ t₂) ∶ Nat

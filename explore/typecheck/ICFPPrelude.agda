module ICFPPrelude where

record ⊤ : Set where
  constructor ⟨⟩

data ⊥ : Set where

¬_ : Set → Set
¬_ A = A → ⊥

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero + n = n
suc m + n = suc (m + n)

infixr 2 _∪_
data _∪_ A B : Set where
  inl : A → A ∪ B
  inr : B → A ∪ B

data _≡_ {a}{A : Set a}(x : A) : A → Set a where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

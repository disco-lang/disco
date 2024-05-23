open import Data.Nat
open import Data.Nat.DivMod
open import Data.Nat.LCM
open import Data.Fin
open import Data.Sum
open import Data.Product

Sub : ℕ → Set₁
Sub n = Fin n → Set

ℤSet : Set₁
ℤSet = Σ[ m ∈ ℕ ] (NonZero m × Sub m)

_∪_ : ℤSet → ℤSet → ℤSet
(m₁ , (pf₁ , B₁)) ∪ (m₂ , (pf₂ , B₂)) = lcm m₁ m₂ , {!!} , λ x → B₁ (toℕ x mod m₁) ⊎ B₂ (toℕ x mod m₂)


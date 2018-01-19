{-

    Interactive Programming with Dependent Types

                      Ulf Norell

                  ICFP 2013, Boston

 -}

module ICFP where

open import ICFPPrelude

data Type : Set where
  nat : Type
  _⇒_ : (a b : Type) → Type

infixr 7 _⇒_

data Raw : Set where
  var : (n : Nat) → Raw
  app : (e₁ e₂ : Raw) → Raw
  lam : (a : Type) (e : Raw) → Raw

data Cxt : Set where
  [] : Cxt
  _,_ : (Γ : Cxt) (a : Type) → Cxt

data Var : Cxt → Type → Set where
  zero : ∀ {Γ a} → Var (Γ , a) a
  suc  : ∀ {Γ a b} → Var Γ a → Var (Γ , b) a

data Term : Cxt → Type → Set where
  var : ∀ {Γ a} → Var Γ a → Term Γ a
  app : ∀ {Γ a b} (u : Term Γ (a ⇒ b)) (v : Term Γ a) →
        Term Γ b
  lam : ∀ {Γ} a {b} (u : Term (Γ , a) b) → Term Γ (a ⇒ b)

eraseVar : ∀ {Γ a} → Var Γ a → Nat
eraseVar zero = zero
eraseVar (suc x) = suc (eraseVar x)

erase : ∀ {Γ a} → Term Γ a → Raw
erase (var x) = var (eraseVar x)
erase (app v v₁) = app (erase v) (erase v₁)
erase (lam a v) = lam a (erase v)

data _≠_ : Type → Type → Set where
  nat≠fun : ∀ {a b} → nat ≠ a ⇒ b
  fun≠nat : ∀ {a b} → a ⇒ b ≠ nat
  src≠    : ∀ {a a′ b c} → a ≠ a′ → a ⇒ b ≠ a′ ⇒ c
  tgt≠    : ∀ {a b c c′} → c ≠ c′ → a ⇒ c ≠ b ⇒ c′

sound : ∀ a → a ≠ a → ⊥
sound nat ()
sound (a ⇒ b) (src≠ x) = sound a x
sound (a ⇒ b) (tgt≠ x) = sound b x

infix 4 _≠_ _≟_

data _≟_ : Type → Type → Set where
  equal : ∀ {a} → a ≟ a
  not-equal : ∀ {a b} → a ≠ b → a ≟ b

compareFun : ∀ {a b c d} → a ≟ b → c ≟ d → a ⇒ c ≟ b ⇒ d
compareFun equal equal = equal
compareFun equal (not-equal x) = not-equal (tgt≠ x)
compareFun (not-equal x) q = not-equal (src≠ x)

compare : ∀ a b → a ≟ b
compare nat nat = equal
compare nat (a ⇒ b) = not-equal nat≠fun
compare (a ⇒ b) nat = not-equal fun≠nat
compare (a ⇒ b) (a₁ ⇒ b₁) = compareFun (compare a a₁) (compare b b₁)

data OutOfScope : Cxt → Nat → Set where
  oops : ∀ {n} → OutOfScope [] n
  suc  : ∀ {Γ a n} → OutOfScope Γ n → OutOfScope (Γ , a) (suc n)

data WellScoped? : Cxt → Nat → Set where
  yes : ∀ {Γ} a (x : Var Γ a) → WellScoped? Γ (eraseVar x)
  no  : ∀ {Γ n} → OutOfScope Γ n → WellScoped? Γ n

data TypeError : Cxt → Raw → Set where
  arg-mismatch : ∀ {a a′ Γ b} {u : Term Γ (a ⇒ b)} {u₁ : Term Γ a′} →
                 a ≠ a′ → TypeError Γ (app (erase u) (erase u₁))
  not-fun : ∀ {Γ} {u : Term Γ nat} {a₁} {u₁ : Term Γ a₁} →
            TypeError Γ (app (erase u) (erase u₁))
  bad-arg : ∀ {Γ e₂ e₁} → TypeError Γ e₂ → TypeError Γ (app e₁ e₂)
  bad-fun : ∀ {Γ e₁ e₂} → TypeError Γ e₁ → TypeError Γ (app e₁ e₂)
  out-of-scope : ∀ {Γ n} → OutOfScope Γ n → TypeError Γ (var n)
  lam : ∀ {Γ e} a → TypeError (Γ , a) e → TypeError Γ (lam a e)

data WellTyped? : Cxt → Raw → Set where
  yes : ∀ {Γ} a (u : Term Γ a) → WellTyped? Γ (erase u)
  no  : ∀ {Γ e} (err : TypeError Γ e) → WellTyped? Γ e

checkApp : ∀ {Γ e₁ e₂} → WellTyped? Γ e₁ → WellTyped? Γ e₂ →
             WellTyped? Γ (app e₁ e₂)
checkApp (yes nat u) (yes a₁ u₁) = no not-fun
checkApp (yes (a ⇒ b) u) (yes a′ u₁) with compare a a′
checkApp (yes (a ⇒ b) u) (yes .a u₁) | equal         = yes b (app u u₁)
checkApp (yes (a ⇒ b) u) (yes a′ u₁) | not-equal err = no (arg-mismatch err)
checkApp (yes a u) (no err) = no (bad-arg err)
checkApp (no err) (yes a u) = no (bad-fun err)
checkApp (no err) (no err₁) = no (bad-arg err₁)

lookupSuc : ∀ {Γ a n} →
            WellScoped? Γ n → WellScoped? (Γ , a) (suc n)
lookupSuc (yes a₁ x) = yes a₁ (suc x)
lookupSuc (no x) = no (suc x)

lookupVar : ∀ Γ n → WellScoped? Γ n
lookupVar [] n = no oops
lookupVar (Γ , a) zero = yes a zero
lookupVar (Γ , a) (suc n) = lookupSuc (lookupVar Γ n)

checkVar : ∀ {Γ n} → WellScoped? Γ n → WellTyped? Γ (var n)
checkVar (yes a x) = yes a (var x)
checkVar (no x) = no (out-of-scope x)

checkLam : ∀ {Γ e} a → WellTyped? (Γ , a) e → WellTyped? Γ (lam a e)
checkLam a (yes a₁ u) = yes (a ⇒ a₁) (lam a u)
checkLam a (no err) = no (lam a err)

check : ∀ Γ e → WellTyped? Γ e
check Γ (var n) = checkVar (lookupVar Γ n)
check Γ (app e₁ e₂) = checkApp (check Γ e₁) (check Γ e₂)
check Γ (lam a e) = checkLam a (check (Γ , a) e)

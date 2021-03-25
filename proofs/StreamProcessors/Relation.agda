module StreamProcessors.Relation where

open import Codata.Thunk as Thunk using (Thunk; force)
open import Level
open import Relation.Binary
open import Size

open import StreamProcessors.Core

module Generic {α ρ} (R : {T : Set α} → Rel T ρ) (R-isEquivalence : {T : Set α} → IsEquivalence (R {T})) where
  infix 1 _⊢_≈_

  data _⊢_≈_ (i : Size) {A B : Set α} : Rel (Pipe A B ∞) (α Level.⊔ ρ) where
    ≈stop :
        i ⊢ stop ≈ stop
    ≈yield : ∀ {value₁ value₂ : B} {next₁ next₂ : Thunk (Pipe A B) ∞}
      → (value : R value₁ value₂)
      → (next : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i next₁ next₂)
      → i ⊢ yield value₁ next₁ ≈ yield value₂ next₂
    ≈demand : ∀ {onNext₁ onNext₂ : A → Thunk (Pipe A B) ∞}
      → (onNext : ∀ value → Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i (onNext₁ value) (onNext₂ value))
      → i ⊢ demand onNext₁ ≈ demand onNext₂
    ≈lazyˡ : ∀ {a : Thunk (Pipe A B) ∞} {b : Pipe A B ∞}
      → (next : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i a (λ where .force → b))
      → i ⊢ lazy a ≈ b
    ≈lazyʳ : ∀ {a : Pipe A B ∞} {b : Thunk (Pipe A B) ∞}
      → (next : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i (λ where .force → a) b)
      → i ⊢ a ≈ lazy b
    ≈thunk : ∀ {a b : Pipe A B ∞}
      → (relation : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i (λ where .force → a) (λ where .force → b))
      → i ⊢ a ≈ b

  ≈lazyᵇ : ∀ {i : Size} {A B : Set α} {a b : Thunk (Pipe A B) ∞}
    → (next : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i a b)
    → i ⊢ lazy a ≈ lazy b
  ≈lazyᵇ next = ≈lazyˡ λ where .force → ≈lazyʳ λ where .force → next .force

  refl : ∀ {i : Size} {A B : Set α} → Reflexive (_⊢_≈_ i {A} {B})
  refl {x = stop} = ≈stop
  refl {x = yield _ _} = ≈yield (R-isEquivalence .IsEquivalence.refl) λ where .force → refl
  refl {x = demand _} = ≈demand λ _ → λ where .force → refl
  refl {x = lazy _} = ≈lazyᵇ λ where .force → refl

  sym : ∀ {i : Size} {A B : Set α} → Symmetric (_⊢_≈_ i {A} {B})
  sym ≈stop = ≈stop
  sym (≈yield value next) = ≈yield (R-isEquivalence .IsEquivalence.sym value) λ where .force → sym (next .force)
  sym (≈demand onNext) = ≈demand λ value → λ where .force → sym (onNext value .force)
  sym (≈lazyˡ next) = ≈lazyʳ λ where .force → sym (next .force)
  sym (≈lazyʳ next) = ≈lazyˡ λ where .force → sym (next .force)
  sym (≈thunk relation) = ≈thunk λ where .force → sym (relation .force)

  trans : ∀ {i : Size} {A B : Set α} → Transitive (_⊢_≈_ i {A} {B})
  trans ≈stop bc = bc
  trans (≈lazyˡ next) bc = ≈lazyˡ λ where .force → trans (next .force) bc
  trans (≈thunk relation) bc = ≈thunk λ where .force → trans (relation .force) bc
  trans ab (≈lazyʳ next) = ≈lazyʳ λ where .force → trans ab (next .force)
  trans ab (≈thunk relation) = ≈thunk λ where .force → trans ab (relation .force)
  trans (≈yield value₁ next₁) (≈yield value₂ next₂) = ≈yield (R-isEquivalence .IsEquivalence.trans value₁ value₂) λ where .force → trans (next₁ .force) (next₂ .force)
  trans (≈demand onNext₁) (≈demand onNext₂) = ≈demand λ value → λ where .force → trans (onNext₁ value .force) (onNext₂ value .force)
  trans (≈lazyʳ next₁) (≈lazyˡ next₂) = ≈thunk λ where .force → trans (next₁ .force) (next₂ .force)

  isEquivalence : ∀ {i : Size} {A B : Set α} → IsEquivalence (_⊢_≈_ i {A} {B})
  isEquivalence =
    record
      { refl = refl
      ; sym = sym
      ; trans = trans
      }

  setoid : ∀ {i : Size} {A B : Set α} → Setoid α (α Level.⊔ ρ)
  setoid {i} {A} {B} = record
    { isEquivalence = isEquivalence {i} {A} {B}
    }

  module ≈-Reasoning {i : Size} {A B : Set α} where
    open import Relation.Binary.Reasoning.Setoid (setoid {i} {A} {B}) public

module PropositionalEquality {α} where
  import Relation.Binary.PropositionalEquality as Eq
  open Generic {α} {α} Eq._≡_ Eq.isEquivalence public

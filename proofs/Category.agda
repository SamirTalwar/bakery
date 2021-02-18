module Category where

open import Level
open import Relation.Binary.PropositionalEquality

record Category {α β} (Object : Set α) (_⟶_ : Object → Object → Set β) : Set (α ⊔ β) where
  field
    _∘_ : {A B C : Object} → B ⟶ C → A ⟶ B → A ⟶ C
    id : {A : Object} → A ⟶ A

    law-idˡ : {A B : Object} → (f : A ⟶ B) → (id ∘ f) ≡ f
    law-idʳ : {A B : Object} → (f : A ⟶ B) → (f ∘ id) ≡ f
    law-assoc : {A B C D : Object} → (h : C ⟶ D) → (g : B ⟶ C) → (f : A ⟶ B) → h ∘ (g ∘ f) ≡ (h ∘ g) ∘ f

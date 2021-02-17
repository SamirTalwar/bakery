module Category where

open import Relation.Binary.PropositionalEquality

record Category (Object : Set₁) (_⟶_ : Object → Object → Set₁) : Set₁ where
  field
    _∘_ : {A B C : Object} → B ⟶ C → A ⟶ B → A ⟶ C
    id : {A : Object} → A ⟶ A
    assoc : {A B C D : Object} → (h : C ⟶ D) → (g : B ⟶ C) → (f : A ⟶ B) → h ∘ (g ∘ f) ≡ (h ∘ g) ∘ f

module StreamProcessors.Categorical where

open import Algebra.Definitions
open import Category.Applicative
open import Category.Functor
open import Category.Monad
open import Codata.Thunk as Thunk using (Thunk; force)
open import Function using (_∘_; _∘′_)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_)
open import Size using (∞)

open import Category

open import StreamProcessors.Composition
open import StreamProcessors.Core
open import StreamProcessors.Relation
open StreamProcessors.Relation.PropositionalEquality

|>-identityˡ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
  → i ⊢ id |> pipe ≈ pipe
|>-identityˡ stop = ≈stop
|>-identityˡ (yield value next) = ≈yield Eq.refl λ where .force → |>-identityˡ (next .force)
|>-identityˡ (demand onNext) = ≈demand λ value → λ where .force → ≈lazyˡ λ where .force → |>-identityˡ (onNext value .force)
|>-identityˡ (lazy next) = ≈lazyᵇ λ where .force → |>-identityˡ (next .force)

|>-identityʳ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
  → i ⊢ pipe |> id ≈ pipe
|>-identityʳ stop = ≈stop
|>-identityʳ (yield value next) = ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → |>-identityʳ (next .force)
|>-identityʳ (demand onNext) = ≈demand λ value → λ where .force → |>-identityʳ (onNext value .force)
|>-identityʳ (lazy next) = ≈lazyᵇ λ where .force → |>-identityʳ (next .force)

<|-identityˡ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
  → i ⊢ id <| pipe ≈ pipe
<|-identityˡ pipe = |>-identityʳ pipe

<|-identityʳ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
  → i ⊢ pipe <| id ≈ pipe
<|-identityʳ pipe = |>-identityˡ pipe

|>-assoc : ∀ {i} {α} {A B C D : Set α} (f : Pipe A B ∞) (g : Pipe B C ∞) (h : Pipe C D ∞)
  → i ⊢ (f |> g) |> h ≈ f |> (g |> h)
|>-assoc _ _ stop = ≈stop
|>-assoc _ stop (demand _) = ≈stop
|>-assoc stop (demand _) (demand _) = ≈stop
|>-assoc f g (yield value next) = ≈yield Eq.refl λ where .force → |>-assoc f g (next .force)
|>-assoc f (yield value next) (demand h) = ≈lazyᵇ λ where .force → |>-assoc f (next .force) (h value .force)
|>-assoc (yield value next) (demand g) h@(demand _) = ≈lazyᵇ λ where .force → |>-assoc (next .force) (g value .force) h
|>-assoc (demand onNext) g@(demand _) h@(demand _) = ≈demand λ value → λ where .force → |>-assoc (onNext value .force) g h
|>-assoc (lazy next) g@(demand _) h@(demand _) = ≈lazyᵇ λ where .force → |>-assoc (next .force) g h
|>-assoc f (lazy next) h@(demand _) = ≈lazyᵇ λ where .force → |>-assoc f (next .force) h
|>-assoc f g (lazy next) = ≈lazyᵇ λ where .force → |>-assoc f g (next .force)

<|-assoc : ∀ {i} {α} {A B C D : Set α} (h : Pipe C D ∞) (g : Pipe B C ∞) (f : Pipe A B ∞)
  → i ⊢ h <| (g <| f) ≈ (h <| g) <| f
<|-assoc h g f = |>-assoc f g h

pipes-form-a-category : ∀ {i} {α} → Category (Set α) (λ A B → Pipe A B ∞) (i ⊢_≈_)
pipes-form-a-category =
  record
    { _∘_ = _<|_
    ; id = id
    ; law-idˡ = <|-identityˡ
    ; law-idʳ = <|-identityʳ
    ; law-assoc = <|-assoc
    }

module StreamProcessors where

open import Codata.Thunk as Thunk using (Thunk; force)
open import Level using (Level)
open import Size

module Core where
  data Pipe {α : Level} (A B : Set α) (i : Size) : Set α where
    stop : Pipe A B i
    yield : (value : B) → (next : Thunk (Pipe A B) i) → Pipe A B i
    demand : (onNext : (value : A) → Thunk (Pipe A B) i) → Pipe A B i
    lazy : (next : Thunk (Pipe A B) i) → Pipe A B i

  stop♯ : ∀ {i} {α} {A B : Set α} → Thunk (Pipe A B) i
  stop♯ .force = stop

  id : ∀ {i} {α} {A : Set α} → Pipe A A i
  id′ : ∀ {i} {α} {A : Set α} → A → Pipe A A i
  id = demand λ value → λ where .force → id′ value
  id′ value = yield value λ where .force → id

module Composition where
  open Core

  infixl 9 _|>_ _♯|>_ _|>♯_ _♯|>♯_
  infixr 9 _<|_

  _|>_ : ∀  {i} {α} {A B C : Set α} → Pipe A B i → Pipe B C i → Pipe A C i
  _♯|>_ : ∀  {i} {α} {A B C : Set α} → Thunk (Pipe A B) i → Pipe B C i → Thunk (Pipe A C) i
  _|>♯_ : ∀  {i} {α} {A B C : Set α} → Pipe A B i → Thunk (Pipe B C) i → Thunk (Pipe A C) i
  _♯|>♯_ : ∀  {i} {α} {A B C : Set α} → Thunk (Pipe A B) i → Thunk (Pipe B C) i → Thunk (Pipe A C) i
  _ |> stop = stop
  up |> yield value next = yield value (up |>♯ next)
  stop |> demand onNext = stop
  yield value next |> demand onNext = lazy (next ♯|>♯ onNext value)
  demand onNext |> down@(demand _) = demand (λ value → onNext value ♯|> down)
  lazy up |> down@(demand _) = lazy (up ♯|> down)
  up |> lazy down = lazy (up |>♯ down)
  a ♯|> b = λ where .force → a .force |> b
  a |>♯ b = λ where .force → a |> b .force
  a ♯|>♯ b = λ where .force → a .force |> b .force

  _<|_ : ∀ {i} {α} {A B C : Set α} → Pipe B C i → Pipe A B i → Pipe A C i
  down <| up = up |> down

module Relation where
  open import Level
  open import Relation.Binary

  open Core

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

module Algebra where
  open import Algebra.Definitions
  open import Algebra.Structures
  import Relation.Binary.PropositionalEquality as Eq
  open import Data.Product as Product using (_,_)

  open Core
  open Relation.PropositionalEquality

  infixr 5 _++_ _♯++_

  _++_ : ∀ {i : Size} {α} {A B : Set α} → Pipe A B i → Pipe A B i → Pipe A B i
  _♯++_ : ∀ {i : Size} {α} {A B : Set α} → Thunk (Pipe A B) i → Pipe A B i → Thunk (Pipe A B) i
  stop ++ b = b
  yield value next ++ b = yield value (next ♯++ b)
  demand onNext ++ b = demand λ value → onNext value ♯++ b
  lazy next ++ b = lazy (next ♯++ b)
  a ♯++ b = λ where .force → a .force ++ b

  concat : ∀ {i : Size} {α} {A B : Set α} → Pipe A (Pipe A B i) i → Pipe A B i
  concat stop = stop
  concat (yield pipe next) = pipe ++ lazy λ where .force → concat (next .force)
  concat (demand onNext) = demand λ value → λ where .force → concat (onNext value .force)
  concat (lazy next) = lazy λ where .force → concat (next .force)

  ++-cong : ∀ {i} {α} {A B : Set α} → Congruent₂ (_⊢_≈_ i {A} {B}) _++_
  ++-cong ≈stop b = b
  ++-cong (≈yield value next) b = ≈yield value λ where .force → ++-cong (next .force) b
  ++-cong (≈demand onNext) b = ≈demand λ value → λ where .force → ++-cong (onNext value .force) b
  ++-cong (≈lazyˡ next) b = ≈lazyˡ λ where .force → ++-cong (next .force) b
  ++-cong (≈lazyʳ next) b = ≈lazyʳ λ where .force → ++-cong (next .force) b
  ++-cong (≈thunk relation) b = ≈thunk λ where .force → ++-cong (relation .force) b

  ++-assoc : ∀ {i} {α} {A B : Set α} → Associative (_⊢_≈_ i {A} {B}) _++_
  ++-assoc stop b c = refl
  ++-assoc (yield value next) b c = ≈yield Eq.refl λ where .force → ++-assoc (next .force) b c
  ++-assoc (demand onNext) b c = ≈demand λ value → λ where .force → ++-assoc (onNext value .force) b c
  ++-assoc (lazy next) b c = ≈lazyᵇ λ where .force → ++-assoc (next .force) b c

  ++-identityˡ : ∀ {i} {α} {A B : Set α} {s : Pipe A B ∞} → i ⊢ s ≈ stop → LeftIdentity (_⊢_≈_ i {A} {B}) s _++_
  ++-identityˡ {s = s} s≈stop _ = ++-cong s≈stop refl

  ++-identityʳ : ∀ {i} {α} {A B : Set α} {s : Pipe A B ∞} → i ⊢ s ≈ stop → RightIdentity (_⊢_≈_ i {A} {B}) s _++_
  ++-identityʳ s≈stop stop = s≈stop
  ++-identityʳ s≈stop (yield value next) = ≈yield Eq.refl λ where .force → ++-identityʳ s≈stop (next .force)
  ++-identityʳ s≈stop (demand onNext) = ≈demand λ value → λ where .force → ++-identityʳ s≈stop (onNext value .force)
  ++-identityʳ s≈stop (lazy next) = ≈lazyᵇ λ where .force → ++-identityʳ s≈stop (next .force)

  ++-identity : ∀ {i} {α} {A B : Set α} → Identity (_⊢_≈_ i {A} {B}) stop _++_
  ++-identity = ++-identityˡ refl , ++-identityʳ refl

  isMagma : ∀ {α} {i : Size} {A B : Set α} → IsMagma (_⊢_≈_ i {A} {B}) _++_
  isMagma =
    record
      { isEquivalence = isEquivalence
      ; ∙-cong = ++-cong
      }

  isSemigroup : ∀ {α} {i : Size} {A B : Set α} → IsSemigroup (_⊢_≈_ i {A} {B}) _++_
  isSemigroup =
    record
      { isMagma = isMagma
      ; assoc = ++-assoc
      }

  isMonoid : ∀ {α} {i : Size} {A B : Set α} → IsMonoid (_⊢_≈_ i {A} {B}) _++_ stop
  isMonoid =
    record
      { isSemigroup = isSemigroup
      ; identity = ++-identity
      }

module Categorical where
  open import Algebra.Definitions
  open import Category.Applicative
  open import Category.Functor
  open import Category.Monad
  open import Function using (_∘_; _∘′_)
  open import Relation.Binary.PropositionalEquality as Eq using (_≡_)

  open import Category

  open Algebra
  open Composition
  open Core
  open Relation.PropositionalEquality
  open Relation.PropositionalEquality.≈-Reasoning

  infixl 4 _<$>_ _⊛_ _>>=_

  pure : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
  pure value = yield value stop♯

  _<$>_ : ∀ {i} {α} {A B C : Set α} → (B → C) → Pipe A B i → Pipe A C i
  _<$>♯_ : ∀ {i} {α} {A B C : Set α} → (B → C) → Thunk (Pipe A B) i → Thunk (Pipe A C) i
  f <$> stop = stop
  f <$> yield value next = yield (f value) (f <$>♯ next)
  f <$> demand onNext = demand λ value → f <$>♯ (onNext value)
  f <$> lazy next = lazy (f <$>♯ next)
  f <$>♯ x = λ where .force → f <$> x .force

  _⊛_ : ∀ {i} {α} {A B C : Set α} → Pipe A (B → C) i → Pipe A B i → Pipe A C i
  _♯⊛_ : ∀ {i} {α} {A B C : Set α} → Thunk (Pipe A (B → C)) i → Pipe A B i → Thunk (Pipe A C) i
  stop ⊛ x = stop
  yield value next ⊛ x = (value <$> x) ++ lazy (next ♯⊛ x)
  demand onNext ⊛ x = demand λ value → onNext value ♯⊛ x
  lazy next ⊛ x = lazy (next ♯⊛ x)
  f ♯⊛ x = λ where .force → f .force ⊛ x

  _>>=_ : ∀ {i} {α} {A B C : Set α} → Pipe A B i → (B → Pipe A C i) → Pipe A C i
  _♯>>=_ : ∀ {i} {α} {A B C : Set α} → Thunk (Pipe A B) i → (B → Pipe A C i) → Thunk (Pipe A C) i
  stop >>= f = stop
  yield value next >>= f = f value ++ lazy (next ♯>>= f)
  demand onNext >>= f = demand λ value → onNext value ♯>>= f
  lazy next >>= f = lazy (next ♯>>= f)
  pipe ♯>>= f = λ where .force → pipe .force >>= f

  functor : ∀ {i} {α} {A : Set α} → RawFunctor {α} (λ B → Pipe A B i)
  functor =
    record
      { _<$>_ = _<$>_
      }

  applicative : ∀ {i} {α} {A : Set α} → RawApplicative {α} (λ B → Pipe A B i)
  applicative =
    record
      { pure = pure
      ; _⊛_ = _⊛_
      }

  monad : ∀ {i} {α} {A : Set α} → RawMonad {α} (λ B → Pipe A B i)
  monad =
    record
      { return = pure
      ; _>>=_ = _>>=_
      }

  instance pipeFunctor = functor
  instance pipeApplicative = applicative
  instance pipeMonad = monad

  infixl 4 _≈<$>_
  infixl 4 _≈⊛_

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

  _≈<$>_ : ∀ {i} {α : Level} {A B C : Set α} {f₁ f₂ : B → C} {x₁ x₂ : Pipe A B ∞}
    → f₁ ≡ f₂
    → i ⊢ x₁ ≈ x₂
    → i ⊢ f₁ <$> x₁ ≈ f₂ <$> x₂
  f ≈<$> ≈stop = ≈stop
  f ≈<$> ≈yield value next = ≈yield (Eq.cong₂ (λ f x → f x) f value) λ where .force → f ≈<$> next .force
  f ≈<$> ≈demand onNext = ≈demand λ value → λ where .force → f ≈<$> onNext value .force
  f ≈<$> ≈lazyˡ next = ≈lazyˡ λ where .force → f ≈<$> next .force
  f ≈<$> ≈lazyʳ next = ≈lazyʳ λ where .force → f ≈<$> next .force
  f ≈<$> ≈thunk relation = ≈thunk λ where .force → f ≈<$> relation .force

  functor-identity : ∀ {i} {α} {A B : Set α} (pipe : Pipe A B ∞)
    → i ⊢ Function.id <$> pipe ≈ pipe
  functor-identity stop = ≈stop
  functor-identity (yield value next) = ≈yield Eq.refl λ where .force → functor-identity (next .force)
  functor-identity (demand onNext) = ≈demand λ value → λ where .force → functor-identity (onNext value .force)
  functor-identity (lazy next) = ≈lazyᵇ λ where .force → functor-identity (next .force)

  functor-compose : ∀ {i} {α} {A B C D : Set α} (pipe : Pipe A B ∞) (f : B → C) (g : C → D)
    → i ⊢ (g ∘ f) <$> pipe ≈ ((g <$>_) ∘ (f <$>_)) pipe
  functor-compose stop f g = ≈stop
  functor-compose (yield value next) f g = ≈yield Eq.refl λ where .force → functor-compose (next .force) f g
  functor-compose (demand onNext) f g = ≈demand λ value → λ where .force → functor-compose (onNext value .force) f g
  functor-compose (lazy next) f g = ≈lazyᵇ λ where .force → functor-compose (next .force) f g

  functor-concatenation : ∀ {i} {α} {A B C : Set α} (f : B → C) (x y : Pipe A B ∞)
    → i ⊢ (f <$> x) ++ (f <$> y) ≈ f <$> (x ++ y)
  functor-concatenation f stop y = refl
  functor-concatenation f (yield value next) y =
    begin
      (f <$> yield value next) ++ (f <$> y)
    ≈⟨ ≈yield Eq.refl (λ where .force → refl) ⟩
      (yield (f value) (λ where .force → f <$> next .force)) ++ (f <$> y)
    ≈⟨ ≈yield Eq.refl (λ where .force → refl) ⟩
      yield (f value) (λ where .force → (f <$> next .force) ++ (f <$> y))
    ≈⟨ ≈yield Eq.refl (λ where .force → functor-concatenation f (next .force) y) ⟩
      yield (f value) (λ where .force → f <$> (next .force ++ y))
    ≈⟨ ≈yield Eq.refl (λ where .force → refl) ⟩
      f <$> (yield value next ++ y)
    ∎
  functor-concatenation f (demand onNext) y = ≈demand λ value → λ where .force → functor-concatenation f (onNext value .force) y
  functor-concatenation f (lazy next) y = ≈lazyᵇ λ where .force → functor-concatenation f (next .force) y

  _≈⊛_ : ∀ {i} {α : Level} {A B C : Set α} {f₁ f₂ : Pipe A (B → C) ∞} {x₁ x₂ : Pipe A B ∞}
    → i ⊢ f₁ ≈ f₂
    → i ⊢ x₁ ≈ x₂
    → i ⊢ f₁ ⊛ x₁ ≈ f₂ ⊛ x₂
  ≈stop ≈⊛ x = ≈stop
  ≈yield value next ≈⊛ x = ++-cong (value ≈<$> x) (≈lazyᵇ λ where .force → next .force ≈⊛ x)
  ≈demand onNext ≈⊛ x = ≈demand λ value → λ where .force → onNext value .force ≈⊛ x
  ≈lazyˡ next ≈⊛ x = ≈lazyˡ λ where .force → next .force ≈⊛ x
  ≈lazyʳ next ≈⊛ x = ≈lazyʳ λ where .force → next .force ≈⊛ x
  ≈thunk relation ≈⊛ x = ≈thunk λ where .force → relation .force ≈⊛ x

  applicative-identity : ∀ {i} {α} {A B : Set α} (x : Pipe A B ∞)
    → i ⊢ pure Function.id ⊛ x ≈ x
  applicative-identity stop = ≈lazyˡ λ where .force → ≈stop
  applicative-identity (yield value next) =
    begin
      pure Function.id ⊛ yield value next
    ≈⟨ ≈yield Eq.refl (λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      (Function.id <$> yield value next) ++ lazy (stop♯ ♯⊛ yield value next)
    ≈⟨ ++-identityʳ (≈lazyˡ λ where .force → refl) (Function.id <$> yield value next) ⟩
      Function.id <$> yield value next
    ≈⟨ functor-identity (yield value next) ⟩
      yield value next
    ∎
  applicative-identity (demand onNext) =
    begin
      pure Function.id ⊛ demand onNext
    ≈⟨ ≈demand (λ x → λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      (Function.id <$> demand onNext) ++ lazy (stop♯ ♯⊛ demand onNext)
    ≈⟨ ++-identityʳ (≈lazyˡ λ where .force → refl) (Function.id <$> demand onNext) ⟩
      Function.id <$> demand onNext
    ≈⟨ functor-identity (demand onNext) ⟩
      demand onNext
    ∎
  applicative-identity (lazy next) =
    begin
      pure Function.id ⊛ lazy next
    ≈⟨ ≈lazyᵇ (λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      (Function.id <$> lazy next) ++ lazy (stop♯ ♯⊛ lazy next)
    ≈⟨ ++-identityʳ (≈lazyˡ λ where .force → refl) (Function.id <$> lazy next) ⟩
      Function.id <$> lazy next
    ≈⟨ functor-identity (lazy next) ⟩
      lazy next
    ∎

  applicative-homomorphism : ∀ {i} {α} {A B C : Set α} (f : B → C) (x : B)
    → i ⊢ pure {A = A} f ⊛ pure {A = A} x ≈ pure {A = A} (f x)
  applicative-homomorphism f x = ≈yield Eq.refl λ where .force → ≈lazyˡ λ where .force → refl

  applicative-map : ∀ {i} {α} {A B C : Set α} (f : B → C) (x : Pipe A B ∞)
    → i ⊢ pure f ⊛ x ≈ f <$> x
  applicative-map f x = ++-identityʳ (≈lazyˡ λ where .force → refl) (f <$> x)

  applicative-interchange : ∀ {i} {α} {A B : Set α} (pipe : Pipe A (A → B) ∞) (x : A)
    → i ⊢ pipe ⊛ pure x ≈ pure (λ g → g x) ⊛ pipe
  applicative-interchange stop x = ≈lazyʳ λ where .force → refl
  applicative-interchange (yield value next) x =
    begin
      yield value next ⊛ pure x
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      (value <$> pure x) ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      yield (value x) stop♯ ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → ≈lazyʳ λ where .force → refl) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → lazy next ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → applicative-interchange (lazy next) x) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → pure (λ g → g x) ⊛ lazy next)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → ≈lazyˡ λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → pure (λ g → g x) ⊛ next .force)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyˡ λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      ((λ g → g x) <$> yield value next) ++ lazy (stop♯ ♯⊛ next .force)
    ≈⟨ ≈yield Eq.refl (λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      pure (λ g → g x) ⊛ yield value next
    ∎
  applicative-interchange (demand onNext) x =
    begin
      demand onNext ⊛ pure x
    ≈⟨ ≈demand (λ value → λ where .force → refl) ⟩
      demand (λ value → λ where .force → onNext value .force ⊛ pure x)
    ≈⟨ ≈demand (λ value → λ where .force → ≈lazyʳ λ where .force → refl) ⟩
      demand (λ value → λ where .force → lazy (onNext value) ⊛ pure x)
    ≈⟨ ≈demand (λ value → λ where .force → applicative-interchange (lazy (onNext value)) x) ⟩
      demand (λ value → λ where .force → pure (λ g → g x) ⊛ lazy (onNext value))
    ≈⟨ ≈demand (λ value → λ where .force → ≈lazyˡ λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      ((λ g → g x) <$> demand onNext) ++ lazy (stop♯ ♯⊛ demand onNext)
    ≈⟨ ≈demand (λ value → λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      pure (λ g → g x) ⊛ demand onNext
    ∎
  applicative-interchange (lazy next) x =
    begin
      lazy next ⊛ pure x
    ≈⟨ ≈lazyᵇ (λ where .force → applicative-interchange (next .force) x) ⟩
      ((λ g → g x) <$> lazy next) ++ _
    ≈⟨ ≈lazyᵇ (λ where .force → ++-cong refl (sym (≈lazyᵇ λ where .force → refl))) ⟩
      pure (λ g → g x) ⊛ lazy next
    ∎

  applicative-concatenation : ∀ {i} {α} {A B C : Set α} (f g : Pipe A (B → C) ∞) (x : Pipe A B ∞)
    → i ⊢ (f ⊛ x) ++ (g ⊛ x) ≈ (f ++ g) ⊛ x
  applicative-concatenation stop g x = refl
  applicative-concatenation (yield value next) g x =
    begin
      (yield value next ⊛ x) ++ (g ⊛ x)
    ≈⟨ refl ⟩
      ((value <$> x) ++ lazy (next ♯⊛ x)) ++ (g ⊛ x)
    ≈⟨ ++-assoc (value <$> x) (lazy (next ♯⊛ x)) (g ⊛ x) ⟩
      (value <$> x) ++ (lazy (next ♯⊛ x) ++ (g ⊛ x))
    ≈⟨ refl ⟩
      (value <$> x) ++ lazy ((next ♯⊛ x) ♯++ (g ⊛ x))
    ≈⟨ ++-cong refl (≈lazyᵇ λ where .force → applicative-concatenation (next .force) g x) ⟩
      (value <$> x) ++ lazy ((next ♯++ g) ♯⊛ x)
    ≈⟨ refl ⟩
      (yield value (next ♯++ g)) ⊛ x
    ≈⟨ refl ⟩
      (yield value next ++ g) ⊛ x
    ∎
  applicative-concatenation (demand onNext) g x = ≈demand λ value → λ where .force → applicative-concatenation (onNext value .force) g x
  applicative-concatenation (lazy next) g x = ≈lazyᵇ λ where .force → applicative-concatenation (next .force) g x

  applicative-composition : ∀ {i} {α} {A B C D : Set α} (g : Pipe A (C → D) ∞) (f : Pipe A (B → C) ∞) (x : Pipe A B ∞)
    → i ⊢ (((pure _∘′_ ⊛ g) ⊛ f) ⊛ x) ≈ g ⊛ (f ⊛ x)
  applicative-composition stop f x =
    ≈lazyˡ λ where .force → ≈stop
  applicative-composition (yield value next) f x =
    begin
      ((pure _∘′_ ⊛ yield value next) ⊛ f) ⊛ x
    ≈⟨ ((refl {x = pure _∘′_} ≈⊛ ≈yield Eq.refl λ where .force → refl) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      ((pure _∘′_ ⊛ (pure value ++ next .force)) ⊛ f) ⊛ x
    ≈⟨ (applicative-map _∘′_ (pure value ++ next .force) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      ((_∘′_ <$> (pure value ++ next .force)) ⊛ f) ⊛ x
    ≈⟨ (sym (functor-concatenation _∘′_ (pure value) (next .force)) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      (((_∘′_ <$> pure value) ++ (_∘′_ <$> next .force)) ⊛ f) ⊛ x
    ≈⟨ (++-cong (sym (applicative-map _∘′_ (pure value))) (sym (applicative-map _∘′_ (next .force))) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      (((pure _∘′_ ⊛ pure value) ++ (pure _∘′_ ⊛ next .force)) ⊛ f) ⊛ x
    ≈⟨ sym (applicative-concatenation (pure _∘′_ ⊛ pure value) (pure _∘′_ ⊛ next .force) f) ≈⊛ refl ⟩
      (((pure _∘′_ ⊛ pure value) ⊛ f) ++ ((pure _∘′_ ⊛ next .force) ⊛ f)) ⊛ x
    ≈⟨ sym (applicative-concatenation ((pure _∘′_ ⊛ pure value) ⊛ f) ((pure _∘′_ ⊛ next .force) ⊛ f) x) ⟩
      (((pure _∘′_ ⊛ pure value) ⊛ f) ⊛ x) ++ (((pure _∘′_ ⊛ next .force) ⊛ f) ⊛ x)
    ≈⟨ ++-cong ((refl {x = pure _∘′_ ⊛ pure value} ≈⊛ refl {x = f}) ≈⊛ refl {x = x}) (≈lazyʳ λ where .force → refl) ⟩
      (((pure _∘′_ ⊛ pure value) ⊛ f) ⊛ x) ++ lazy (λ where .force → ((pure _∘′_ ⊛ next .force) ⊛ f) ⊛ x)
    ≈⟨ ++-cong (≈thunk λ where .force → applicative-composition (pure value) f x) (≈lazyᵇ λ where .force → applicative-composition (next .force) f x) ⟩
      (pure value ⊛ (f ⊛ x)) ++ lazy (λ where .force → next .force ⊛ (f ⊛ x))
    ≈⟨ ++-cong (applicative-map value (f ⊛ x)) (≈lazyᵇ λ where .force → refl) ⟩
      (value <$> (f ⊛ x)) ++ lazy (next ♯⊛ (f ⊛ x))
    ≈⟨ refl ⟩
      yield value next ⊛ (f ⊛ x)
    ∎
  applicative-composition (demand onNext) f x =
    begin
      ((pure _∘′_ ⊛ demand onNext) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      (((_∘′_ <$> demand onNext) ++ lazy (stop♯ ♯⊛ demand onNext)) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((demand (λ value → _∘′_ <$>♯ onNext value) ++ lazy (stop♯ ♯⊛ demand onNext)) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((demand (λ value → (_∘′_ <$>♯ onNext value) ♯++ lazy (stop♯ ♯⊛ demand onNext))) ⊛ f) ⊛ x
    ≈⟨ ≈demand (λ value → λ where .force → ++-cong refl (≈lazyᵇ λ where .force → ≈stop)) ≈⊛ refl {x = f} ≈⊛ refl {x = x} ⟩
      ((demand (λ value → (_∘′_ <$>♯ onNext value) ♯++ lazy (stop♯ ♯⊛ onNext value .force))) ⊛ f) ⊛ x
    ≈⟨ ≈demand (λ value → λ where .force → applicative-composition (onNext value .force) f x) ⟩
      demand (λ value → onNext value ♯⊛ (f ⊛ x))
    ≈⟨ refl ⟩
      demand onNext ⊛ (f ⊛ x)
    ∎
  applicative-composition (lazy next) f x =
    begin
      ((pure _∘′_ ⊛ lazy next) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      (((_∘′_ <$> lazy next) ++ lazy (stop♯ ♯⊛ lazy next)) ⊛ f) ⊛ x
    ≈⟨ ++-cong (refl {x = _∘′_ <$> lazy next}) (≈lazyᵇ λ where .force → ≈stop) ≈⊛ refl {x = f} ≈⊛ refl {x = x} ⟩
      (((_∘′_ <$> lazy next) ++ _) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      (((_∘′_ <$> lazy next) ++ _) ⊛ f) ⊛ x
    ≈⟨ ≈lazyᵇ (λ where .force → applicative-composition (next .force) f x) ⟩
      lazy next ⊛ (f ⊛ x)
    ∎

  monad-identityˡ : ∀ {i} {α} {A B C : Set α} (x : B) (f : B → Pipe A C ∞)
    → i ⊢ pure x >>= f ≈ f x
  monad-identityˡ x f = ++-identityʳ (≈lazyˡ λ where .force → ≈stop) (f x)

  monad-identityʳ : ∀ {i} {α} {A B : Set α} (x : Pipe A B ∞)
    → i ⊢ x >>= pure ≈ x
  monad-identityʳ stop = ≈stop
  monad-identityʳ (yield value next) = ≈yield Eq.refl λ where .force → ≈lazyˡ λ where .force → monad-identityʳ (next .force)
  monad-identityʳ (demand onNext) = ≈demand λ value → λ where .force → monad-identityʳ (onNext value .force)
  monad-identityʳ (lazy next) = ≈lazyᵇ λ where .force → monad-identityʳ (next .force)

  monad-concatenation : ∀ {i} {α} {A B C : Set α} (x y : Pipe A B ∞) (f : B → Pipe A C ∞)
    → i ⊢ (x >>= f) ++ (y >>= f) ≈ (x ++ y) >>= f
  monad-concatenation stop y f = refl
  monad-concatenation (yield value next) y f =
    begin
      (yield value next >>= f) ++ (y >>= f)
    ≈⟨ refl ⟩
      ((f value) ++ lazy (next ♯>>= f)) ++ (y >>= f)
    ≈⟨ ++-assoc (f value) (lazy (next ♯>>= f)) (y >>= f) ⟩
      (f value) ++ (lazy (next ♯>>= f) ++ (y >>= f))
    ≈⟨ ++-cong refl (≈lazyᵇ λ where .force → monad-concatenation (next .force) y f) ⟩
      (yield value next ++ y) >>= f
    ∎
  monad-concatenation (demand onNext) y f = ≈demand λ value → λ where .force → monad-concatenation (onNext value .force) y f
  monad-concatenation (lazy next) y f = ≈lazyᵇ λ where .force → monad-concatenation (next .force) y f

  monad-associativity : ∀ {i} {α} {A B C D : Set α} (pipe : Pipe A B ∞) (f : B → Pipe A C ∞) (g : C → Pipe A D ∞)
    → i ⊢ (pipe >>= f) >>= g ≈ pipe >>= (λ x → f x >>= g)
  monad-associativity stop f g = ≈stop
  monad-associativity (yield value next) f g =
    begin
      (yield value next >>= f) >>= g
    ≈⟨ refl ⟩
      (f value ++ lazy (next ♯>>= f)) >>= g
    ≈⟨ sym (monad-concatenation (f value) (lazy (next ♯>>= f)) g) ⟩
      (f value >>= g) ++ (lazy (next ♯>>= f) >>= g)
    ≈⟨ ++-cong refl (≈lazyᵇ λ where .force → monad-associativity (next .force) f g) ⟩
      yield value next >>= (λ x → f x >>= g)
    ∎
  monad-associativity (demand onNext) f g = ≈demand λ value → λ where .force → monad-associativity (onNext value .force) f g
  monad-associativity (lazy next) f g = ≈lazyᵇ λ where .force → monad-associativity (next .force) f g

module Functional where
  open import Data.Bool
  open import Data.Nat

  open Core
  open Categorical

  blackHole : ∀ {i} {α} {A B : Set α} → Pipe A B i
  blackHole = demand λ _ → λ where .force → blackHole

  repeat : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
  repeat value = yield value λ where .force → repeat value

  map : ∀ {i} {α} {A B : Set α} → (A → B) → Pipe A B i
  map′ : ∀ {i : Size} {α} {A B : Set α} → (A → B) → A → Pipe A B i
  map f = demand λ value → λ where .force → map′ f value
  map′ f value = yield (f value) (λ where .force → map f)

  filter : ∀ {i} {α} {A : Set α} → (A → Bool) → Pipe A A i
  filter′ : ∀ {i} {α} {A : Set α} → (A → Bool) → A → Pipe A A i
  filter f = demand λ value → λ where .force → filter′ f value
  filter′ f value with f value
  ... | false = filter f
  ... | true = yield value (λ where .force → filter f)

  take : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  take′ : ∀ {i} {α} {A : Set α} → (count : ℕ) → A → Pipe A A i
  take zero = stop
  take (suc n) = demand λ value → λ where .force → take′ n value
  take′ n value = yield value (λ where .force → take n)

  drop : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  drop zero = id
  drop (suc n) = demand λ _ → λ where .force → drop n

module Processing where
  open import Data.Empty.Polymorphic
  open import Data.List
  open import Data.Nat

  open Core

  Fuel : Set
  Fuel = ℕ

  process : ∀ {α} {A : Set α} → Fuel → Pipe ⊥ A ∞ → List A
  process zero _ = []
  process (suc _) stop = []
  process (suc fuel) (yield value next) = value ∷ process fuel (next .force)
  process (suc _) (demand onNext) = []
  process (suc fuel) (lazy next) = process fuel (next .force)

module Examples where

  module RelationExamples where
    open import Data.Nat
    import Relation.Binary.PropositionalEquality as Eq

    open Composition
    open Core
    open Functional
    open Relation.PropositionalEquality

    _ : ∀ {i} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
    _ = helper
      where
      helper : ∀ {i : Size} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
      helper = ≈demand λ _ → λ where .force → ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → helper

  module ProcessingExamples where
    open import Data.Empty.Polymorphic
    open import Data.List using (List; []; _∷_)
    open import Data.Nat
    open import Data.Nat.DivMod using (_%_)
    open import Data.Nat.Properties using (+-suc)
    open import Data.Vec as Vec using (Vec; []; _∷_)
    open import Relation.Binary
    open import Relation.Binary.PropositionalEquality
    open Relation.Binary.PropositionalEquality.≡-Reasoning

    open Composition
    open Core
    open Functional
    open Processing

    fromVec : ∀ {i} {α} {A : Set α} {size : ℕ}
      → (xs : Vec A size)
      → Pipe ⊥ A i
    fromVec [] = stop
    fromVec (x ∷ xs) = yield x λ where .force → fromVec xs

    process-id : ∀ {α} {A : Set α} (size : ℕ) (xs : Vec A size)
      → process (size + size) (fromVec xs |> id) ≡ Vec.toList xs
    process-id 0 [] = refl
    process-id (suc size) (x ∷ xs) =
      begin
        process (suc size + suc size) (fromVec (x ∷ xs) |> id)
      ≡⟨⟩
        process (size + suc size) (fromVec xs |> id′ x)
      ≡⟨ cong (λ n → process n (fromVec xs |> id′ x)) (+-suc size size) ⟩
        process (suc (size + size)) (fromVec xs |> id′ x)
      ≡⟨⟩
        (x ∷ process (size + size) (fromVec xs |> id))
      ≡⟨ cong (x ∷_) (process-id size xs) ⟩
        Vec.toList (x ∷ xs)
      ∎

    _ : process 100 (fromVec (1 ∷ 2 ∷ 3 ∷ []) |> map (_+ 1)) ≡ 2 ∷ 3 ∷ 4 ∷ []
    _ = refl

    nats : ∀ {i} → Pipe ⊥ ℕ i
    natsFrom : ∀ {i} → ℕ → Pipe ⊥ ℕ i
    nats = natsFrom 0
    natsFrom n = yield n λ where .force → natsFrom (suc n)

    _ : process 100 (nats |> drop 5 |> filter (λ n → n % 2 ≡ᵇ 0) |> map (_* 2) |> take 3) ≡ 12 ∷ 16 ∷ 20 ∷ []
    _ = refl

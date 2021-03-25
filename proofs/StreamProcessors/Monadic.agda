module StreamProcessors.Monadic where

open import Algebra.Definitions
open import Category.Applicative
open import Category.Functor
open import Category.Monad
open import Codata.Thunk as Thunk using (Thunk; force)
open import Function using (_∘_; _∘′_)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_)
open import Size using (∞)

open import Category

open import StreamProcessors.Algebra
open import StreamProcessors.Composition
open import StreamProcessors.Core
open import StreamProcessors.Relation
open StreamProcessors.Relation.PropositionalEquality
open StreamProcessors.Relation.PropositionalEquality.≈-Reasoning

module Operators where
  infixl 4 _<$>_ _⊛_ _>>=_

  pure : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
  pure value = yield value stop♯

  _<$>_ : ∀ {i} {α} {A B C : Set α} → (B → C) → Pipe A B i → Pipe A C i
  _<$>♯_ : ∀ {i} {α} {A B C : Set α} → (B → C) → Thunk (Pipe A B) i → Thunk (Pipe A C) i
  f <$> stop = stop
  f <$> yield value next = yield (f value) (f <$>♯ next)
  f <$> demand onNext = demand λ value → f <$>♯ (onNext value)
  f <$> lazy next = lazy (f <$>♯ next)
  (f <$>♯ x) .force = f <$> x .force

  _⊛_ : ∀ {i} {α} {A B C : Set α} → Pipe A (B → C) i → Pipe A B i → Pipe A C i
  _♯⊛_ : ∀ {i} {α} {A B C : Set α} → Thunk (Pipe A (B → C)) i → Pipe A B i → Thunk (Pipe A C) i
  stop ⊛ x = stop
  yield value next ⊛ x = (value <$> x) ++ lazy (next ♯⊛ x)
  demand onNext ⊛ x = demand λ value → onNext value ♯⊛ x
  lazy next ⊛ x = lazy (next ♯⊛ x)
  (f ♯⊛ x) .force = f .force ⊛ x

  _>>=_ : ∀ {i} {α} {A B C : Set α} → Pipe A B i → (B → Pipe A C i) → Pipe A C i
  _♯>>=_ : ∀ {i} {α} {A B C : Set α} → Thunk (Pipe A B) i → (B → Pipe A C i) → Thunk (Pipe A C) i
  stop >>= f = stop
  yield value next >>= f = f value ++ lazy (next ♯>>= f)
  demand onNext >>= f = demand λ value → onNext value ♯>>= f
  lazy next >>= f = lazy (next ♯>>= f)
  (pipe ♯>>= f) .force = pipe .force >>= f

functor : ∀ {i} {α} {A : Set α} → RawFunctor {α} (λ B → Pipe A B i)
functor =
  record
    { _<$>_ = Operators._<$>_
    }

applicative : ∀ {i} {α} {A : Set α} → RawApplicative {α} (λ B → Pipe A B i)
applicative =
  record
    { pure = Operators.pure
    ; _⊛_ = Operators._⊛_
    }

monad : ∀ {i} {α} {A : Set α} → RawMonad {α} (λ B → Pipe A B i)
monad =
  record
    { return = Operators.pure
    ; _>>=_ = Operators._>>=_
    }

instance pipeFunctor = functor
instance pipeApplicative = applicative
instance pipeMonad = monad

module Proofs where
  open Operators

  infixl 4 _≈<$>_
  infixl 4 _≈⊛_

  _≈<$>_ : ∀ {i} {α} {A B C : Set α} {f₁ f₂ : B → C} {x₁ x₂ : Pipe A B ∞}
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

  _≈⊛_ : ∀ {i} {α} {A B C : Set α} {f₁ f₂ : Pipe A (B → C) ∞} {x₁ x₂ : Pipe A B ∞}
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

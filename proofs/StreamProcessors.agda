module StreamProcessors where

open import Codata.Colist as Colist using (Colist; []; _∷_)
open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Maybe as Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Data.Product as Product using (_×_; _,_; ∃-syntax)
open import Level using (Level)
open import Size

infixl 9 _|>_ _♯|>_ _|>♯_ _♯|>♯_
infixr 9 _<|_
infixr 5 _++_ _♯++_

Stream : ∀ {α} → (A : Set α) → Set α
Stream A = Colist A ∞

data Pipe {α : Level} (A B : Set α) (i : Size) : Set α where
  stop : Pipe A B i
  yield : (value : B) → (next : Thunk (Pipe A B) i) → Pipe A B i
  demand : (onNext : (value : A) → Thunk (Pipe A B) i) → Pipe A B i
  lazy : (next : Thunk (Pipe A B) i) → Pipe A B i

stop♯ : ∀ {i} {α} {A B : Set α} → Thunk (Pipe A B) i
stop♯ .force = stop

Fuel : Set
Fuel = ℕ

process : ∀ {α} {A B : Set α} → Fuel → Pipe A B ∞ → Stream A → Stream B
process zero _ _ = []
process (suc _) stop xs = []
process (suc fuel) (yield value next) xs =
  value ∷ λ where .force → process fuel (next .force) xs
process (suc _) (demand onNext) [] = []
process (suc fuel) (demand onNext) (x ∷ xs) =
  process fuel (onNext x .force) (xs .force)
process (suc fuel) (lazy next) xs =
  process fuel (next .force) xs

id : ∀ {i} {α} {A : Set α} → Pipe A A i
private id′ : ∀ {i} {α} {A : Set α} → A → Pipe A A i
id = demand λ value → λ where .force → id′ value
id′ value = yield value λ where .force → id

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

_++_ : ∀ {i : Size} {α} → {A B : Set α} → Pipe A B i → Pipe A B i → Pipe A B i
private _♯++_ : ∀ {i : Size} {α} → {A B : Set α} → Thunk (Pipe A B) i → Pipe A B i → Thunk (Pipe A B) i
stop ++ b = b
yield value next ++ b = yield value (next ♯++ b)
demand onNext ++ b = demand λ value → onNext value ♯++ b
lazy next ++ b = lazy (next ♯++ b)
a ♯++ b = λ where .force → a .force ++ b

module Relation where
  open import Level
  open import Relation.Binary

  module Generic {α ρ} (R : {T : Set α} → Rel T ρ) (R-isEquivalence : {T : Set α} → IsEquivalence (R {T})) where
    infix 1 _⊢_≈_

    data _⊢_≈_ (i : Size) {A B : Set α} : Rel (Pipe A B ∞) (α Level.⊔ ρ) where
      ≈stop :
          i ⊢ stop ≈ stop
      ≈thunks : ∀ {a b : Pipe A B ∞}
        → (rel : Thunk.Thunk^R (λ i → _⊢_≈_ i {A} {B}) i (λ where .force → a) (λ where .force → b))
        → i ⊢ a ≈ b
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
    sym (≈thunks rel) = ≈thunks λ where .force → sym (rel .force)
    sym (≈yield value next) = ≈yield (R-isEquivalence .IsEquivalence.sym value) λ where .force → sym (next .force)
    sym (≈demand onNext) = ≈demand λ value → λ where .force → sym (onNext value .force)
    sym (≈lazyˡ next) = ≈lazyʳ λ where .force → sym (next .force)
    sym (≈lazyʳ next) = ≈lazyˡ λ where .force → sym (next .force)

    trans : ∀ {i : Size} {A B : Set α} → Transitive (_⊢_≈_ i {A} {B})
    trans ≈stop bc = bc
    trans (≈thunks rel) bc = ≈thunks λ where .force → trans (rel .force) bc
    trans (≈lazyˡ next) bc = ≈lazyˡ λ where .force → trans (next .force) bc
    trans ab (≈thunks rel) = ≈thunks λ where .force → trans ab (rel .force)
    trans ab (≈lazyʳ next) = ≈lazyʳ λ where .force → trans ab (next .force)
    trans (≈yield value₁ next₁) (≈yield value₂ next₂) = ≈yield (R-isEquivalence .IsEquivalence.trans value₁ value₂) λ where .force → trans (next₁ .force) (next₂ .force)
    trans (≈demand onNext₁) (≈demand onNext₂) = ≈demand λ value → λ where .force → trans (onNext₁ value .force) (onNext₂ value .force)
    trans (≈lazyʳ next₁) (≈lazyˡ next₂) = ≈thunks λ where .force → trans (next₁ .force) (next₂ .force)

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

    _≈|>_ : ∀ {i} {A B C : Set α} {a c : Pipe A B ∞} {b d : Pipe B C ∞}
      → i ⊢ a ≈ c
      → i ⊢ b ≈ d
      → i ⊢ a |> b ≈ c |> d
    ac ≈|> ≈thunks rel = ≈thunks λ where .force → ac ≈|> rel .force
    ac ≈|> ≈stop = ≈stop
    ac ≈|> ≈yield value next = ≈yield value λ where .force → ac ≈|> next .force
    ≈stop ≈|> ≈demand onNext = ≈stop
    ≈thunks rel ≈|> ≈demand onNext = ≈thunks λ where .force → rel .force ≈|> ≈demand onNext
    ≈yield {value} Eq.refl next ≈|> ≈demand onNext = ≈lazyᵇ λ where .force → next .force ≈|> onNext value .force
    ≈demand onNext₁ ≈|> ≈demand onNext₂ = ≈demand λ value → λ where .force → onNext₁ value .force ≈|> ≈demand onNext₂
    ≈lazyˡ next ≈|> ≈demand onNext = ≈lazyˡ λ where .force → next .force ≈|> ≈demand onNext
    ≈lazyʳ next ≈|> ≈demand onNext = ≈lazyʳ λ where .force → next .force ≈|> ≈demand onNext
    ac ≈|> ≈lazyˡ next = ≈lazyˡ λ where .force → ac ≈|> next .force
    ac ≈|> ≈lazyʳ next = ≈lazyʳ λ where .force → ac ≈|> next .force

module Algebra where
  open import Algebra.Definitions
  open import Algebra.Structures
  import Relation.Binary.PropositionalEquality as Eq

  open Relation.PropositionalEquality

  ++-cong : ∀ {i} {α} {A B : Set α} → Congruent₂ (_⊢_≈_ i {A} {B}) _++_
  ++-cong ≈stop b = b
  ++-cong (≈thunks rel) b = ≈thunks λ where .force → ++-cong (rel .force) b
  ++-cong (≈yield value next) b = ≈yield value λ where .force → ++-cong (next .force) b
  ++-cong (≈demand onNext) b = ≈demand λ value → λ where .force → ++-cong (onNext value .force) b
  ++-cong (≈lazyˡ next) b = ≈lazyˡ λ where .force → ++-cong (next .force) b
  ++-cong (≈lazyʳ next) b = ≈lazyʳ λ where .force → ++-cong (next .force) b

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

module Reasoning where
  open import Algebra.Definitions
  import Relation.Binary.PropositionalEquality as Eq

  open import Category
  open Relation.PropositionalEquality

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

module Functional where
  open import Data.Bool

  blackHole : ∀ {i} {α} {A B : Set α} → Pipe A B i
  blackHole = demand λ _ → λ where .force → blackHole

  repeat : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
  repeat value = yield value λ where .force → repeat value

  map : ∀ {i} {α} {A B : Set α} → (A → B) → Pipe A B i
  private map′ : ∀ {i : Size} {α} {A B : Set α} → (A → B) → A → Pipe A B i
  map f = demand λ value → λ where .force → map′ f value
  map′ f value = yield (f value) (λ where .force → map f)

  filter : ∀ {i} {α} {A : Set α} → (A → Bool) → Pipe A A i
  private filter′ : ∀ {i} {α} {A : Set α} → (A → Bool) → A → Pipe A A i
  filter f = demand λ value → λ where .force → filter′ f value
  filter′ f value with f value
  ... | false = filter f
  ... | true = yield value (λ where .force → filter f)

  take : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  private take′ : ∀ {i} {α} {A : Set α} → (count : ℕ) → A → Pipe A A i
  take zero = stop
  take (suc n) = demand λ value → λ where .force → take′ n value
  take′ n value = yield value (λ where .force → take n)

  drop : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  drop zero = id
  drop (suc n) = demand λ _ → λ where .force → drop n

module Categorical where
  open import Category.Applicative
  open import Category.Functor
  open import Function using (_∘_)
  open import Relation.Binary.PropositionalEquality as Eq using (_≡_)

  open Algebra
  open Functional
  open Relation.PropositionalEquality
  open Relation.PropositionalEquality.≈-Reasoning

  infixl 4 _<$>_
  infixl 4 _⊛_

  pure : ∀ {i} {α : Level} {A B : Set α} → B → Pipe A B i
  pure value = yield value stop♯

  _<$>_ : ∀ {i} {α : Level} {A B C : Set α} → (B → C) → Pipe A B i → Pipe A C i
  f <$> pipe = pipe |> map f

  _⊛_ : ∀ {i} {α : Level} {A B C : Set α} → Pipe A (B → C) i → Pipe A B i → Pipe A C i
  private _♯⊛_ : ∀ {i} {α : Level} {A B C : Set α} → Thunk (Pipe A (B → C)) i → Pipe A B i → Thunk (Pipe A C) i
  stop ⊛ x = stop
  yield value next ⊛ x = (value <$> x) ++ lazy (next ♯⊛ x)
  demand onNext ⊛ x = demand λ value → onNext value ♯⊛ x
  lazy next ⊛ x = lazy (next ♯⊛ x)
  f ♯⊛ x = λ where .force → f .force ⊛ x

  functor : ∀ {i} {α : Level} {A : Set α} → RawFunctor {α} (λ B → Pipe A B i)
  functor =
    record
      { _<$>_ = _<$>_
      }

  applicative : ∀ {i} {α : Level} {A : Set α} → RawApplicative {α} (λ B → Pipe A B i)
  applicative =
    record
      { pure = pure
      ; _⊛_ = _⊛_
      }

  instance pipeFunctor = functor

  infixl 4 _≈<$>_
  infixl 4 _≈⊛_

  ≈map : ∀ {i} {α : Level} {A B C : Set α} {f₁ f₂ : B → C} {x₁ x₂ : Pipe A B ∞}
    → f₁ ≡ f₂
    → i ⊢ x₁ ≈ x₂
    → i ⊢ x₁ |> map f₁ ≈ x₂ |> map f₂
  ≈map Eq.refl ≈stop = ≈stop
  ≈map Eq.refl (≈thunks rel) = ≈thunks λ where .force → ≈map Eq.refl (rel .force)
  ≈map {f₁ = f₁} Eq.refl (≈yield value next) = ≈lazyᵇ λ where .force → ≈yield (Eq.cong f₁ value) λ where .force → ≈map Eq.refl (next .force)
  ≈map Eq.refl (≈demand onNext) = ≈demand λ value → λ where .force → ≈map Eq.refl (onNext value .force)
  ≈map Eq.refl (≈lazyˡ next) = ≈lazyˡ λ where .force → ≈map Eq.refl (next .force)
  ≈map Eq.refl (≈lazyʳ next) = ≈lazyʳ λ where .force → ≈map Eq.refl (next .force)

  _≈<$>_ : ∀ {i} {α : Level} {A B C : Set α} {f₁ f₂ : B → C} {x₁ x₂ : Pipe A B ∞}
    → f₁ ≡ f₂
    → i ⊢ x₁ ≈ x₂
    → i ⊢ f₁ <$> x₁ ≈ f₂ <$> x₂
  f ≈<$> x = ≈map f x

  functor-identity : ∀ {i} {α} {A B : Set α}
    → (pipe : Pipe A B ∞)
    → let _<$>_ = functor .RawFunctor._<$>_ in
      i ⊢ Function.id <$> pipe ≈ pipe
  functor-identity stop = ≈stop
  functor-identity (yield value next) = ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → functor-identity (next .force)
  functor-identity (demand onNext) = ≈demand λ value → λ where .force → functor-identity (onNext value .force)
  functor-identity (lazy next) = ≈lazyᵇ λ where .force → functor-identity (next .force)

  functor-compose : ∀ {i} {α} {A B C D : Set α}
    → (pipe : Pipe A B ∞) (f : B → C) (g : C → D)
    → i ⊢ (g ∘ f) <$> pipe ≈ ((g <$>_) ∘ (f <$>_)) pipe
  functor-compose stop f g = ≈stop
  functor-compose (yield value next) f g = ≈lazyᵇ λ where .force → ≈lazyʳ λ where .force → ≈yield Eq.refl λ where .force → functor-compose (next .force) f g
  functor-compose (demand onNext) f g = ≈demand λ value → λ where .force → functor-compose (onNext value .force) f g
  functor-compose (lazy next) f g = ≈lazyᵇ λ where .force → functor-compose (next .force) f g

  functor-concatenation : ∀ {i} {α} {A B C : Set α}
    → (f : B → C) (x y : Pipe A B ∞)
    → i ⊢ (f <$> x) ++ (f <$> y) ≈ f <$> (x ++ y)
  functor-concatenation f stop y = refl
  functor-concatenation f (yield value next) y =
    begin
      (f <$> yield value next) ++ (f <$> y)
    ≈⟨ ≈lazyˡ (λ where .force → ≈yield Eq.refl λ where .force → refl) ⟩
      (yield (f value) (λ where .force → f <$> next .force)) ++ (f <$> y)
    ≈⟨ ≈yield Eq.refl (λ where .force → refl) ⟩
      yield (f value) (λ where .force → (f <$> next .force) ++ (f <$> y))
    ≈⟨ ≈yield Eq.refl (λ where .force → functor-concatenation f (next .force) y) ⟩
      yield (f value) (λ where .force → f <$> (next .force ++ y))
    ≈⟨ ≈lazyʳ (λ where .force → ≈yield Eq.refl λ where .force → refl) ⟩
      f <$> (yield value next ++ y)
    ∎
  functor-concatenation f (demand onNext) y = ≈demand λ value → λ where .force → functor-concatenation f (onNext value .force) y
  functor-concatenation f (lazy next) y = ≈lazyᵇ λ where .force → functor-concatenation f (next .force) y

  _≈⊛_ : ∀ {i} {α : Level} {A B C : Set α} {f₁ f₂ : Pipe A (B → C) ∞} {x₁ x₂ : Pipe A B ∞}
    → i ⊢ f₁ ≈ f₂
    → i ⊢ x₁ ≈ x₂
    → i ⊢ f₁ ⊛ x₁ ≈ f₂ ⊛ x₂
  ≈stop ≈⊛ x = ≈stop
  ≈thunks rel ≈⊛ x = ≈thunks λ where .force → rel .force ≈⊛ x
  ≈yield value next ≈⊛ x = ++-cong (value ≈<$> x) (≈lazyᵇ λ where .force → next .force ≈⊛ x)
  ≈demand onNext ≈⊛ x = ≈demand λ value → λ where .force → onNext value .force ≈⊛ x
  ≈lazyˡ next ≈⊛ x = ≈lazyˡ λ where .force → next .force ≈⊛ x
  ≈lazyʳ next ≈⊛ x = ≈lazyʳ λ where .force → next .force ≈⊛ x

  applicative-identity : ∀ {i} {α} {A B : Set α}
    → (x : Pipe A B ∞)
    → i ⊢ pure Function.id ⊛ x ≈ x
  applicative-identity stop = ≈lazyˡ λ where .force → ≈stop
  applicative-identity (yield value next) =
    begin
      pure Function.id ⊛ yield value next
    ≈⟨ ≈lazyᵇ (λ where .force → ≈yield Eq.refl λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
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

  applicative-homomorphism : ∀ {i} {α} {A B C : Set α}
    → (f : B → C) (x : B)
    → i ⊢ pure {A = A} f ⊛ pure {A = A} x ≈ pure {A = A} (f x)
  applicative-homomorphism f x = ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → ≈lazyˡ λ where .force → refl

  applicative-map : ∀ {i} {α} {A B C : Set α}
    → (f : B → C) (x : Pipe A B ∞)
    → i ⊢ pure f ⊛ x ≈ f <$> x
  applicative-map f x = ++-identityʳ (≈lazyˡ λ where .force → refl) (f <$> x)

  applicative-interchange : ∀ {i} {α} {A B : Set α}
    → (pipe : Pipe A (A → B) ∞) (x : A)
    → i ⊢ pipe ⊛ pure x ≈ pure (λ g → g x) ⊛ pipe
  applicative-interchange stop x = ≈lazyʳ λ where .force → refl
  applicative-interchange (yield value next) x =
    begin
      yield value next ⊛ pure x
    ≈⟨ ≈lazyᵇ (λ where .force → ≈yield Eq.refl λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      (value <$> pure x) ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈lazyˡ (λ where .force → ≈yield Eq.refl λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      yield (value x) stop♯ ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → refl) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → next .force ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → ≈lazyʳ λ where .force → refl) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → lazy next ⊛ pure x)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → applicative-interchange (lazy next) x) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → pure (λ g → g x) ⊛ lazy next)
    ≈⟨ ≈yield Eq.refl (λ where .force → ≈lazyᵇ λ where .force → ≈lazyˡ λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      pure ((λ g → g x) value) ++ lazy (λ where .force → pure (λ g → g x) ⊛ next .force)
    ≈⟨ ≈lazyʳ (λ where .force → ≈yield Eq.refl λ where .force → ≈lazyˡ λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
      ((λ g → g x) <$> yield value next) ++ lazy (stop♯ ♯⊛ next .force)
    ≈⟨ ≈lazyᵇ (λ where .force → ≈yield Eq.refl λ where .force → ++-cong refl (≈lazyᵇ λ where .force → refl)) ⟩
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

  applicative-composition : ∀ {i} {α} {A B C D : Set α}
    → (g : Pipe A (C → D) ∞) (f : Pipe A (B → C) ∞) (x : Pipe A B ∞)
    → i ⊢ (((pure (λ g f x → g (f x)) ⊛ g) ⊛ f) ⊛ x) ≈ g ⊛ (f ⊛ x)
  applicative-composition stop f x =
    ≈lazyˡ λ where .force → ≈stop
  applicative-composition (yield value next) f x =
    begin
      ((pure (λ g f x → g (f x)) ⊛ yield value next) ⊛ f) ⊛ x
    ≈⟨ ((refl {x = pure (λ g f x → g (f x))} ≈⊛ ≈yield Eq.refl λ where .force → refl) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      ((pure (λ g f x → g (f x)) ⊛ (pure value ++ next .force)) ⊛ f) ⊛ x
    ≈⟨ (applicative-map (λ g f x → g (f x)) (pure value ++ next .force) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      (((λ g f x → g (f x)) <$> (pure value ++ next .force)) ⊛ f) ⊛ x
    ≈⟨ (sym (functor-concatenation (λ g f x → g (f x)) (pure value) (next .force)) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      ((((λ g f x → g (f x)) <$> pure value) ++ ((λ g f x → g (f x)) <$> next .force)) ⊛ f) ⊛ x
    ≈⟨ (++-cong (sym (applicative-map (λ g f x → g (f x)) (pure value))) (sym (applicative-map (λ g f x → g (f x)) (next .force))) ≈⊛ refl {x = f}) ≈⊛ refl {x = x} ⟩
      (((pure (λ g f x → g (f x)) ⊛ pure value) ++ (pure (λ g f x → g (f x)) ⊛ next .force)) ⊛ f) ⊛ x
    ≈⟨ sym (applicative-concatenation (pure (λ g f x → g (f x)) ⊛ pure value) (pure (λ g f x → g (f x)) ⊛ next .force) f) ≈⊛ refl ⟩
      (((pure (λ g f x → g (f x)) ⊛ pure value) ⊛ f) ++ ((pure (λ g f x → g (f x)) ⊛ next .force) ⊛ f)) ⊛ x
    ≈⟨ sym (applicative-concatenation ((pure (λ g f x → g (f x)) ⊛ pure value) ⊛ f) ((pure (λ g f x → g (f x)) ⊛ next .force) ⊛ f) x) ⟩
      (((pure (λ g f x → g (f x)) ⊛ pure value) ⊛ f) ⊛ x) ++ (((pure (λ g f x → g (f x)) ⊛ next .force) ⊛ f) ⊛ x)
    ≈⟨ ++-cong ((refl {x = pure (λ g f x → g (f x)) ⊛ pure value} ≈⊛ refl {x = f}) ≈⊛ refl {x = x}) (≈lazyʳ λ where .force → refl) ⟩
      (((pure (λ g f x → g (f x)) ⊛ pure value) ⊛ f) ⊛ x) ++ lazy (λ where .force → ((pure (λ g f x → g (f x)) ⊛ next .force) ⊛ f) ⊛ x)
    ≈⟨ ++-cong (≈thunks λ where .force → applicative-composition (pure value) f x) (≈lazyᵇ λ where .force → applicative-composition (next .force) f x) ⟩
      (pure value ⊛ (f ⊛ x)) ++ lazy (λ where .force → next .force ⊛ (f ⊛ x))
    ≈⟨ ++-cong (applicative-map value (f ⊛ x)) (≈lazyᵇ λ where .force → refl) ⟩
      (value <$> (f ⊛ x)) ++ lazy (next ♯⊛ (f ⊛ x))
    ≈⟨ refl ⟩
      yield value next ⊛ (f ⊛ x)
    ∎
  applicative-composition (demand onNext) f x =
    begin
      ((pure (λ g f x → g (f x)) ⊛ demand onNext) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((((λ g f x → g (f x)) <$> demand onNext) ++ lazy (stop♯ ♯⊛ demand onNext)) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((demand (λ value → onNext value ♯|> map (λ g f x → g (f x))) ++ lazy (stop♯ ♯⊛ demand onNext)) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((demand (λ value → onNext value ♯|> map (λ g f x → g (f x)) ♯++ lazy (stop♯ ♯⊛ demand onNext))) ⊛ f) ⊛ x
    ≈⟨ ≈demand (λ value → λ where .force → ++-cong refl (≈lazyᵇ λ where .force → ≈stop)) ≈⊛ refl {x = f} ≈⊛ refl {x = x} ⟩
      ((demand (λ value → onNext value ♯|> map (λ g f x → g (f x)) ♯++ lazy (stop♯ ♯⊛ onNext value .force))) ⊛ f) ⊛ x
    ≈⟨ ≈demand (λ value → λ where .force → applicative-composition (onNext value .force) f x) ⟩
      demand (λ value → onNext value ♯⊛ (f ⊛ x))
    ≈⟨ refl ⟩
      demand onNext ⊛ (f ⊛ x)
    ∎
  applicative-composition (lazy next) f x =
    begin
      ((pure (λ g f x → g (f x)) ⊛ lazy next) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((((λ g f x → g (f x)) <$> lazy next) ++ lazy (stop♯ ♯⊛ lazy next)) ⊛ f) ⊛ x
    ≈⟨ ++-cong (refl {x = (λ g f x → g (f x)) <$> lazy next}) (≈lazyᵇ λ where .force → ≈stop) ≈⊛ refl {x = f} ≈⊛ refl {x = x} ⟩
      ((((λ g f x → g (f x)) <$> lazy next) ++ _) ⊛ f) ⊛ x
    ≈⟨ refl ⟩
      ((((λ g f x → g (f x)) <$> lazy next) ++ _) ⊛ f) ⊛ x
    ≈⟨ ≈lazyᵇ (λ where .force → applicative-composition (next .force) f x) ⟩
      lazy next ⊛ (f ⊛ x)
    ∎

module Examples where
  open import Codata.Colist.Bisimilarity renaming (_⊢_≈_ to _L⊢_≈_; refl to Lrefl)
  open import Data.List using (List; []; _∷_)
  open import Data.Nat.DivMod using (_%_)
  open import Data.Nat.Properties using (+-suc)
  open import Data.Vec as Vec using (Vec; []; _∷_)
  import Relation.Binary.PropositionalEquality as Eq

  open Functional
  open Relation.PropositionalEquality

  nats : ∀ {i} → Colist ℕ i
  natsFrom : ∀ {i} → ℕ → Colist ℕ i
  nats = natsFrom 0
  natsFrom n = n ∷ λ where .force → natsFrom (suc n)

  process-id : ∀ {i} {α} {A : Set α} (size : ℕ) (xs : Vec A size)
    → let xs-stream = Colist.fromList (Vec.toList xs)
      in i L⊢ process (size + size) id xs-stream ≈ xs-stream
  process-id 0 [] = []
  process-id (suc size) (x ∷ xs) =
    Codata.Colist.Bisimilarity.≈-Reasoning.begin
      process (suc size + suc size) id (Colist.fromList (Vec.toList (x ∷ xs)))
    Codata.Colist.Bisimilarity.≈-Reasoning.≈⟨ Lrefl ⟩
      process (size + suc size) (id′ x) (Colist.fromList (Vec.toList xs))
    Codata.Colist.Bisimilarity.≈-Reasoning.≈⟨ fromEq (Eq.cong (λ n → process n (id′ x) (Colist.fromList (Vec.toList xs))) (+-suc size size)) ⟩
      process (suc (size + size)) (id′ x) (Colist.fromList (Vec.toList xs))
    Codata.Colist.Bisimilarity.≈-Reasoning.≈⟨ Eq.refl ∷ (λ where .force → Lrefl) ⟩
      (x ∷ λ where .force → process (size + size) id (Colist.fromList (Vec.toList xs)))
    Codata.Colist.Bisimilarity.≈-Reasoning.≈⟨ Eq.refl ∷ (λ where .force → process-id size xs) ⟩
      Colist.fromList (Vec.toList (x ∷ xs))
    Codata.Colist.Bisimilarity.≈-Reasoning.∎

  _ : ∀ {i} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
  _ = helper
    where
    helper : ∀ {i : Size} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
    helper = ≈demand λ _ → λ where .force → ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → helper

  _ : ∀ {i} → i L⊢ process 100 (map (_+ 1)) (Colist.fromList (1 ∷ 2 ∷ 3 ∷ [])) ≈ Colist.fromList (2 ∷ 3 ∷ 4 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

  _ : ∀ {i} → i L⊢ process 100 (drop 5 |> filter (λ n → n % 2 ≡ᵇ 0) |> map (_* 2) |> take 3) nats ≈ Colist.fromList (12 ∷ 16 ∷ 20 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

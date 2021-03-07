module StreamProcessors where

open import Codata.Colist as Colist using (Colist; []; _∷_)
open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Maybe as Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Data.Product as Product using (_×_; _,_; ∃-syntax)
open import Level using (Level)
open import Size

infixl 9 _|>_
infixr 9 _<|_
infixr 5 _++_

Stream : ∀ {α} → (A : Set α) → Set α
Stream A = Colist A ∞

data Pipe {α : Level} (A B : Set α) (i : Size) : Set α where
  stop : Pipe A B i
  yield : (value : B) → (next : Thunk (Pipe A B) i) → Pipe A B i
  demand : (f : A → Thunk (Pipe A B) i) → Pipe A B i
  lazy : (next : Thunk (Pipe A B) i) → Pipe A B i

Fuel : Set
Fuel = ℕ

process : ∀ {α} {A B : Set α} → Fuel → Pipe A B ∞ → Stream A → Stream B
process zero _ _ = []
process (suc _) stop xs = []
process (suc fuel) (yield value next) xs =
  value ∷ λ where .force → process fuel (next .force) xs
process (suc _) (demand f) [] = []
process (suc fuel) (demand f) (x ∷ xs) =
  process fuel (f x .force) (xs .force)
process (suc fuel) (lazy next) xs =
  process fuel (next .force) xs

id : ∀ {i} {α} {A : Set α} → Pipe A A i
private id′ : ∀ {i} {α} {A : Set α} → A → Pipe A A i
id = demand λ x → λ where .force → id′ x
id′ x = yield x λ where .force → id

_|>_ : ∀  {i} {α} {A B C : Set α} → Pipe A B i → Pipe B C i → Pipe A C i
_ |> stop = stop
up |> yield value next = yield value (λ where .force → up |> next .force)
stop |> demand f = stop
yield value next |> demand f = lazy λ where .force → next .force |> f value .force
demand f |> down@(demand _) = demand (λ x → λ where .force → f x .force |> down)
lazy up |> down@(demand _) = lazy λ where .force → up .force |> down
up |> lazy down = lazy λ where .force → up |> down .force

_<|_ : ∀ {i} {α} {A B C : Set α} → Pipe B C i → Pipe A B i → Pipe A C i
down <| up = up |> down

_++_ : ∀ {i : Size} {α} → {A B : Set α} → Pipe A B i → Pipe A B i → Pipe A B i
stop ++ b = b
yield value next ++ b = yield value λ where .force → next .force ++ b
demand f ++ b = demand λ x → λ where .force → f x .force ++ b
lazy next ++ b = lazy λ where .force → next .force ++ b

module Relation where
  open import Level
  open import Relation.Binary

  module Generic {α ρ} {A B : Set α} (R : REL B B ρ) (Rsym : Symmetric R) (Rtrans : Transitive R) where
    infix 1 _⊢_≈_

    data _⊢_≈_ (i : Size) : REL (Pipe A B ∞) (Pipe A B ∞) (α Level.⊔ ρ) where
      ≈refl : ∀ {pipe : Pipe A B ∞}
        → i ⊢ pipe ≈ pipe
      ≈thunks : ∀ {a b : Pipe A B ∞}
        → (rel : Thunk.Thunk^R _⊢_≈_ i (λ where .force → a) (λ where .force → b))
        → i ⊢ a ≈ b
      ≈yield : ∀ {value₁ value₂ next₁ next₂}
        → (value : R value₁ value₂)
        → (next : Thunk.Thunk^R _⊢_≈_ i next₁ next₂)
        → i ⊢ yield value₁ next₁ ≈ yield value₂ next₂
      ≈demand : ∀ {f : A → Thunk (Pipe A B) ∞} {g : A → Thunk (Pipe A B) ∞}
        → (onNext : ∀ x → Thunk.Thunk^R _⊢_≈_ i (f x) (g x))
        → i ⊢ demand f ≈ demand g
      ≈lazyˡ : ∀ {a b}
        → (next : Thunk.Thunk^R _⊢_≈_ i a (λ where .force → b))
        → i ⊢ lazy a ≈ b
      ≈lazyʳ : ∀ {a b}
        → (next : Thunk.Thunk^R _⊢_≈_ i (λ where .force → a) b)
        → i ⊢ a ≈ lazy b

    ≈lazyᵇ : ∀ {i : Size} {a b : Thunk (Pipe A B) ∞}
      → (next : Thunk.Thunk^R _⊢_≈_ i a b)
      → i ⊢ lazy a ≈ lazy b
    ≈lazyᵇ next = ≈lazyˡ λ where .force → ≈lazyʳ λ where .force → next .force

    sym : ∀ {i : Size} → Symmetric (i ⊢_≈_)
    sym ≈refl = ≈refl
    sym (≈thunks rel) = ≈thunks λ where .force → sym (rel .force)
    sym (≈yield value next) = ≈yield (Rsym value) λ where .force → sym (next .force)
    sym (≈demand onNext) = ≈demand λ x → λ where .force → sym (onNext x .force)
    sym (≈lazyˡ next) = ≈lazyʳ λ where .force → sym (next .force)
    sym (≈lazyʳ next) = ≈lazyˡ λ where .force → sym (next .force)

    trans : ∀ {i : Size} → Transitive (i ⊢_≈_)
    trans ≈refl bc = bc
    trans (≈thunks rel) bc = ≈thunks λ where .force → trans (rel .force) bc
    trans (≈lazyˡ next) bc = ≈lazyˡ λ where .force → trans (next .force) bc
    trans ab ≈refl = ab
    trans ab (≈thunks rel) = ≈thunks λ where .force → trans ab (rel .force)
    trans ab (≈lazyʳ next) = ≈lazyʳ λ where .force → trans ab (next .force)
    trans (≈yield value₁ next₁) (≈yield value₂ next₂) = ≈yield (Rtrans value₁ value₂) λ where .force → trans (next₁ .force) (next₂ .force)
    trans (≈demand onNext) (≈demand g) = ≈demand λ x → λ where .force → trans (onNext x .force) (g x .force)
    trans (≈lazyʳ next₁) (≈lazyˡ next₂) = ≈thunks λ where .force → trans (next₁ .force) (next₂ .force)

  module PropositionalEquality {α} {A B : Set α} where
    import Relation.Binary.PropositionalEquality as Eq
    open Generic {α} {α} {A} {B} Eq._≡_ Eq.sym Eq.trans public

    isEquivalence : ∀ {α} → (i : Size) (A B : Set α) → IsEquivalence (i ⊢_≈_)
    isEquivalence i A B =
      record
        { refl = ≈refl
        ; sym = sym
        ; trans = trans
        }

module Algebra where
  open import Algebra.Definitions
  open import Algebra.Structures
  import Relation.Binary.PropositionalEquality as Eq

  open Relation.PropositionalEquality

  ++-cong : ∀ {i} {α} {A B : Set α} → Congruent₂ (i ⊢_≈_) (_++_ {∞} {α} {A} {B})
  ++-cong (≈refl {stop}) b = b
  ++-cong (≈refl {yield value next}) b = ≈yield Eq.refl λ where .force → ++-cong ≈refl b
  ++-cong (≈refl {demand f}) b = ≈demand λ x → λ where .force → ++-cong ≈refl b
  ++-cong (≈refl {lazy next}) b = ≈lazyᵇ λ where .force → ++-cong ≈refl b
  ++-cong (≈thunks rel) b = ≈thunks λ where .force → ++-cong (rel .force) b
  ++-cong (≈yield value next) b = ≈yield value λ where .force → ++-cong (next .force) b
  ++-cong (≈demand onNext) b = ≈demand λ x → λ where .force → ++-cong (onNext x .force) b
  ++-cong (≈lazyˡ next) b = ≈lazyˡ λ where .force → ++-cong (next .force) b
  ++-cong (≈lazyʳ next) b = ≈lazyʳ λ where .force → ++-cong (next .force) b

  ++-assoc : ∀ {i} {α} {A B : Set α} → Associative (i ⊢_≈_) (_++_ {∞} {α} {A} {B})
  ++-assoc stop b c = ≈refl
  ++-assoc (yield value next) b c = ≈yield Eq.refl λ where .force → ++-assoc (next .force) b c
  ++-assoc (demand f) b c = ≈demand λ x → λ where .force → ++-assoc (f x .force) b c
  ++-assoc (lazy next) b c = ≈lazyᵇ λ where .force → ++-assoc (next .force) b c

  ++-identityˡ : ∀ {i} {α} {A B : Set α} → LeftIdentity (i ⊢_≈_) stop (_++_ {∞} {α} {A} {B})
  ++-identityˡ x = ≈refl

  ++-identityʳ : ∀ {i} {α} {A B : Set α} → RightIdentity (i ⊢_≈_) stop (_++_ {∞} {α} {A} {B})
  ++-identityʳ stop = ≈refl
  ++-identityʳ (yield value next) = ≈yield Eq.refl λ where .force → ++-identityʳ (next .force)
  ++-identityʳ (demand f) = ≈demand λ x → λ where .force → ++-identityʳ (f x .force)
  ++-identityʳ (lazy next) = ≈lazyᵇ λ where .force → ++-identityʳ (next .force)

  ++-identity : ∀ {i} {α} {A B : Set α} → Identity (i ⊢_≈_) stop (_++_ {∞} {α} {A} {B})
  ++-identity = ++-identityˡ , ++-identityʳ

  isMagma : ∀ {α} → (i : Size) (A B : Set α) → IsMagma (i ⊢_≈_) (_++_ {∞} {α} {A} {B})
  isMagma i A B =
    record
      { isEquivalence = isEquivalence i A B
      ; ∙-cong = ++-cong
      }

  isSemigroup : ∀ {α} → (i : Size) (A B : Set α) → IsSemigroup (i ⊢_≈_) _++_
  isSemigroup i A B =
    record
      { isMagma = isMagma i A B
      ; assoc = ++-assoc
      }

  isMonoid : ∀ {α} → (i : Size) (A B : Set α) → IsMonoid (i ⊢_≈_) _++_ stop
  isMonoid i A B =
    record
      { isSemigroup = isSemigroup i A B
      ; identity = ++-identity
      }

module Reasoning where
  open import Algebra.Definitions
  import Relation.Binary.PropositionalEquality as Eq

  open import Category
  open Relation.PropositionalEquality

  |>-identityˡ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
    → i ⊢ id |> pipe ≈ pipe
  |>-identityˡ stop = ≈refl
  |>-identityˡ (yield value next) = ≈yield Eq.refl λ where .force → |>-identityˡ (next .force)
  |>-identityˡ (demand onNext) = ≈demand λ x → λ where .force → ≈lazyˡ λ where .force → |>-identityˡ (onNext x .force)
  |>-identityˡ (lazy next) = ≈lazyᵇ λ where .force → |>-identityˡ (next .force)

  |>-identityʳ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
    → i ⊢ pipe |> id ≈ pipe
  |>-identityʳ stop = ≈refl
  |>-identityʳ (yield value next) = ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → |>-identityʳ (next .force)
  |>-identityʳ (demand onNext) = ≈demand λ x → λ where .force → |>-identityʳ (onNext x .force)
  |>-identityʳ (lazy next) = ≈lazyᵇ λ where .force → |>-identityʳ (next .force)

  <|-identityˡ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
    → i ⊢ id <| pipe ≈ pipe
  <|-identityˡ pipe = |>-identityʳ pipe

  <|-identityʳ : ∀ {i} {α} {A B : Set α} → (pipe : Pipe A B ∞)
    → i ⊢ pipe <| id ≈ pipe
  <|-identityʳ pipe = |>-identityˡ pipe

  |>-assoc : ∀ {i} {α} {A B C D : Set α} (f : Pipe A B ∞) (g : Pipe B C ∞) (h : Pipe C D ∞)
    → i ⊢ (f |> g) |> h ≈ f |> (g |> h)
  |>-assoc _ _ stop = ≈refl
  |>-assoc _ stop (demand _) = ≈refl
  |>-assoc stop (demand _) (demand _) = ≈refl
  |>-assoc f g (yield value next) = ≈yield Eq.refl λ where .force → |>-assoc f g (next .force)
  |>-assoc f (yield value next) (demand h) = ≈lazyᵇ λ where .force → |>-assoc f (next .force) (h value .force)
  |>-assoc (yield value next) (demand g) h@(demand _) = ≈lazyᵇ λ where .force → |>-assoc (next .force) (g value .force) h
  |>-assoc (demand f) g@(demand _) h@(demand _) = ≈demand λ x → λ where .force → |>-assoc (f x .force) g h
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
  blackHole = demand λ x → λ where .force → blackHole

  repeat : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
  repeat value = yield value λ where .force → repeat value

  map : ∀ {i} {α} {A B : Set α} → (A → B) → Pipe A B i
  private map′ : ∀ {i : Size} {α} {A B : Set α} → (A → B) → A → Pipe A B i
  map f = demand λ x → λ where .force → map′ f x
  map′ f x = yield (f x) (λ where .force → map f)

  filter : ∀ {i} {α} {A : Set α} → (A → Bool) → Pipe A A i
  private filter′ : ∀ {i} {α} {A : Set α} → (A → Bool) → A → Pipe A A i
  filter f = demand λ x → λ where .force → filter′ f x
  filter′ f x with f x
  ... | false = filter f
  ... | true = yield x (λ where .force → filter f)

  take : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  private take′ : ∀ {i} {α} {A : Set α} → (count : ℕ) → A → Pipe A A i
  take zero = stop
  take (suc n) = demand λ x → λ where .force → take′ n x
  take′ n x = yield x (λ where .force → take n)

  drop : ∀ {i} {α} {A : Set α} → (count : ℕ) → Pipe A A i
  drop zero = id
  drop (suc n) = demand λ _ → λ where .force → drop n

module Categorical where
  open import Category.Functor
  open import Function using (_∘_)
  import Relation.Binary.PropositionalEquality as Eq

  open Functional
  open Relation.PropositionalEquality

  functor : ∀ {i} {f} {A} → RawFunctor {f} (λ B → Pipe A B i)
  functor =
    record
      { _<$>_ = λ f → _|> map f
      }

  instance pipeFunctor = functor

  functor-id : ∀ {i} {α} {A B : Set α} (pipe : Pipe A B ∞)
    → let _<$>_ = functor .RawFunctor._<$>_ in
      i ⊢ Function.id <$> pipe ≈ pipe
  functor-id stop = ≈refl
  functor-id (yield value next) = ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → functor-id (next .force)
  functor-id (demand f) = ≈demand λ x → λ where .force → functor-id (f x .force)
  functor-id (lazy next) = ≈lazyᵇ λ where .force → functor-id (next .force)

  functor-compose : ∀ {i} {α} {A B C D : Set α} (pipe : Pipe A B ∞) (f : B → C) (g : C → D)
    → let _<$>_ = functor .RawFunctor._<$>_ in
      i ⊢ (g ∘ f) <$> pipe ≈ (g <$>_ ∘ f <$>_) pipe
  functor-compose stop f g = ≈refl
  functor-compose (yield value next) f g = ≈lazyᵇ λ where .force → ≈lazyʳ λ where .force → ≈yield Eq.refl λ where .force → functor-compose (next .force) f g
  functor-compose (demand d) f g = ≈demand λ x → λ where .force → functor-compose (d x .force) f g
  functor-compose (lazy next) f g = ≈lazyᵇ λ where .force → functor-compose (next .force) f g

module Examples where
  open import Codata.Colist.Bisimilarity renaming (_⊢_≈_ to _L⊢_≈_; refl to Lrefl)
  open Codata.Colist.Bisimilarity.≈-Reasoning
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
    begin
      process (suc size + suc size) id (Colist.fromList (Vec.toList (x ∷ xs)))
    ≈⟨ Lrefl ⟩
      process (size + suc size) (id′ x) (Colist.fromList (Vec.toList xs))
    ≈⟨ fromEq (Eq.cong (λ n → process n (id′ x) (Colist.fromList (Vec.toList xs))) (+-suc size size)) ⟩
      process (suc (size + size)) (id′ x) (Colist.fromList (Vec.toList xs))
    ≈⟨ Eq.refl ∷ (λ where .force → Lrefl) ⟩
      (x ∷ λ where .force → process (size + size) id (Colist.fromList (Vec.toList xs)))
    ≈⟨ Eq.refl ∷ (λ where .force → process-id size xs) ⟩
      Colist.fromList (Vec.toList (x ∷ xs))
    ∎

  _ : ∀ {i} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
  _ = helper
    where
    helper : ∀ {i : Size} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
    helper = ≈demand λ x → λ where .force → ≈lazyˡ λ where .force → ≈yield Eq.refl λ where .force → helper

  _ : ∀ {i} → i L⊢ process 100 (map (_+ 1)) (Colist.fromList (1 ∷ 2 ∷ 3 ∷ [])) ≈ Colist.fromList (2 ∷ 3 ∷ 4 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

  _ : ∀ {i} → i L⊢ process 100 (drop 5 |> filter (λ n → n % 2 ≡ᵇ 0) |> map (_* 2) |> take 3) nats ≈ Colist.fromList (12 ∷ 16 ∷ 20 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

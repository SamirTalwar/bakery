module StreamProcessors where

open import Codata.Colist as Colist using (Colist; []; _∷_)
open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Maybe as Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Data.Product as Product using (_×_; _,_; ∃-syntax)
open import Size

infixl 10 _|>_
infixl 10 _<|_

Stream : (A : Set) → Set
Stream A = Colist A ∞

data Pipe (A B : Set) (i : Size) : Set where
  stop : Pipe A B i
  yield : (value : B) → (next : Thunk (Pipe A B) i) → Pipe A B i
  demand : (f : A → Thunk (Pipe A B) i) → Pipe A B i
  lazy : Thunk (Pipe A B) i → Pipe A B i

Fuel : Set
Fuel = ℕ

process : {A B : Set} → Fuel → Pipe A B ∞ → Stream A → Stream B
process zero _ _ = []
process (suc _) stop xs = []
process (suc fuel) (yield value next) xs =
  value ∷ λ where .force → process fuel (next .force) xs
process (suc _) (demand f) [] = []
process (suc fuel) (demand f) (x ∷ xs) =
  process fuel (f x .force) (xs .force)
process (suc fuel) (lazy pipe) xs =
  process fuel (pipe .force) xs

id : ∀ {i : Size} {A : Set} → Pipe A A i
private id′ : ∀ {i : Size} {A : Set} → A → Pipe A A i
id = demand (λ x → λ where .force → id′ x)
id′ x = yield x λ where .force → id

_|>_ : ∀  {i : Size} → {A B C : Set} → Pipe A B i → Pipe B C i → Pipe A C i
_ |> stop = stop
up@stop |> yield value next = yield value (λ where .force → up |> next .force)
up@(yield _ _) |> yield value next = yield value (λ where .force → up |> next .force)
up@(demand _) |> yield value next = yield value (λ where .force → up |> next .force)
up@(lazy _) |> yield value next = yield value (λ where .force → up |> next .force)
stop |> demand f = stop
yield value next |> demand f = lazy λ where .force → next .force |> f value .force
demand f |> down@(demand _) = demand (λ x → λ where .force → f x .force |> down)
lazy up |> down@(demand _) = lazy λ where .force → up .force |> down
up@stop |> lazy down = lazy λ where .force → up |> down .force
up@(yield _ _) |> lazy down = lazy λ where .force → up |> down .force
demand f |> lazy down = demand (λ x → λ where .force → f x .force |> down .force)
lazy up |> lazy down = lazy λ where .force → up .force |> down .force

_<|_ : ∀ {i : Size} {A B C : Set} → Pipe B C i → Pipe A B i → Pipe A C i
down <| up = up |> down

module Relation where
  open import Level
  open import Relation.Binary
  open import Relation.Binary.Reasoning.Setoid
  import Relation.Binary.PropositionalEquality as Eq

  data Bisim {ρ : Level} {A B : Set} (R : REL B B ρ) (i : Size) : REL (Pipe A B ∞) (Pipe A B ∞) ρ where
    stop : Bisim R i stop stop
    yield : ∀ {value₁ value₂ next₁ next₂}
      → R value₁ value₂
      → Thunk.Thunk^R (Bisim R) i next₁ next₂
      → Bisim R i (yield value₁ next₁) (yield value₂ next₂)
    demand₁ : ∀ {f : A → Thunk (Pipe A B) ∞} {b : Pipe A B ∞}
      → (∀ x → Thunk.Thunk^R (Bisim R) i (f x) (λ where .force → b))
      → Bisim R i (demand f) b
    demand₂ : ∀ {a : Pipe A B ∞} {g : A → Thunk (Pipe A B) ∞}
      → (∀ x → Thunk.Thunk^R (Bisim R) i (λ where .force → a) (g x))
      → Bisim R i a (demand g)
    demand : ∀ {f : A → Thunk (Pipe A B) ∞} {g : A → Thunk (Pipe A B) ∞}
      → (∀ x → Thunk.Thunk^R (Bisim R) i (f x) (g x))
      → Bisim R i (demand f) (demand g)
    lazy₁ : ∀ {a b}
      → Thunk.Thunk^R (Bisim R) i a (λ where .force → b)
      → Bisim R i (lazy a) b
    lazy₂ : ∀ {a b}
      → Thunk.Thunk^R (Bisim R) i (λ where .force → a) b
      → Bisim R i a (lazy b)

  module _ {A B : Set} where
    infix 1 _⊢_≈_
    _⊢_≈_ : ∀ (i : Size) → Pipe A B ∞ → Pipe A B ∞ → Set
    _⊢_≈_ = Bisim Eq._≡_

module Functional where
  open import Data.Bool

  map : ∀ {i : Size} → {A B : Set} → (A → B) → Pipe A B i
  private map′ : ∀ {i : Size} → {A B : Set} → (A → B) → A → Pipe A B i
  map f = demand λ x → λ where .force → map′ f x
  map′ f x = yield (f x) (λ where .force → map f)

  filter : ∀ {i : Size} → {A : Set} → (A → Bool) → Pipe A A i
  private filter′ : ∀ {i : Size} → {A : Set} → (A → Bool) → A → Pipe A A i
  filter f = demand λ x → λ where .force → filter′ f x
  filter′ f x with f x
  ... | false = filter f
  ... | true = yield x (λ where .force → filter f)

  take : ∀ {i : Size} → {A : Set} → (count : ℕ) → Pipe A A i
  take′ : ∀ {i : Size} → {A : Set} → (count : ℕ) → A → Pipe A A i
  take zero = stop
  take (suc n) = demand λ x → λ where .force → take′ n x
  take′ n x = yield x (λ where .force → take n)

  drop : ∀ {i : Size} → {A : Set} → (count : ℕ) → Pipe A A i
  drop zero = id
  drop (suc n) = demand λ _ → λ where .force → drop n

module Examples where
  open import Codata.Colist.Bisimilarity renaming (_⊢_≈_ to _L⊢_≈_)
  open Codata.Colist.Bisimilarity.≈-Reasoning
  open import Data.List using (List; []; _∷_)
  open import Data.Nat.DivMod using (_%_)
  open import Data.Nat.Properties using (+-suc)
  open import Data.Vec as Vec using (Vec; []; _∷_)
  import Relation.Binary.PropositionalEquality as Eq

  open Functional
  open Relation

  nats : ∀ {i : Size} → Colist ℕ i
  natsFrom : ∀ {i : Size} → ℕ → Colist ℕ i
  nats = natsFrom 0
  natsFrom n = n ∷ λ where .force → natsFrom (suc n)

  process-id : ∀ {A : Set} {i : Size} (size : ℕ) (xs : Vec A size)
    → let xs-stream = Colist.fromList (Vec.toList xs)
      in i L⊢ process (size + size) id xs-stream ≈ xs-stream
  process-id 0 [] = []
  process-id (suc size) (x ∷ xs) =
    begin
      process (suc size + suc size) id (Colist.fromList (Vec.toList (x ∷ xs)))
    ≈⟨ refl ⟩
      process (size + suc size) (id′ x) (Colist.fromList (Vec.toList xs))
    ≈⟨ fromEq (Eq.cong (λ n → process n (id′ x) (Colist.fromList (Vec.toList xs))) (+-suc size size)) ⟩
      process (suc (size + size)) (id′ x) (Colist.fromList (Vec.toList xs))
    ≈⟨ Eq.refl ∷ (λ where .force → refl) ⟩
      (x ∷ λ where .force → process (size + size) id (Colist.fromList (Vec.toList xs)))
    ≈⟨ Eq.refl ∷ (λ where .force → process-id size xs) ⟩
      Colist.fromList (Vec.toList (x ∷ xs))
    ∎

  _ : ∀ {i : Size} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
  _ = helper
    where
    helper : ∀ {i : Size} → i ⊢ map suc |> map suc ≈ map (λ n → suc (suc n))
    helper = demand λ x → λ where .force → lazy₁ λ where .force → yield Eq.refl λ where .force → helper

  _ : ∀ {i : Size} → i L⊢ process 100 (map (_+ 1)) (Colist.fromList (1 ∷ 2 ∷ 3 ∷ [])) ≈ Colist.fromList (2 ∷ 3 ∷ 4 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

  _ : ∀ {i : Size} → i L⊢ process 100 (drop 5 |> filter (λ n → n % 2 ≡ᵇ 0) |> map (_* 2) |> take 3) nats ≈ Colist.fromList (12 ∷ 16 ∷ 20 ∷ [])
  _ = Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → Eq.refl ∷ λ where .force → []

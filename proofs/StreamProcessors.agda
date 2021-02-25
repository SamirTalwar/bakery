module StreamProcessors where

open import Codata.Colist as Colist using ([]; _∷_)
open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Maybe as Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Data.Product as Product using (_×_; _,_; ∃-syntax)
open import Size

Stream : (A : Set) → Set
Stream A = Colist.Colist A ∞

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

module Reasoning where
  open import Codata.Colist.Bisimilarity
  open Codata.Colist.Bisimilarity.≈-Reasoning
  open import Data.Nat.Properties using (+-suc)
  open import Data.Vec as Vec using (Vec; []; _∷_)
  import Relation.Binary.PropositionalEquality as Eq

  process-id : ∀ {A : Set} {i : Size} (size : ℕ) (xs : Vec A size)
    → let xs-stream = Colist.fromList (Vec.toList xs)
      in i ⊢ process (size + size) id xs-stream ≈ xs-stream
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

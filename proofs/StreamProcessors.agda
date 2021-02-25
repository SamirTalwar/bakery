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

id : ∀ {i : Size} {A : Set} → Pipe A A i
private id′ : ∀ {i : Size} {A : Set} → A → Pipe A A i
id = demand (λ x → λ where .force → id′ x)
id′ x = yield x λ where .force → id

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

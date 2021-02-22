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

module Examples where
  open import Data.List as List using (List) renaming ([] to L[]; _∷_ to _L∷_)
  open import Data.Nat.Properties
  open import Data.Vec as Vec using (Vec) renaming ([] to V[]; _∷_ to _V∷_)
  open import Relation.Binary.PropositionalEquality
  open Relation.Binary.PropositionalEquality.≡-Reasoning

  forceList : {A : Set} → Fuel → Stream A → Maybe (List A)
  forceList _ [] = just L[]
  forceList zero (_ ∷ _) = nothing
  forceList (suc fuel) (x ∷ xs♯) with forceList fuel (xs♯ .force)
  ... | nothing = nothing
  ... | just xs = just (x L∷ xs)

  forceVec : {A : Set} → (size : ℕ) → Stream A → Maybe (Vec A size)
  forceVec zero [] = just V[]
  forceVec zero (_ ∷ _) = nothing
  forceVec (suc size) [] = nothing
  forceVec (suc size) (x ∷ xs♯) = Maybe.map (x V∷_) (forceVec size (xs♯ .force))

  process-id : ∀ {A : Set} (size : ℕ) (xs : Vec A size)
    → forceVec size (process (size + size) id (Colist.fromList (Vec.toList xs))) ≡ just xs
  process-id zero V[] = refl
  process-id (suc size) (x V∷ xs) =
    begin
      forceVec (suc size) (process (suc size + suc size) id (Colist.fromList (Vec.toList (x V∷ xs))))
    ≡⟨⟩
      forceVec (suc size) (process (size + suc size) (id′ x) (Colist.fromList (Vec.toList xs)))
    ≡⟨ cong (λ s → forceVec (suc size) (process s (id′ x) (Colist.fromList (Vec.toList xs)))) (+-suc size size) ⟩
      forceVec (suc size) (process (suc (size + size)) (id′ x) (Colist.fromList (Vec.toList xs)))
    ≡⟨⟩
      forceVec (suc size) (x ∷ λ where .force → process (size + size) id (Colist.fromList (Vec.toList xs)))
    ≡⟨⟩
      Maybe.map (x V∷_) (forceVec size (process (size + size) id (Colist.fromList (Vec.toList xs))))
    ≡⟨ cong (Maybe.map (x V∷_)) (process-id size xs) ⟩
      just (x V∷ xs)
    ∎

module StreamProcessors.Functional where

open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Bool
open import Data.Nat

open import StreamProcessors.Core

blackHole : ∀ {i} {α} {A B : Set α} → Pipe A B i
blackHole = demand λ _ → λ where .force → blackHole

repeat : ∀ {i} {α} {A B : Set α} → B → Pipe A B i
repeat value = yield value λ where .force → repeat value

map : ∀ {i} {α} {A B : Set α} → (A → B) → Pipe A B i
map′ : ∀ {i} {α} {A B : Set α} → (A → B) → A → Pipe A B i
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

module Pipes where

open import Data.Maybe
open import Data.Product

data Producer (T : Set) (State : Set) : Set where
  producer : (State → Maybe T × State) → Producer T State

pull : ∀ {T : Set} {State : Set}
  → Producer T State
  → State
  → Maybe T × State
pull (producer apply) state = apply state

data Consumer (T : Set) (State : Set) : Set where
  consumer : (T → State → State) → Consumer T State

push : ∀ {T : Set} {State : Set}
  → T
  → Consumer T State
  → State
  → State
push item (consumer apply) state = apply item state

module examples where
  open import Data.List
  open import Data.Nat
  open import Function using (_∘_; id)
  import Relation.Binary.PropositionalEquality as Eq
  open Eq
  open Eq.≡-Reasoning
  import Lens
  open Lens using (Lens)

  counterProducer : {State : Set} → Lens State ℕ → Producer ℕ State
  counterProducer lens =
    producer (λ state → let value = Lens.get lens state in just value , Lens.put lens (suc value) state)

  record CounterState : Set where
    constructor counterState
    field
      counter : ℕ

  counterLens : Lens CounterState ℕ
  counterLens = record {
                    get = CounterState.counter
                  ; put = λ new state → record state { counter = new }
                  ; law-get-put = λ{ whole → refl }
                  ; law-put-get = λ{ whole part → refl }
                  ; law-put-put = λ{ whole part1 part2 → refl }
                }

  _ : let prod = counterProducer counterLens
          state₀ = counterState 0
          n₁ , state₁ = pull prod state₀
          n₂ , state₂ = pull prod state₁
          n₃ , state₃ = pull prod state₂
        in (n₁ ,′ n₂ ,′ n₃) ≡ (just 0 ,′ just 1 ,′ just 2)
  _ = refl

  listConsumer : ∀ {T : Set} → {State : Set}
    → Lens State (List T)
    → Consumer T State
  listConsumer lens = consumer (λ item state → Lens.put lens (item ∷ (Lens.get lens state)) state)

  _ : let con = listConsumer Lens.id
        in (push 3 con ∘ push 2 con ∘ push 1 con) []
      ≡ 3 ∷ 2 ∷ 1 ∷ []
  _ = refl

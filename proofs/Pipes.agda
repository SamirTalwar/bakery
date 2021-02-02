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

  record Counter (State : Set) : Set where
    field
      read : State → ℕ
      write : ℕ → State → State

  counterProducer : {State : Set} → Counter State → Producer ℕ State
  counterProducer record { read = read ; write = write } =
    producer (λ state → let value = read state in just value , write (suc value) state)

  record CounterState : Set where
    constructor counterState
    field
      counter : ℕ

  _ : let prod = counterProducer {CounterState} record {
                   read = CounterState.counter;
                   write = λ new state → record state { counter = new }
                 }
          state₀ = counterState 0
          n₁ , state₁ = pull prod state₀
          n₂ , state₂ = pull prod state₁
          n₃ , state₃ = pull prod state₂
        in (n₁ ,′ n₂ ,′ n₃) ≡ (just 0 ,′ just 1 ,′ just 2)
  _ = refl

  record Collector {T : Set} {State : Set} : Set where
    field
      read : State → List T
      write : List T → State → State

  collectorConsumer : ∀ {T : Set} → {State : Set}
    → Collector {T} {State}
    → Consumer T State
  collectorConsumer record { read = read ; write = write } =
    consumer (λ item state → write (item ∷ (read state)) state)

  _ : let con = collectorConsumer {ℕ} record { read = id ; write = λ new _ → new }
        in (push 3 con ∘ push 2 con ∘ push 1 con) []
      ≡ 3 ∷ 2 ∷ 1 ∷ []
  _ = refl

module Pipes where

open import Data.Maybe
open import Data.Nat
open import Data.Product
open import Data.Sum

data Producer (T State : Set) : Set₁ where
  producer : (State → Maybe T × State) → Producer T State

pull : ∀ {T State : Set}
  → Producer T State
  → State
  → Maybe T × State
pull (producer apply) state = apply state

data Consumer (T State : Set) : Set₁ where
  stop : Consumer T State
  consumer : (T → State → State) → Consumer T State

push : ∀ {T State : Set}
  → T
  → Consumer T State
  → State
  → State
push    _             stop state = state
push item (consumer apply) state = apply item state

data Pipe (State : Set) : Set₁ where
  _|>_ : {T : Set} → Producer T State → Consumer T State → Pipe State

data OutOfFuel : Set where
  outOfFuel : OutOfFuel

runPipe : {State : Set} → Pipe State → State → ℕ → OutOfFuel ⊎ State
runPipe pipe@(prod |>             stop) state          _ = inj₂ state
runPipe      (   _ |>       consumer _)     _       zero = inj₁ outOfFuel
runPipe pipe@(prod |> con@(consumer _)) state (suc fuel) with pull prod state
... |    nothing , newState = inj₂ newState
... | just value , newState = runPipe pipe (push value con newState) fuel

module Common where
  nullProducer : ∀ {T State} → Producer T State
  nullProducer = producer (λ state → nothing , state)

  nullConsumer : ∀ {T State} → Consumer T State
  nullConsumer = stop

  blackHoleConsumer : ∀ {T State} → Consumer T State
  blackHoleConsumer = consumer λ _ state → state

module examples where
  open import Data.List
  open import Function using (_∘_; id)
  import Relation.Binary.PropositionalEquality as Eq
  open Eq
  open Eq.≡-Reasoning
  import Lens
  open Lens using (Lens)
  open Common

  counterProducer : {State : Set} → Lens State ℕ → Producer ℕ State
  counterProducer lens =
    producer λ state → let value = Lens.get lens state in just value , Lens.put lens (suc value) state

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
        in (n₁ ,′ n₂ ,′ n₃ ,′ state₃) ≡ (just 0 ,′ just 1 ,′ just 2 ,′ counterState 3)
  _ = refl

  listProducer : ∀ {T State : Set}
    → Lens State (List T)
    → Producer T State
  listProducer lens = producer λ state →
      let output , list = listProducer′ (Lens.get lens state) in output , Lens.put lens list state
    where
    listProducer′ : ∀ {T : Set} → List T → Maybe T × List T
    listProducer′       [] = nothing , []
    listProducer′ (x ∷ xs) = just x , xs

  _ : let prod = listProducer Lens.id
          state₀ = 1 ∷ 2 ∷ 3 ∷ []
          n₁ , state₁ = pull prod state₀
          n₂ , state₂ = pull prod state₁
          n₃ , state₃ = pull prod state₂
        in (n₁ ,′ n₂ ,′ n₃ ,′ state₃) ≡ (just 1 ,′ just 2 ,′ just 3 ,′ [])
  _ = refl

  listConsumer : ∀ {T State : Set}
    → Lens State (List T)
    → Consumer T State
  listConsumer lens = consumer λ item state → Lens.put lens (item ∷ (Lens.get lens state)) state

  _ : let con = listConsumer Lens.id
          state₀ = []
          state₁ = push 1 con state₀
          state₂ = push 2 con state₁
          state₃ = push 3 con state₂
          in state₃ ≡ 3 ∷ 2 ∷ 1 ∷ []
  _ = refl

  record TwoLists (T : Set) : Set where
    field
      input : List T
      output : List T

  twoLists : ∀ {T : Set} → List T → TwoLists T
  twoLists input = record { input = input ; output = [] }

  twoListsInput : ∀ {T : Set} → Lens (TwoLists T) (List T)
  twoListsInput =
    record
      { get = λ state → TwoLists.input state
      ; put = λ new state → record state { input = new }
      ; law-get-put = λ whole → refl
      ; law-put-get = λ whole part → refl
      ; law-put-put = λ whole part1 part2 → refl
      }

  twoListsOutput : ∀ {T : Set} → Lens (TwoLists T) (List T)
  twoListsOutput =
    record
      { get = λ state → TwoLists.output state
      ; put = λ new state → record state { output = new }
      ; law-get-put = λ whole → refl
      ; law-put-get = λ whole part → refl
      ; law-put-put = λ whole part1 part2 → refl
      }

  _ : runPipe (listProducer twoListsInput |> listConsumer twoListsOutput) (record { input = 1 ∷ 2 ∷ 3 ∷ [] ; output = [] }) 4
      ≡ inj₂ (record { input = [] ; output = 3 ∷ 2 ∷ 1 ∷ [] })
  _ = refl

  reversesLists : ∀ {T : Set} (input : List T) (output : List T)
    → runPipe (listProducer twoListsInput |> listConsumer twoListsOutput) (record { input = input ; output = output }) (suc (length input))
      ≡ inj₂ (record { input = [] ; output = input ʳ++ output })
  reversesLists [] output = refl
  reversesLists input@(x ∷ xs) output = reversesLists xs (x ∷ output)

  counterRunsOutOfFuel : ∀ (input : ℕ) (fuel : ℕ)
    → runPipe (counterProducer Lens.id |> blackHoleConsumer) input fuel ≡ inj₁ outOfFuel
  counterRunsOutOfFuel _ zero = refl
  counterRunsOutOfFuel input (suc fuel) = counterRunsOutOfFuel (suc input) fuel

  consumerCanStopFlow : ∀ (input : ℕ) (fuel : ℕ)
    → runPipe (counterProducer Lens.id |> nullConsumer) input fuel ≡ inj₂ input
  consumerCanStopFlow input       zero = refl
  consumerCanStopFlow input (suc fuel) = consumerCanStopFlow input fuel

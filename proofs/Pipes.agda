module Pipes where

open import Data.Maybe
open import Data.Nat
open import Data.Product
open import Data.Sum
open import Function using (_$_)

infixr 10 _|>_

data Transformer (T : Set) : Set₁ where
  transformer : (T → T) → Transformer T

data Producer (T State : Set) : Set₁ where
  producer : (State → Maybe T × State) → Producer T State
  _|>_ : Producer T State → Transformer T → Producer T State

pull : ∀ {T State : Set}
  → Producer T State
  → State
  → Maybe T × State
pull (producer apply) state = apply state
pull (prod |> (transformer f)) state with pull prod state
... |    nothing , newState = nothing , newState
... | just value , newState = (just (f value)) , newState

data Consumer (T State : Set) : Set₁ where
  consumer : (T → State → Maybe State) → Consumer T State
  _|>_ : Transformer T → Consumer T State → Consumer T State

push : ∀ {T State : Set}
  → T
  → Consumer T State
  → State
  → Maybe State
push item (consumer apply) state = apply item state
push item (transformer f |> con) state = push (f item) con state

data Pipe (State : Set) : Set₁ where
  _|>_ : {T : Set} → Producer T State → Consumer T State → Pipe State

data OutOfFuel : Set where
  outOfFuel : OutOfFuel

runPipe : {State : Set} → Pipe State → State → ℕ → OutOfFuel ⊎ State
runPipe      (   _ |>    _)     _       zero = inj₁ outOfFuel
runPipe pipe@(prod |>  con) state (suc fuel) with pull prod state
... |    nothing , pulledState = inj₂ pulledState
... | just value , pulledState with push value con pulledState
...     | nothing = inj₂ pulledState
...     | just newState = runPipe pipe newState fuel

module Common where
  nullProducer : ∀ {T State} → Producer T State
  nullProducer = producer λ state → nothing , state

  nullConsumer : ∀ {T State} → Consumer T State
  nullConsumer = consumer λ _ _ → nothing

  blackHoleConsumer : ∀ {T State} → Consumer T State
  blackHoleConsumer = consumer λ _ state → just state

module examples where
  open import Data.Empty
  open import Data.List
  open import Function using (_∘_; id)
  import Relation.Binary.PropositionalEquality as Eq
  open Eq
  open Eq.≡-Reasoning
  import Lens
  open Lens using (Lens)
  open Common

  repeatProducer : {T State : Set} → (value : T) → Producer T State
  repeatProducer value = producer λ state → just value , state

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
  listConsumer lens = consumer λ item state → just $ Lens.put lens (item ∷ (Lens.get lens state)) state

  _ : let con = listConsumer Lens.id
          state = do
            state₀ ← just []
            state₁ ← push 1 con state₀
            state₂ ← push 2 con state₁
            push 3 con state₂
          in state ≡ just (3 ∷ 2 ∷ 1 ∷ [])
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
  counterRunsOutOfFuel input (suc fuel) =
    counterRunsOutOfFuel (suc input) fuel

  steadyStateProducerRunsOutOfFuel : ∀ {T State : Set} (prod : Producer T State) (input : State) (fuel : ℕ)
    → ∃[ value ] (pull prod input ≡ (just value , input))
    → runPipe (prod |> blackHoleConsumer) input fuel ≡ inj₁ outOfFuel
  steadyStateProducerRunsOutOfFuel _ _ zero _ = refl
  steadyStateProducerRunsOutOfFuel prod input (suc fuel) (value , isInfinite) rewrite isInfinite =
    steadyStateProducerRunsOutOfFuel prod input fuel (value , isInfinite)

  consumerCanStopFlow : ∀ {T State : Set} (value : T) (input : State)
    → ∃[ fuel ] (runPipe (repeatProducer value |> nullConsumer) input fuel ≡ inj₂ input)
  consumerCanStopFlow prod input = 1 , refl

  _ : runPipe (listProducer twoListsInput |> transformer (_* 2) |> listConsumer twoListsOutput) (record { input = 1 ∷ 2 ∷ 3 ∷ [] ; output = [] }) 4
      ≡ inj₂ (record { input = [] ; output = 6 ∷ 4 ∷ 2 ∷ [] })
  _ = refl

  postulate
    Producer-≡ : ∀ {T State : Set} (prod₁ : Producer T State) (prod₂ : Producer T State)
      → (∀ (state : State) → pull prod₁ state ≡ pull prod₂ state)
      → prod₁ ≡ prod₂

    Consumer-≡ : ∀ {T State : Set} (con₁ : Consumer T State) (con₂ : Consumer T State) (item : T)
      → (∀ (state : State) → push item con₁ state ≡ push item con₂ state)
      → con₁ ≡ con₂

    Pipe-≡ : ∀ {State : Set} (pipe₁ : Pipe State) (pipe₂ : Pipe State)
      → (∀ (state : State) (fuel : ℕ) → runPipe pipe₁ state fuel ≡ runPipe pipe₂ state fuel)
      → pipe₁ ≡ pipe₂

  associative : ∀ {T State : Set} (prod : Producer T State) (trans : Transformer T) (con : Consumer T State)
    → prod |> (trans |> con) ≡ (prod |> trans) |> con
  associative prod trans con = Pipe-≡ (prod |> (trans |> con)) ((prod |> trans) |> con) (associative′ prod trans con)
    where
    associative′ : ∀ {T State : Set} (prod : Producer T State) (trans : Transformer T) (con : Consumer T State) (state : State) (fuel : ℕ)
      → runPipe (prod |> (trans |> con)) state fuel ≡ runPipe ((prod |> trans) |> con) state fuel
    associative′ _ _ _ _ zero = refl
    associative′ (producer p) (transformer t) (consumer c) state (suc fuel) with pull (producer p) state
    ... | nothing , pulledState = refl
    ... | just value , pulledState with push value (transformer t |> consumer c) pulledState
    ...     | nothing = refl
    ...     | just newState = associative′ (producer p) (transformer t) (consumer c) newState fuel
    associative′ (producer p) (transformer t) (c₁ |> c₂) state (suc fuel) with pull (producer p) state
    ... | nothing , pulledState = refl
    ... | just value , pulledState with push value (transformer t |> (c₁ |> c₂)) pulledState
    ... |     nothing = refl
    ... |     just newState = associative′ (producer p) (transformer t) (c₁ |> c₂) newState fuel
    associative′   (p₁ |> p₂) (transformer t) (consumer c) state (suc fuel) with pull (p₁ |> p₂) state
    ... | nothing , pulledState = refl
    ... | just value , pulledState with push value (transformer t |> consumer c) pulledState
    ... |     nothing = refl
    ... |     just newState = associative′ (p₁ |> p₂) (transformer t) (consumer c) newState fuel
    associative′   (p₁ |> p₂) (transformer t) (c₁ |> c₂) state (suc fuel) with pull (p₁ |> p₂) state
    ... | nothing , pulledState = refl
    ... | just value , pulledState with push value (transformer t |> (c₁ |> c₂)) pulledState
    ... |     nothing = refl
    ... |     just newState = associative′ (p₁ |> p₂) (transformer t) (c₁ |> c₂) newState fuel

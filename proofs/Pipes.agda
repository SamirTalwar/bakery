module Pipes where

open import Data.Empty
open import Data.Maybe
open import Data.Nat
open import Data.Product
open import Data.Sum
open import Data.Unit
open import Function using (_$_)

infixr 10 _|>_

data Progress : Set where
  stop : Progress
  continue : Progress

record Result (T State : Set) : Set where
  constructor result
  field
    progress : Progress
    value : Maybe T
    state : State

Iterate : (I O State : Set) → Set
Iterate I O State = I → State → Result O State

record Pipe (I O State : Set) : Set₁ where
  field
    iterate : Iterate I O State

Producer : Set → Set → Set₁
Producer = Pipe ⊤

producer : {O State : Set} → (State → Maybe O × State) → Producer O State
producer apply = record { iterate = iterate apply }
  where
  iterate : {O State : Set} → (State → Maybe O × State) → Iterate ⊤ O State
  iterate apply tt state with apply state
  ... |    nothing , newState = result stop nothing newState
  ... | just value , newState = result continue (just value) newState

Consumer : Set → Set → Set₁
Consumer I = Pipe I ⊥

consumer : {I State : Set} → (I → State → Progress × State) → Consumer I State
consumer apply = record { iterate = iterate apply }
  where
  iterate : {I State : Set} → (I → State → Progress × State) → Iterate I ⊥ State
  iterate apply input state with apply input state
  ... | progress , newState = result progress nothing newState

pull : ∀ {T State : Set}
  → Producer T State
  → State
  → Result T State
pull (record { iterate = iterate }) state = iterate tt state

push : ∀ {T State : Set}
  → T
  → Consumer T State
  → State
  → Progress × State
push item (record { iterate = iterate }) state
  with iterate item state
... | result progress nothing newState = progress , newState

Transformer : ∀ {State : Set} → (A B : Set) → Set₁
Transformer {State} A B = Pipe A B State

transformer : ∀ {State : Set} → {A B : Set} → (A → B) → Pipe A B State
transformer f = record { iterate = λ input state → result continue (just (f input)) state }

id : ∀ {State : Set} → {T : Set} → Pipe T T State
id = transformer Function.id

_|>_ : {I T O State : Set} → Pipe I T State → Pipe T O State → Pipe I O State
up |> down = record { iterate = iterate up down }
  where
  iterate : {I T O State : Set} → Pipe I T State → Pipe T O State → I → State → Result O State
  iterate (record {iterate = iterateUp}) (record {iterate = iterateDown}) input state with iterateUp input state
  ... | result     stop       output pulledState = result stop nothing pulledState
  ... | result continue      nothing pulledState = result stop nothing pulledState
  ... | result continue (just value) pulledState = iterateDown value pulledState

_<|_ : {I T O State : Set} → Pipe T O State → Pipe I T State → Pipe I O State
down <| up = up |> down

Pipeline : Set → Set₁
Pipeline = Pipe ⊤ ⊥

data OutOfFuel : Set where
  outOfFuel : OutOfFuel

runPipeline : {State : Set} → Pipeline State → State → ℕ → OutOfFuel ⊎ State
runPipeline _ _ zero = inj₁ outOfFuel
runPipeline pipeline@(record { iterate = iterate }) state (suc fuel) with iterate tt state
... | result     stop nothing newState = inj₂ newState
... | result continue nothing newState = runPipeline pipeline newState fuel

module Reasoning where
  import Relation.Binary.PropositionalEquality as Eq
  open Eq
  open Eq.≡-Reasoning

  open import Category

  postulate
    Pipe-≡ : ∀ {I O State : Set} (pipe₁ : Pipe I O State) (pipe₂ : Pipe I O State)
      → (∀ (input : I) (state : State) → Pipe.iterate pipe₁ input state ≡ Pipe.iterate pipe₂ input state)
      → pipe₁ ≡ pipe₂

  |>-associative : ∀ {A B C D State : Set} (a : Pipe A B State) (b : Pipe B C State) (c : Pipe C D State)
    → (a |> b) |> c ≡ a |> (b |> c)
  |>-associative a b c = Pipe-≡ ((a |> b) |> c) (a |> (b |> c)) (associative′ a b c)
    where
    associative′ : ∀ {A B C D State : Set} (a : Pipe A B State) (b : Pipe B C State) (c : Pipe C D State) (input : A) (state : State)
      → Pipe.iterate ((a |> b) |> c) input state ≡ Pipe.iterate (a |> (b |> c)) input state
    associative′ record { iterate = iterateA } record { iterate = iterateB } record { iterate = iterateC } input state
      with iterateA input state
    ... | result stop _ _ = refl
    ... | result continue nothing _ = refl
    ... | result continue (just value₁) state₁ with iterateB value₁ state₁
    ...     | result stop _ _ = refl
    ...     | result continue nothing _ = refl
    ...     | result continue (just _) _ = refl

  PipeForState : (State I O : Set) → Set₁
  PipeForState State I O = Pipe I O State

  pipes-are-categories : ∀ {State : Set} → Category Set (PipeForState State)
  pipes-are-categories = record {
    _∘_ = λ g f → f |> g ;
    id = id ;
    assoc = λ h g f → |>-associative f g h
    }

module Common where
  nullProducer : ∀ {T State} → Producer T State
  nullProducer = producer λ state → nothing , state

  nullConsumer : ∀ {T State} → Consumer T State
  nullConsumer = consumer λ _ state → stop , state

  blackHoleConsumer : ∀ {T State} → Consumer T State
  blackHoleConsumer = consumer λ _ state → continue , state

module examples where
  open import Data.Empty
  open import Data.List
  open import Function using (_∘_; id)
  import Relation.Binary.PropositionalEquality as Eq
  open Eq
  open Eq.≡-Reasoning

  open import Lens using (Lens)
  open Common

  repeatProducer : {T State : Set} → (value : T) → Producer T State
  repeatProducer value = producer λ state → just value , state

  counterProducer : {State : Set} → Lens State ℕ → Producer ℕ State
  counterProducer lens = producer λ state → let value = Lens.get lens state in just value , Lens.put lens (suc value) state

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
          result p₁ n₁ state₁ = pull prod state₀
          result p₂ n₂ state₂ = pull prod state₁
          result p₃ n₃ state₃ = pull prod state₂
          ns = n₁ ∷ n₂ ∷ n₃ ∷ []
        in (ns ,′ state₃) ≡ ((just 0 ∷ just 1 ∷ just 2 ∷ []) ,′ counterState 3)
  _ = refl

  listProducer : ∀ {T State : Set}
    → Lens State (List T)
    → Producer T State
  listProducer lens =
    producer λ state → let output , list = listProducer′ (Lens.get lens state) in output , (Lens.put lens list state)
    where
    listProducer′ : ∀ {T : Set} → List T → Maybe T × List T
    listProducer′       [] = nothing , []
    listProducer′ (x ∷ xs) = just x , xs

  _ : let prod = listProducer Lens.id
          state₀ = 1 ∷ 2 ∷ 3 ∷ []
          result p₁ n₁ state₁ = pull prod state₀
          result p₂ n₂ state₂ = pull prod state₁
          result p₃ n₃ state₃ = pull prod state₂
          ns = n₁ ∷ n₂ ∷ n₃ ∷ []
        in (ns ,′ state₃) ≡ ((just 1 ∷ just 2 ∷ just 3 ∷ []) ,′ [])
  _ = refl

  listConsumer : ∀ {T State : Set}
    → Lens State (List T)
    → Consumer T State
  listConsumer lens = consumer λ item state → continue , (Lens.put lens (item ∷ (Lens.get lens state)) state)

  _ : let con = listConsumer Lens.id
          state₀ = []
          p₁ , state₁ = push 1 con state₀
          p₂ , state₂ = push 2 con state₁
          p₃ , state₃ = push 3 con state₂
          in (p₃ ,′ state₃) ≡ (continue , (3 ∷ 2 ∷ 1 ∷ []))
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

  _ : runPipeline (listProducer twoListsInput |> listConsumer twoListsOutput) (record { input = 1 ∷ 2 ∷ 3 ∷ [] ; output = [] }) 4
      ≡ inj₂ (record { input = [] ; output = 3 ∷ 2 ∷ 1 ∷ [] })
  _ = refl

  reversesLists : ∀ {T : Set} (input : List T) (output : List T)
    → runPipeline (listProducer twoListsInput |> listConsumer twoListsOutput) (record { input = input ; output = output }) (suc (length input))
      ≡ inj₂ (record { input = [] ; output = input ʳ++ output })
  reversesLists [] output = refl
  reversesLists input@(x ∷ xs) output = reversesLists xs (x ∷ output)

  counterRunsOutOfFuel : ∀ (input : ℕ) (fuel : ℕ)
    → runPipeline (counterProducer Lens.id |> blackHoleConsumer) input fuel ≡ inj₁ outOfFuel
  counterRunsOutOfFuel _ zero = refl
  counterRunsOutOfFuel input (suc fuel) =
    counterRunsOutOfFuel (suc input) fuel

  steadyStateProducerRunsOutOfFuel : ∀ {T State : Set} (prod : Producer T State) (input : State) (fuel : ℕ)
    → ∃[ value ] (pull prod input ≡ result continue (just value) input)
    → runPipeline (prod |> blackHoleConsumer) input fuel ≡ inj₁ outOfFuel
  steadyStateProducerRunsOutOfFuel _ _ zero _ = refl
  steadyStateProducerRunsOutOfFuel prod input (suc fuel) (value , isInfinite) rewrite isInfinite =
    steadyStateProducerRunsOutOfFuel prod input fuel (value , isInfinite)

  consumerCanStopFlow : ∀ {T State : Set} (value : T) (input : State)
    → ∃[ fuel ] (runPipeline (repeatProducer value |> nullConsumer) input fuel ≡ inj₂ input)
  consumerCanStopFlow prod input = 1 , refl

  _ : runPipeline (listProducer twoListsInput |> transformer (_* 2) |> listConsumer twoListsOutput) (record { input = 1 ∷ 2 ∷ 3 ∷ [] ; output = [] }) 4
      ≡ inj₂ (record { input = [] ; output = 6 ∷ 4 ∷ 2 ∷ [] })
  _ = refl

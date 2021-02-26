module Pipes where

open import Data.Empty
open import Data.Nat
open import Data.Unit

infixr 10 _|>_

data Result T State : Set where
  stop : State → Result T State
  demand : State → Result T State
  yield : T → State → Result T State

Next : (I O State : Set) → Set
Next I O State = I → State → Result O State

data Pipe I O State : Set₁ where
  mkPipe : Next I O State → Pipe I O State

next : {I O State : Set} → Pipe I O State → Next I O State
next (mkPipe next) = next

Producer : Set → Set → Set₁
Producer = Pipe ⊤

Consumer : Set → Set → Set₁
Consumer I = Pipe I ⊥

id : ∀ {State : Set} → {T : Set} → Pipe T T State
id = mkPipe yield

_|>_ : {I T O State : Set} → Pipe I T State → Pipe T O State → Pipe I O State
up |> down = mkPipe (next′ up down)
  where
  next′ : {I T O State : Set} → Pipe I T State → Pipe T O State → Next I O State
  next′ up down input state with next up input state
  ... | stop pulledState = stop pulledState
  ... | demand pulledState = demand pulledState
  ... | yield value pulledState = next down value pulledState

_<|_ : {I T O State : Set} → Pipe T O State → Pipe I T State → Pipe I O State
down <| up = up |> down

Pipeline : Set → Set₁
Pipeline = Pipe ⊤ ⊥

data PipelineResult State : Set where
  outOfFuel : PipelineResult State
  finalState : State → PipelineResult State

runPipeline : {State : Set} → Pipeline State → State → ℕ → PipelineResult State
runPipeline _ _ zero = outOfFuel
runPipeline pipeline state (suc fuel) with next pipeline tt state
... | stop newState = finalState newState
... | demand newState = runPipeline pipeline newState fuel

module Reasoning where
  open import Relation.Binary.PropositionalEquality

  open import Category

  postulate
    Pipe-≡ : ∀ {I O State : Set} (pipe₁ : Pipe I O State) (pipe₂ : Pipe I O State)
      → (∀ (input : I) (state : State) → next pipe₁ input state ≡ next pipe₂ input state)
      → pipe₁ ≡ pipe₂

  pipe|>id≡pipe : ∀ {I O State : Set} (pipe : Pipe I O State)
    → pipe |> id ≡ pipe
  pipe|>id≡pipe pipe = Pipe-≡ (pipe |> id) pipe (pipe|>id≡pipe′ pipe)
    where
    pipe|>id≡pipe′ : ∀ {I O State : Set} (pipe : Pipe I O State) (input : I) (state : State)
      → next (pipe |> id) input state ≡ next pipe input state
    pipe|>id≡pipe′ pipe input state with next pipe input state
    ... | stop _ = refl
    ... | demand _ = refl
    ... | yield _ _ = refl

  id|>pipe≡pipe : ∀ {I O State : Set} (pipe : Pipe I O State)
    → id |> pipe ≡ pipe
  id|>pipe≡pipe pipe = Pipe-≡ (id |> pipe) pipe (id|>pipe≡pipe′ pipe)
    where
    id|>pipe≡pipe′ : ∀ {I O State : Set} (pipe : Pipe I O State) (input : I) (state : State)
      → next (id |> pipe) input state ≡ next pipe input state
    id|>pipe≡pipe′ pipe input state with next pipe input state
    ... | stop _ = refl
    ... | demand _ = refl
    ... | yield _ _ = refl

  |>-associative : ∀ {A B C D State : Set} (a : Pipe A B State) (b : Pipe B C State) (c : Pipe C D State)
    → (a |> b) |> c ≡ a |> (b |> c)
  |>-associative a b c = Pipe-≡ ((a |> b) |> c) (a |> (b |> c)) (associative′ a b c)
    where
    associative′ : ∀ {A B C D State : Set} (a : Pipe A B State) (b : Pipe B C State) (c : Pipe C D State) (input : A) (state : State)
      → next ((a |> b) |> c) input state ≡ next (a |> (b |> c)) input state
    associative′ a b c input state
      with next a input state
    ... | stop _ = refl
    ... | demand _ = refl
    ... | yield _ _ = refl

  pipes-are-categories : ∀ {State : Set} → Category Set (λ I O → Pipe I O State) (_≡_)
  pipes-are-categories = record {
    _∘_ = λ g f → f |> g ;
    id = id ;
    law-idˡ = λ f → pipe|>id≡pipe f ;
    law-idʳ = λ f → id|>pipe≡pipe f ;
    law-assoc = λ h g f → |>-associative f g h
    }

module Functional where
  null : ∀ {I O State} → Pipe I O State
  null = mkPipe λ _ state → stop state

  blackHole : ∀ {T State} → Consumer T State
  blackHole = mkPipe λ _ state → demand state

  map : ∀ {State : Set} → {A B : Set} → (A → B) → Pipe A B State
  map f = mkPipe λ input state → yield (f input) state

  repeatProducer : {T State : Set} → (value : T) → Producer T State
  repeatProducer value = mkPipe λ _ state → yield value state

module Examples where
  open import Data.List using (List; []; _∷_; length; _ʳ++_)
  open import Data.Maybe using (Maybe; just; nothing)
  open import Data.Product using (_×_; _,_; _,′_; ∃-syntax)
  open import Relation.Binary.PropositionalEquality
  open Relation.Binary.PropositionalEquality.≡-Reasoning

  open import Lens using (Lens)
  open Functional
  open Reasoning

  definitelyPull : ∀ {T State : Set}
    → Producer T State
    → State
    → Maybe T × State
  definitelyPull prod state with next prod tt state
  ... | stop newState = nothing , newState
  ... | demand newState = nothing , newState
  ... | yield value newState = just value , newState

  definitelyPush : ∀ {T State : Set}
    → T
    → Consumer T State
    → State
    → State
  definitelyPush input con state with next con input state
  ... | stop newState = newState
  ... | demand newState = newState

  counterProducer : {State : Set} → Lens State ℕ → Producer ℕ State
  counterProducer lens = mkPipe λ _ state → let value , newState = Lens.get-and-modify lens suc state in yield value newState

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
          n₁ , state₁ = definitelyPull prod state₀
          n₂ , state₂ = definitelyPull prod state₁
          n₃ , state₃ = definitelyPull prod state₂
          ns = n₁ ∷ n₂ ∷ n₃ ∷ []
        in (ns ,′ state₃) ≡ ((just 0 ∷ just 1 ∷ just 2 ∷ []) ,′ counterState 3)
  _ = refl

  listProducer : ∀ {T State : Set}
    → Lens State (List T)
    → Producer T State
  listProducer lens = mkPipe (next′ lens)
    where
    next′ : ∀ {T State : Set} → Lens State (List T) → Next ⊤ T State
    next′ lens tt state with Lens.get lens state
    ... | [] = stop state
    ... | x ∷ xs = yield x (Lens.put lens xs state)

  _ : let prod = listProducer Lens.id
          state₀ = 1 ∷ 2 ∷ 3 ∷ []
          n₁ , state₁ = definitelyPull prod state₀
          n₂ , state₂ = definitelyPull prod state₁
          n₃ , state₃ = definitelyPull prod state₂
          ns = n₁ ∷ n₂ ∷ n₃ ∷ []
        in (ns ,′ state₃) ≡ ((just 1 ∷ just 2 ∷ just 3 ∷ []) ,′ [])
  _ = refl

  listConsumer : ∀ {T State : Set}
    → Lens State (List T)
    → Consumer T State
  listConsumer lens = mkPipe λ item state → demand (Lens.modify lens (item ∷_) state)

  _ : let con = listConsumer Lens.id
          state₀ = []
          state₁ = definitelyPush 1 con state₀
          state₂ = definitelyPush 2 con state₁
          state₃ = definitelyPush 3 con state₂
          in state₃ ≡ (3 ∷ 2 ∷ 1 ∷ [])
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
      ≡ finalState (record { input = [] ; output = 3 ∷ 2 ∷ 1 ∷ [] })
  _ = refl

  reversesLists : ∀ {T : Set} (input : List T) (output : List T)
    → runPipeline (listProducer twoListsInput |> listConsumer twoListsOutput) (record { input = input ; output = output }) (suc (length input))
      ≡ finalState (record { input = [] ; output = input ʳ++ output })
  reversesLists [] output = refl
  reversesLists input@(x ∷ xs) output = reversesLists xs (x ∷ output)

  counterRunsOutOfFuel : ∀ (input : ℕ) (fuel : ℕ)
    → runPipeline (counterProducer Lens.id |> blackHole) input fuel ≡ outOfFuel
  counterRunsOutOfFuel _ zero = refl
  counterRunsOutOfFuel input (suc fuel) =
    counterRunsOutOfFuel (suc input) fuel

  steadyStateProducerRunsOutOfFuel : ∀ {T State : Set} (prod : Producer T State) (input : State) (fuel : ℕ)
    → ∃[ value ] (next prod tt input ≡ yield value input)
    → runPipeline (prod |> blackHole) input fuel ≡ outOfFuel
  steadyStateProducerRunsOutOfFuel _ _ zero _ = refl
  steadyStateProducerRunsOutOfFuel prod input (suc fuel) (value , isInfinite) rewrite isInfinite =
    steadyStateProducerRunsOutOfFuel prod input fuel (value , isInfinite)

  consumerCanStopFlow : ∀ {T State : Set} (value : T) (input : State)
    → ∃[ fuel ] (runPipeline (repeatProducer value |> null) input fuel ≡ finalState input)
  consumerCanStopFlow prod input = 1 , refl

  _ : runPipeline (listProducer twoListsInput |> map (_* 2) |> listConsumer twoListsOutput) (record { input = 1 ∷ 2 ∷ 3 ∷ [] ; output = [] }) 4
      ≡ finalState (record { input = [] ; output = 6 ∷ 4 ∷ 2 ∷ [] })
  _ = refl

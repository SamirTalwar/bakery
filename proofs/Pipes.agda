module Pipes where

open import Codata.Thunk
open import Data.Maybe using ()
open import Data.Product
open import Function using (_∘_; id)
import Relation.Binary.PropositionalEquality as Eq
open Eq
open Eq.≡-Reasoning
open import Relation.Nullary

infixr 10 !_
infixr 10 _%:_

data Producer (T : Set) : Set

record NonEmptyProducer (T : Set) : Set where
  constructor _%:_
  coinductive
  field
    head : T
    tail : Producer T

data Producer T where
  [] : Producer T
  !_ : NonEmptyProducer T → Producer T

data Consumer (T : Set) (State : Set) : Set where
  consumer : (T → State → State) → Consumer T State

push : {T : Set} {State : Set}
  → T
  → Consumer T State
  → State
  → State
push item (consumer apply) state = apply item state

module examples where
  open import Data.List
  open import Data.Nat

  data Naturals : Set where
    naturalsUpTo : ℕ → Naturals

  naturalsProducer : Naturals → Producer ℕ
  naturalsProducer (naturalsUpTo n) = naturalsProducer′ n n
    where
    naturalsProducer′ : ℕ → ℕ → Producer ℕ
    naturalsProducer′    zero n = []
    naturalsProducer′ (suc m) n = ! (n ∸ m) %: naturalsProducer′ m n

  _ : naturalsProducer (naturalsUpTo 5) ≡ ! 1 %: ! 2 %: ! 3 %: ! 4 %: ! 5 %: []
  _ = refl

  record Collector {T : Set} {S : Set} : Set where
    field
      read : S → List T
      write : List T → S → S

  collectorConsumer : ∀ {T : Set} → {S : Set}
    → Collector {T} {S}
    → Consumer T S
  collectorConsumer record { read = read ; write = write } =
    consumer (λ item state → write (item ∷ (read state)) state)

  _ : let snk = collectorConsumer {ℕ} record { read = id ; write = λ new _ → new }
        in (push 3 snk ∘ push 2 snk ∘ push 1 snk) []
      ≡ 3 ∷ 2 ∷ 1 ∷ []
  _ = refl

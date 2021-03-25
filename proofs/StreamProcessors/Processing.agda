module StreamProcessors.Processing where

open import Category.Monad
open import Category.Monad.State
open import Codata.Thunk as Thunk using (force)
open import Data.Empty.Polymorphic
open import Data.List
open import Data.Nat
open import Data.Product
open import Data.Unit.Polymorphic
open import Function using (_∘_)
open import Size using (∞)

open import Monadic
open import StreamProcessors.Core
open import StreamProcessors.Monadic

Fuel : Set
Fuel = ℕ

process : ∀ {α} {A : Set α}
  → Fuel
  → Pipe ⊥ A ∞
  → List A
process zero _ = []
process (suc _) stop = []
process (suc fuel) (yield value next) = value ∷ process fuel (next .force)
process (suc _) (demand onNext) = []
process (suc fuel) (lazy next) = process fuel (next .force)

processM : ∀ {α} {A : Set α} {M : Set α → Set α} {{Monad : RawMonad M}}
  → Fuel
  → Pipe ⊥ (M A) ∞
  → M ⊤
processM zero pipe = return tt
processM (suc fuel) stop = return tt
processM (suc fuel) (yield value next) = value >> processM fuel (next .force)
processM (suc fuel) (demand onNext) = return tt
processM (suc fuel) (lazy next) = processM fuel (next .force)

processMToList : ∀ {α} {A : Set α}
  → Fuel
  → Pipe ⊥ A ∞
  → List A
processMToList {A = A} fuel pipe =
  reverse (proj₂ (processM fuel ((modify ∘ (_∷_)) <$> pipe) []))
    where
    open RawMonadState (StateMonadState (List A)) using (modify)
    instance stateMonad = StateMonad (List A)

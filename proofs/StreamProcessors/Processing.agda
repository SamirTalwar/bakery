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
open import Level using (Level)
open import Size

open import Monadic
open import StreamProcessors.Core
open import StreamProcessors.Monadic

private
  variable
    i : Size
    α : Level
    A : Set α
    M : Set α → Set α

Fuel : Set
Fuel = ℕ

process : Fuel → Pipe ⊥ A ∞ → List A
process zero _ = []
process (suc _) stop = []
process (suc fuel) (yield value next) = value ∷ process fuel (next .force)
process (suc _) (demand onNext) = []
process (suc fuel) (lazy next) = process fuel (next .force)

processM : ∀ {{Monad : RawMonad M}}
  → Fuel
  → Pipe ⊥ (M A) ∞
  → M ⊤
processM zero pipe = return tt
processM (suc fuel) stop = return tt
processM (suc fuel) (yield value next) = value >> processM fuel (next .force)
processM (suc fuel) (demand onNext) = return tt
processM (suc fuel) (lazy next) = processM fuel (next .force)

processMToList : Fuel → Pipe ⊥ A ∞ → List A
processMToList {A = A} fuel pipe =
  reverse (proj₂ (processM fuel ((modify ∘ (_∷_)) <$> pipe) []))
    where
    open RawMonadState (StateMonadState (List A)) using (modify)
    instance stateMonad = StateMonad (List A)

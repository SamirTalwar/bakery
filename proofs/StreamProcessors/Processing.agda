module StreamProcessors.Processing where

open import Codata.Thunk as Thunk using (force)
open import Size using (∞)
open import Data.Empty.Polymorphic
open import Data.List
open import Data.Nat

open import StreamProcessors.Core

Fuel : Set
Fuel = ℕ

process : ∀ {α} {A : Set α} → Fuel → Pipe ⊥ A ∞ → List A
process zero _ = []
process (suc _) stop = []
process (suc fuel) (yield value next) = value ∷ process fuel (next .force)
process (suc _) (demand onNext) = []
process (suc fuel) (lazy next) = process fuel (next .force)

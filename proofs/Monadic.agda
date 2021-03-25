module Monadic where

open import Category.Applicative
open import Category.Functor
open import Category.Monad

module _ {ℓ} {M : Set ℓ → Set ℓ} {{Monad : RawMonad M}} where
  open RawMonad Monad public

module Lens where

import Relation.Binary.PropositionalEquality as Eq
open Eq
open Eq.≡-Reasoning

record Lens A B : Set where
  field
    get : A → B
    put : B → A → A

    law-get-put : ∀ (whole : A) → put (get whole) whole ≡ whole
    law-put-get : ∀ (whole : A) (part : B) → get (put part whole) ≡ part
    law-put-put : ∀ (whole : A) (part1 part2 : B) → put part2 (put part1 whole) ≡ put part2 whole

infixl 10 _⟶_

_⟶_ : ∀ {A} {B} {C} → Lens A B → Lens B C → Lens A C
parent ⟶ child = record
                   { get = λ whole → Lens.get child (Lens.get parent whole)
                   ; put = λ part whole → Lens.put parent (Lens.put child part (Lens.get parent whole)) whole
                   ; law-get-put = λ whole →
                       begin
                         Lens.put parent (Lens.put child (Lens.get child (Lens.get parent whole)) (Lens.get parent whole)) whole
                       ≡⟨ cong (λ x → Lens.put parent x whole) (Lens.law-get-put child ((Lens.get parent whole))) ⟩
                         Lens.put parent (Lens.get parent whole) whole
                       ≡⟨ Lens.law-get-put parent whole ⟩
                         whole
                       ∎
                   ; law-put-get = λ whole part →
                       begin
                         Lens.get child (Lens.get parent (Lens.put parent (Lens.put child part (Lens.get parent whole)) whole))
                       ≡⟨ cong (λ x → Lens.get child x) (Lens.law-put-get parent whole ((Lens.put child part (Lens.get parent whole)))) ⟩
                         Lens.get child (Lens.put child part (Lens.get parent whole))
                       ≡⟨ Lens.law-put-get child (Lens.get parent whole) part ⟩
                         part
                       ∎
                   ; law-put-put = λ whole part1 part2 →
                       begin
                         Lens.put parent (Lens.put child part2 (Lens.get parent (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole))) (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole)
                       ≡⟨ cong (λ x → Lens.put parent (Lens.put child part2 x) (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole)) (Lens.law-put-get parent whole (Lens.put child part1 (Lens.get parent whole))) ⟩
                         Lens.put parent (Lens.put child part2 (Lens.put child part1 (Lens.get parent whole))) (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole)
                       ≡⟨ cong (λ x → Lens.put parent x (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole)) (Lens.law-put-put child (Lens.get parent whole) part1 part2) ⟩
                         Lens.put parent (Lens.put child part2 (Lens.get parent whole)) (Lens.put parent (Lens.put child part1 (Lens.get parent whole)) whole)
                       ≡⟨ Lens.law-put-put parent whole (Lens.put child part1 (Lens.get parent whole)) (Lens.put child part2 (Lens.get parent whole)) ⟩
                         Lens.put parent (Lens.put child part2 (Lens.get parent whole)) whole
                       ∎
                   }

module examples where
  open import Data.Product

  tuple₁ : ∀ {A} {B} → Lens (A × B) A
  tuple₁ = record
             { get = λ{ (fst , snd) → fst }
             ; put = λ{ value (fst , snd) → value , snd }
             ; law-get-put = λ{ whole → refl }
             ; law-put-get = λ{ whole part → refl }
             ; law-put-put = λ{ whole part1 part2 → refl }
             }

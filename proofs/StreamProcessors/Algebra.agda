module StreamProcessors.Algebra where

open import Algebra.Definitions
open import Algebra.Structures
open import Codata.Thunk as Thunk using (Thunk; force)
open import Data.Product as Product using (_,_)
open import Level using (Level)
import Relation.Binary.PropositionalEquality as Eq
open import Size

open import StreamProcessors.Core
import StreamProcessors.Relation
open StreamProcessors.Relation.PropositionalEquality

private
  variable
    i : Size
    α : Level
    A B : Set α

infixr 5 _++_ _♯++_

_++_ : Pipe A B i → Pipe A B i → Pipe A B i
_♯++_ : Thunk (Pipe A B) i → Pipe A B i → Thunk (Pipe A B) i
stop ++ b = b
yield value next ++ b = yield value (next ♯++ b)
demand onNext ++ b = demand λ value → onNext value ♯++ b
lazy next ++ b = lazy (next ♯++ b)
(a ♯++ b) .force = a .force ++ b

concat : Pipe A (Pipe A B i) i → Pipe A B i
concat stop = stop
concat (yield pipe next) = pipe ++ lazy λ where .force → concat (next .force)
concat (demand onNext) = demand λ value → λ where .force → concat (onNext value .force)
concat (lazy next) = lazy λ where .force → concat (next .force)

++-cong : Congruent₂ (_⊢_≈_ i {A} {B}) _++_
++-cong ≈stop b = b
++-cong (≈yield value next) b = ≈yield value λ where .force → ++-cong (next .force) b
++-cong (≈demand onNext) b = ≈demand λ value → λ where .force → ++-cong (onNext value .force) b
++-cong (≈lazyˡ next) b = ≈lazyˡ λ where .force → ++-cong (next .force) b
++-cong (≈lazyʳ next) b = ≈lazyʳ λ where .force → ++-cong (next .force) b
++-cong (≈thunk relation) b = ≈thunk λ where .force → ++-cong (relation .force) b

++-assoc : Associative (_⊢_≈_ i {A} {B}) _++_
++-assoc stop b c = refl
++-assoc (yield value next) b c = ≈yield Eq.refl λ where .force → ++-assoc (next .force) b c
++-assoc (demand onNext) b c = ≈demand λ value → λ where .force → ++-assoc (onNext value .force) b c
++-assoc (lazy next) b c = ≈lazyᵇ λ where .force → ++-assoc (next .force) b c

++-identityˡ : ∀ {s : Pipe A B ∞} → i ⊢ s ≈ stop → LeftIdentity (_⊢_≈_ i {A} {B}) s _++_
++-identityˡ {s = s} s≈stop _ = ++-cong s≈stop refl

++-identityʳ : ∀ {s : Pipe A B ∞} → i ⊢ s ≈ stop → RightIdentity (_⊢_≈_ i {A} {B}) s _++_
++-identityʳ s≈stop stop = s≈stop
++-identityʳ s≈stop (yield value next) = ≈yield Eq.refl λ where .force → ++-identityʳ s≈stop (next .force)
++-identityʳ s≈stop (demand onNext) = ≈demand λ value → λ where .force → ++-identityʳ s≈stop (onNext value .force)
++-identityʳ s≈stop (lazy next) = ≈lazyᵇ λ where .force → ++-identityʳ s≈stop (next .force)

++-identity : Identity (_⊢_≈_ i {A} {B}) stop _++_
++-identity = ++-identityˡ refl , ++-identityʳ refl

isMagma : IsMagma (_⊢_≈_ i {A} {B}) _++_
isMagma =
  record
    { isEquivalence = isEquivalence
    ; ∙-cong = ++-cong
    }

isSemigroup : IsSemigroup (_⊢_≈_ i {A} {B}) _++_
isSemigroup =
  record
    { isMagma = isMagma
    ; assoc = ++-assoc
    }

isMonoid : IsMonoid (_⊢_≈_ i {A} {B}) _++_ stop
isMonoid =
  record
    { isSemigroup = isSemigroup
    ; identity = ++-identity
    }

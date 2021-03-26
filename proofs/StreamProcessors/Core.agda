module StreamProcessors.Core where

open import Codata.Thunk as Thunk using (Thunk; force)
open import Level using (Level)
open import Size using (Size)

private
  variable
    i : Size
    α : Level
    A B : Set α

data Pipe (A B : Set α) (i : Size) : Set α where
  stop : Pipe A B i
  yield : (value : B) → (next : Thunk (Pipe A B) i) → Pipe A B i
  demand : (onNext : (value : A) → Thunk (Pipe A B) i) → Pipe A B i
  lazy : (next : Thunk (Pipe A B) i) → Pipe A B i

stop♯ : Thunk (Pipe A B) i
stop♯ .force = stop

id : Pipe A A i
id′ : A → Pipe A A i
id = demand λ value → λ where .force → id′ value
id′ value = yield value λ where .force → id

module Bakery.Shell.Operation
  ( type (#>) (..),
    (|>),
    (<|),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)

data i #> o = Operation Inputs (Pipe i o (SafeT IO) ())

instance HasInputs (i #> o) where
  getInputs (Operation inputs _) = inputs

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

infixl 5 <|

(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

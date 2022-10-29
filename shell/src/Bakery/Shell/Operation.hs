module Bakery.Shell.Operation
  ( type (#>),
    Operation (..),
    (|>),
    (<|),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)

type i #> o = Operation i o IO ()

data Operation i o m r = Operation Inputs (Pipe i o (SafeT m) r)

instance HasInputs (Operation i o m r) where
  getInputs (Operation inputs _) = inputs

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

infixl 5 <|

(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

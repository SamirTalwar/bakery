module Bakery.Shell.Operation
  ( type (#>) (..),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Pipes qualified
import Pipes.Safe (SafeT)

data i #> o = Operation Inputs (Pipes.Pipe i o (SafeT IO) ())

instance HasInputs (i #> o) where
  getInputs (Operation inputs _) = inputs

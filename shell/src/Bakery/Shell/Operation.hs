module Bakery.Shell.Operation
  ( type (#>) (..),
  )
where

import Bakery.Input (Inputs)
import Pipes qualified
import Pipes.Safe (SafeT)

data i #> o = Operation Inputs (Pipes.Pipe i o (SafeT IO) ())

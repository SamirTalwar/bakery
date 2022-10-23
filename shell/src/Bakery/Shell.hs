module Bakery.Shell
  ( module Bakery.Shell.Builder,
    module Bakery.Shell.Chunk,
    module Bakery.Shell.Evaluate,
    module Bakery.Shell.Run,
    type (#>),
    shellInputs,
  )
where

import Bakery.Input
import Bakery.Shell.Builder
import Bakery.Shell.Chunk
import Bakery.Shell.Evaluate
import Bakery.Shell.Operation
import Bakery.Shell.Run

shellInputs :: i #> o -> Inputs
shellInputs (Operation inputs _) = inputs

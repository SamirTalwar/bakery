module Bakery.Shell.Inputs (deriveInputs) where

import Bakery.Input (Inputs)
import Bakery.Shell.AST (type (#>) (..))
import Bakery.Shell.Argument (Arg (..))
import Bakery.Shell.Path (InputPath (..))
import Data.Foldable (toList)

deriveInputs :: i #> o -> Inputs
deriveInputs NullStdIn = []
deriveInputs NullStdOut = []
deriveInputs (Run args) =
  toList args >>= \case
    StringArg _ -> []
    IntegerArg _ -> []
    PathArg (InputPath inputs _) -> inputs
deriveInputs (Read (InputPath inputs _)) = inputs
deriveInputs (Write _) = []
deriveInputs (Compose a b) = deriveInputs a <> deriveInputs b

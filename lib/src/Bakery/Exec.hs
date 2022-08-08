module Bakery.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell
import Data.Text.IO qualified as Text.IO
import Data.Typeable (Typeable)

newtype Exec = Exec String
  deriving newtype (Eq, Typeable, Show)

exec :: String -> Exec
exec = Exec

instance Identifiable Exec where
  identifier (Exec name) = Id "exec" name

instance Bakeable Exec where
  type Recipe Exec = (StdIn #> StdOut)
  deriveInputs _ = deriveShellInputs
  exists _ = pure False
  follow recipe target = do
    StdOut stdout <- evaluateShell recipe (StdIn mempty)
    Text.IO.putStr stdout
    pure target

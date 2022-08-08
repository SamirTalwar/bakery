module Bakery.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell.AST (type (#>))
import Bakery.Shell.Evaluate qualified as Shell
import Bakery.Shell.Inputs qualified as Shell
import Bakery.Shell.Pipe (StdIn (..), StdOut (..))
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
  deriveInputs _ = Shell.deriveInputs
  exists _ = pure False
  follow recipe target = do
    StdOut stdout <- Shell.evaluate recipe (StdIn mempty)
    Text.IO.putStr stdout
    pure target

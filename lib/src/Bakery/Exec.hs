module Bakery.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell.AST (type (#>))
import Bakery.Shell.Evaluate qualified as Shell
import Bakery.Shell.Inputs qualified as Shell
import Bakery.Shell.Pipe (StdIn (..), StdOut (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Typeable (Typeable)

newtype Exec = Exec Text
  deriving newtype (Eq, Typeable, Show)

exec :: String -> Exec
exec = Exec . Text.pack

instance Identifiable Exec where
  namespace _ = Namespace "exec"
  name (Exec execName) = Name execName

instance Bakeable Exec where
  type Recipe Exec = (StdIn #> StdOut)
  deriveInputs _ = Shell.deriveInputs
  exists _ = pure True
  follow recipe target = do
    StdOut stdout <- liftIO $ Shell.evaluate recipe (StdIn mempty)
    liftIO $ Text.IO.putStr stdout
    pure target

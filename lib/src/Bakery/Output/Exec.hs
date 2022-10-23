module Bakery.Output.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell (StdOut (..), shellInputs, type (#>))
import Bakery.Shell.Evaluate qualified as Shell
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)

newtype Exec = Exec Text
  deriving newtype (Eq, Typeable, Show)

exec :: String -> Exec
exec = Exec . Text.pack

instance Identifiable Exec where
  namespace _ = Namespace ""
  name (Exec execName) = Name execName

instance Bakeable Exec where
  type Recipe Exec = (() #> StdOut)
  parseName = pure . Just . Exec
  deriveInputs _ = shellInputs
  exists _ = pure True
  follow recipe target = do
    stdout <- liftIO $ Shell.evaluate recipe
    case stdout of
      StdOut output -> liftIO $ ByteString.putStr output
      StdOutEnd -> pure ()
    pure target

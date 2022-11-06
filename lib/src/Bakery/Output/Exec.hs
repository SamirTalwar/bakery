module Bakery.Output.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (fold)
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
  newtype Recipe Exec = ExecShell (Shell IO () (Chunk ByteString) ())
    deriving newtype (HasInputs)
  parseName = pure . Just . Exec
  deriveInputs _ = getInputs
  exists _ = pure True
  follow (ExecShell recipe) target = do
    stdout <- liftIO $ evaluate recipe [()]
    forM_ stdout $ liftIO . ByteString.putStr . fold
    pure target

instance IsShell IO () (Chunk ByteString) (Recipe Exec) where
  shell = ExecShell

instance IsShell IO (Chunk ByteString) (Chunk ByteString) (Recipe Exec) where
  shell sh = ExecShell (n sh)

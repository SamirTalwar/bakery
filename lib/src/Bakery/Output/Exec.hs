module Bakery.Output.Exec (Exec, exec) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell
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
  type Recipe Exec = (() #> Chunk ByteString)
  parseName = pure . Just . Exec
  deriveInputs _ = getInputs
  exists _ = pure True
  follow recipe target = do
    stdout <- liftIO $ evaluate recipe
    liftIO $ ByteString.putStr (fold stdout)
    pure target

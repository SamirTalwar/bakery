module Bakery.Path where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text

newtype Path = Path Text
  deriving newtype (Eq)

instance IsString Path where
  fromString = Path . fromString

instance Show Path where
  show (Path path) = Text.unpack path

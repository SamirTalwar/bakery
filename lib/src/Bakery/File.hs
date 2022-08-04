module Bakery.File where

import Bakery.Bakeable
import Bakery.Path
import Bakery.Run
import Data.String (fromString)
import Data.Typeable (Typeable)

newtype File = File Path
  deriving newtype (Eq, Show)
  deriving stock (Typeable)

instance Bakeable File where
  data Recipe File = FileRecipe File (() #> ())
    deriving stock (Show)
  follow (FileRecipe f recipe) = evaluateShell recipe *> pure f

file :: String -> File
file path = File (fromString path)

shell :: (Path -> () #> ()) -> File -> Recipe File
shell runnable f@(File path) = FileRecipe f (runnable path)

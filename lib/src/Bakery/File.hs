module Bakery.File where

import Bakery.Bakeable
import Bakery.Path
import Bakery.Run
import Data.String (fromString)

data File = File Path

instance Bakeable File where
  data Recipe File = FileRecipe File (() #> ())
  follow (FileRecipe f recipe) = evaluateShell recipe *> pure f

file :: String -> File
file path = File (fromString path)

shell :: (Path -> () #> ()) -> File -> Recipe File
shell runnable f@(File path) = FileRecipe f (runnable path)

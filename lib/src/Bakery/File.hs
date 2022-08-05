module Bakery.File (file, shell) where

import Bakery.Bakeable
import Bakery.Run
import Data.String (fromString)
import Data.Typeable (Typeable)
import System.Directory qualified as Directory

newtype File = File FilePath
  deriving newtype (Eq)
  deriving stock (Typeable)

instance Show File where
  show (File path) = path

instance Bakeable File where
  data Recipe File = FileRecipe File (() #> ())
    deriving stock (Show)
  deriveInputs (FileRecipe _ recipe) = deriveShellInputs recipe
  exists (File path) = Directory.doesPathExist path
  follow (FileRecipe f recipe) = evaluateShell recipe *> pure f

instance InShell File where
  inShell (File path) = path

file :: String -> File
file path = File (fromString path)

shell :: (forall a. InShell a => a -> () #> ()) -> File -> Recipe File
shell runnable f = FileRecipe f (runnable f)

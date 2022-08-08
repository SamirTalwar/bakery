module Bakery.File (File, file) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell
import Data.Functor (($>))
import Data.String (fromString)
import Data.Typeable (Typeable)
import System.Directory qualified as Directory

newtype File = File FilePath
  deriving newtype (Eq)
  deriving stock (Typeable)

instance Show File where
  show (File path) = path

instance Identifiable File where
  identifier (File path) = Id "file" path

instance Bakeable File where
  type Recipe File = OutputPath -> () #> ()
  deriveInputs _ recipe = deriveShellInputs (recipe UnknownOutputPath)
  exists (File path) = Directory.doesPathExist path
  follow recipe f@(File path) = evaluateShell (recipe (KnownOutputPath path)) () $> f

instance Path File where
  toInputPath f@(File path) = InputPath [SomeInput (Input f)] path

file :: String -> File
file path = File (fromString path)

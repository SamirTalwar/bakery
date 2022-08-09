module Bakery.File (File, file) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell.AST (type (#>))
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Evaluate qualified as Shell
import Bakery.Shell.Inputs qualified as Shell
import Bakery.Shell.Path (InputPath (..), OutputPath (..), Path (..))
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
  deriveInputs _ recipe = Shell.deriveInputs (recipe UnknownOutputPath)
  exists (File path) = Directory.doesPathExist path
  follow recipe f@(File path) = Shell.evaluate (recipe (KnownOutputPath path)) () $> f

instance Path File where
  toInputPath f@(File path) = InputPath [SomeInput (Input f)] path

instance Argument File where
  toArg = InputPathArg . toInputPath

file :: String -> File
file path = File (fromString path)

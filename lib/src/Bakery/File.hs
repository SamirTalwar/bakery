module Bakery.File (File, file) where

import Bakery.Bakeable
import Bakery.Baking
import Bakery.Env qualified as Env
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell.AST (type (#>))
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Evaluate qualified as Shell
import Bakery.Shell.Inputs qualified as Shell
import Bakery.Shell.Path (InputPath (..), OutputPath (..), Path (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Functor (($>))
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath

newtype File = File FilePath
  deriving newtype (Eq)
  deriving stock (Typeable)

instance Show File where
  show (File path) = path

instance Identifiable File where
  namespace _ = Namespace "file"
  name (File path) = Name (Text.pack path)

instance Bakeable File where
  type Recipe File = OutputPath -> () #> ()
  normalize (File path) = Baking $ do
    root <- asks Env.root
    canonicalPath <- liftIO $ Directory.canonicalizePath path
    pure . File $ FilePath.makeRelative root canonicalPath
  deriveInputs _ recipe = Shell.deriveInputs (recipe UnknownOutputPath)
  exists (File path) = liftIO $ Directory.doesPathExist path
  follow recipe f@(File path) = liftIO $ Shell.evaluate (recipe (KnownOutputPath path)) () $> f

instance Path File where
  toInputPath f@(File path) = InputPath [SomeInput (Input f)] path

instance Argument File where
  toArg = InputPathArg . toInputPath

file :: String -> File
file path = File (fromString path)

module Bakery.Output.File.Internal (File (..), file) where

import Bakery.A
import Bakery.Bakeable
import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell (evaluate_, shellInputs, type (#>))
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Path (OutputPath (..), Path (..))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.Directory qualified as Directory
import System.FilePath ((</>))
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
  type Recipe File = OutputPath -> () #> Void
  normalize (File path) = Baking $ do
    -- we probably need to reimplement this with regards to 'Env.root'
    root <- liftIO Directory.getCurrentDirectory
    canonicalPath <- liftIO $ Directory.canonicalizePath path
    let relativePath = FilePath.makeRelative root canonicalPath
    if FilePath.isRelative relativePath
      then pure $ File ("." </> relativePath)
      else pure $ File relativePath -- this is absolute
  parseName pathText =
    let path = Text.unpack pathText
     in if FilePath.isValid path
          then Just <$> normalize (File path)
          else pure Nothing
  deriveInputs _ recipe = shellInputs $ recipe UnknownOutputPath
  exists (File path) = liftIO $ Directory.doesPathExist path
  follow recipe f@(File path) = liftIO $ evaluate_ (recipe (KnownOutputPath path)) $> f

instance HasInputs File where
  getInputs self = [An (Input self)]

instance Path File where
  toPath (File path) = path

instance Argument File where
  toArg = StringArg . toPath

file :: String -> File
file path = File (fromString path)

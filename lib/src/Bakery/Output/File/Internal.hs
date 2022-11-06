module Bakery.Output.File.Internal (File (..), file, target, Recipe (..)) where

import Bakery.A
import Bakery.Bakeable
import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Path (Path (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Functor (($>))
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

file :: String -> File
file = File

target :: OutputPath (ReaderT FilePath IO)
target = OutputPath ask

newtype File = File FilePath
  deriving newtype (Eq)
  deriving stock (Typeable)

instance Show File where
  show (File path) = path

instance Identifiable File where
  namespace _ = Namespace "file"
  name (File path) = Name (Text.pack path)

instance Bakeable File where
  newtype Recipe File = FileShell (Shell (ReaderT FilePath IO) () Void ())
    deriving newtype (HasInputs)
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
  deriveInputs _ = getInputs
  exists (File path) = liftIO $ Directory.doesPathExist path
  follow (FileShell recipe) f@(File path) = liftIO (runReaderT (evaluate_ recipe) path) $> f

instance IsShell (ReaderT FilePath IO) () Void (Recipe File) where
  shell = FileShell

instance HasInputs File where
  getInputs self = [An (Input self)]

instance Applicative m => Path m File where
  toPath (File path) = pure path

instance Applicative m => Argument m File where
  toArg (File path) = pure $ PathArg path

newtype OutputPath m = OutputPath (m FilePath)

instance HasInputs (OutputPath m) where
  getInputs = const []

instance Functor m => Argument m (OutputPath m) where
  toArg (OutputPath path) = PathArg <$> path

instance Path m (OutputPath m) where
  toPath (OutputPath pathM) = pathM

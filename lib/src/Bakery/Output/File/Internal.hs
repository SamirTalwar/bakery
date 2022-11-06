{-# LANGUAGE FlexibleContexts #-}

module Bakery.Output.File.Internal (File (..), file, copy, target) where

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
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

file :: String -> File
file = File

copy :: Path Identity a => a -> Recipe File
copy = FileCopy . runIdentity . toPath

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
  data Recipe File
    = FileCopy FilePath
    | FileShell (Shell (ReaderT FilePath IO) () Void ())
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
  follow (FileCopy source) f@(File path) = liftIO (Directory.copyFile source path) $> f
  follow (FileShell recipe) f@(File path) = liftIO (runReaderT (evaluate_ recipe) path) $> f

instance HasInputs (Recipe File) where
  getInputs (FileCopy path) = [An (Input (File path))]
  getInputs (FileShell sh) = getInputs sh

instance IsShell (ReaderT FilePath IO) () Void (Recipe File) where
  shell = FileShell

instance IsShell (ReaderT FilePath IO) (Chunk ByteString) Void (Recipe File) where
  shell sh = FileShell (n sh)

instance IsShell (ReaderT FilePath IO) () (Chunk ByteString) (Recipe File) where
  shell sh = FileShell (n sh)

instance IsShell (ReaderT FilePath IO) (Chunk ByteString) (Chunk ByteString) (Recipe File) where
  shell sh = FileShell (n sh)

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

module Bakery.Shell.Path
  ( Path (..),
    OutputPath (..),
    PathException (..),
  )
where

import Bakery.Input (HasInputs (..))
import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadThrow)

class Path a where
  toPath :: MonadThrow m => a -> m FilePath

instance Path FilePath where
  toPath = pure

data OutputPath where
  KnownOutputPath :: FilePath -> OutputPath
  UnknownOutputPath :: OutputPath

instance HasInputs OutputPath where
  getInputs = const []

instance Path OutputPath where
  toPath (KnownOutputPath path) = pure path
  toPath UnknownOutputPath = throw UnknownOutputPathException

data PathException where
  UnknownOutputPathException :: PathException

instance Show PathException where
  show UnknownOutputPathException = "INTERNAL ERROR: Cannot write to an unknown path."

instance Exception PathException

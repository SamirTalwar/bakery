module Bakery.Shell.Path
  ( OutputPath (..),
    Path (..),
    PathException (..),
  )
where

import Bakery.Input (HasInputs (..))
import Control.Exception (Exception)

data OutputPath where
  KnownOutputPath :: FilePath -> OutputPath
  UnknownOutputPath :: OutputPath

deriving stock instance Show OutputPath

instance HasInputs OutputPath where
  getInputs = const []

class Path a where
  toPath :: a -> FilePath

data PathException where
  UnknownOutputPathException :: PathException

instance Show PathException where
  show UnknownOutputPathException = "INTERNAL ERROR: Cannot write to an unknown path."

instance Exception PathException

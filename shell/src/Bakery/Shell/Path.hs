module Bakery.Shell.Path
  ( OutputPath (..),
    Path (..),
    unknownOutputPathFailure,
  )
where

import Bakery.Input (HasInputs (..))

data OutputPath where
  KnownOutputPath :: FilePath -> OutputPath
  UnknownOutputPath :: OutputPath

deriving stock instance Show OutputPath

instance HasInputs OutputPath where
  getInputs = const []

class Path a where
  toPath :: a -> FilePath

unknownOutputPathFailure :: String
unknownOutputPathFailure = "INTERNAL ERROR: Cannot write to an unknown path."

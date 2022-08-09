module Bakery.Shell.Path
  ( InputPath (..),
    OutputPath (..),
    Path (..),
    unknownOutputPathFailure,
  )
where

import Bakery.Input (Inputs)

data InputPath where
  InputPath :: Inputs -> FilePath -> InputPath

deriving stock instance Show InputPath

data OutputPath where
  KnownOutputPath :: FilePath -> OutputPath
  UnknownOutputPath :: OutputPath

deriving stock instance Show OutputPath

class Path a where
  toInputPath :: a -> InputPath

unknownOutputPathFailure :: String
unknownOutputPathFailure = "INTERNAL ERROR: Cannot write to an unknown path."

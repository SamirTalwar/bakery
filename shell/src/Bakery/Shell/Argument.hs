module Bakery.Shell.Argument
  ( Arg (..),
    Argument (..),
    fromArg,
  )
where

import Bakery.Shell.Path (InputPath (..))

data Arg where
  StringArg :: String -> Arg
  IntegerArg :: Integer -> Arg
  PathArg :: InputPath -> Arg

deriving stock instance Show Arg

class Argument a where
  toArg :: a -> Arg

instance Argument String where
  toArg = StringArg

instance Argument Integer where
  toArg = IntegerArg

instance Argument InputPath where
  toArg = PathArg

fromArg :: Arg -> String
fromArg (StringArg arg) = arg
fromArg (IntegerArg arg) = show arg
fromArg (PathArg (InputPath _ arg)) = arg

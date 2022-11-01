module Bakery.Shell.Argument
  ( Arg (..),
    Argument (..),
    fromArg,
  )
where

import Bakery.Shell.Path (OutputPath (..), PathException (..))

data Arg where
  StringArg :: String -> Arg
  IntegerArg :: Integer -> Arg
  OutputPathArg :: OutputPath -> Arg

deriving stock instance Show Arg

class Argument a where
  toArg :: a -> Arg

instance Argument String where
  toArg = StringArg

instance Argument Integer where
  toArg = IntegerArg

instance Argument OutputPath where
  toArg = OutputPathArg

fromArg :: Arg -> String
fromArg (StringArg arg) = arg
fromArg (IntegerArg arg) = show arg
fromArg (OutputPathArg (KnownOutputPath arg)) = arg
fromArg (OutputPathArg UnknownOutputPath) = error (show UnknownOutputPathException)

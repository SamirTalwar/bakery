module Bakery.Shell.Argument
  ( Arg (..),
    Argument (..),
    fromArg,
  )
where

data Arg where
  StringArg :: String -> Arg
  IntegerArg :: Integer -> Arg
  PathArg :: FilePath -> Arg
  ErrorArg :: String -> Arg

deriving stock instance Show Arg

class Argument m a where
  toArg :: a -> m Arg

instance Applicative m => Argument m String where
  toArg = pure . StringArg

instance Applicative m => Argument m Integer where
  toArg = pure . IntegerArg

instance (Monad m, Argument m a) => Argument m (m a) where
  toArg = (toArg =<<)

fromArg :: Arg -> String
fromArg (StringArg arg) = arg
fromArg (IntegerArg arg) = show arg
fromArg (PathArg arg) = arg
fromArg (ErrorArg message) = error message

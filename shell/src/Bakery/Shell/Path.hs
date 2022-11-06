module Bakery.Shell.Path (Path (..)) where

class Path m a where
  toPath :: a -> m FilePath

instance Applicative m => Path m FilePath where
  toPath = pure

instance (Monad m, Path m a) => Path m (m a) where
  toPath = (toPath =<<)

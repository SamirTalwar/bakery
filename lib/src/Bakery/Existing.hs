module Bakery.Existing where

import Bakery.Bakeable
import Data.Typeable (Typeable)

newtype Existing a = Existing a
  deriving newtype (Eq, Show, Typeable)

existing :: Bakeable a => a -> Bake (Existing a)
existing x = Recipe (Existing x) (Verify x)

instance Bakeable a => Bakeable (Existing a) where
  data Recipe (Existing a) = Verify a
    deriving stock (Show)
  exists (Existing x) = exists x
  follow (Verify x) =
    exists x >>= \case
      True -> pure (Existing x)
      False -> fail ("Expected " <> show x <> " to exist.")

instance InShell a => InShell (Existing a) where
  inShell (Existing x) = inShell x

module Bakery.Existing (existing) where

import Bakery.Bakeable (Bake (Recipe), Bakeable (..), InShell (..))
import Data.Typeable (Typeable)

newtype Existing a = Existing a
  deriving newtype (Eq, Show, Typeable)

existing :: Bakeable a => a -> Bake (Existing a)
existing x = Recipe (Existing x) x

instance Bakeable a => Bakeable (Existing a) where
  type Recipe (Existing a) = a
  deriveInputs _ _ = []
  exists (Existing x) = exists x
  follow target recipe =
    exists recipe >>= \case
      True -> pure target
      False -> fail ("Expected " <> show recipe <> " to exist.")

instance InShell a => InShell (Existing a) where
  inShell (Existing x) = inShell x

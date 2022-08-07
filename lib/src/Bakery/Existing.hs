module Bakery.Existing (Existing, existing) where

import Bakery.Bakeable (Bake (Recipe), Bakeable (..), Input (..))
import Bakery.Run (InputPath (..), Path (..))
import Data.Typeable (Typeable)

newtype Existing a = Existing a
  deriving newtype (Eq, Show, Typeable)

existing :: Bakeable a => a -> Bake (Existing a)
existing x = Recipe (identifier x) (Existing x) [] (follow x (Existing x))

instance Bakeable a => Bakeable (Existing a) where
  type Recipe (Existing a) = a
  identifier (Existing x) = identifier x
  deriveInputs _ _ = []
  exists (Existing x) = exists x
  follow recipe target =
    exists recipe >>= \case
      True -> pure target
      False -> fail ("Expected " <> show recipe <> " to exist.")

instance (Path a, Bakeable a) => Path (Existing a) where
  toInputPath self@(Existing x) =
    let InputPath _ path = toInputPath x
     in InputPath [Input self] path

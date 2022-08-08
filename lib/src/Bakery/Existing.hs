module Bakery.Existing (Existing, existing) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Shell (InputPath (..), Path (..))
import Data.Typeable (Typeable)

newtype Existing a = Existing a
  deriving newtype (Eq, Show, Typeable)

existing :: Bakeable a => a -> Bake (Existing a)
existing x = Recipe (identifier x) (Existing x) [] (follow x (Existing x))

instance Identifiable a => Identifiable (Existing a) where
  identifier (Existing x) = identifier x

instance Bakeable a => Bakeable (Existing a) where
  type Recipe (Existing a) = a
  deriveInputs _ _ = []
  exists (Existing x) = exists x
  follow recipe target =
    exists recipe >>= \case
      True -> pure target
      False -> fail ("Expected " <> show recipe <> " to exist.")

instance (Path a, Bakeable a) => Path (Existing a) where
  toInputPath self@(Existing x) =
    let InputPath _ path = toInputPath x
     in InputPath [SomeInput (Input self)] path

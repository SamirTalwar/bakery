module Bakery.Existing (Existing, existing) where

import Bakery.Bakeable
import Bakery.Identifier
import Bakery.Input
import Bakery.Output (Output (..))
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Path (InputPath (..), Path (..))

newtype Existing a = Existing a
  deriving newtype (Eq, Show)

existing :: Bakeable a => a -> Bake (Existing a)
existing x =
  Recipe $
    Output
      (identifier x)
      (Existing x)
      []
      (exists (Existing x))
      (follow x (Existing x))

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

instance (Identifiable a, Path a, Show a) => Path (Existing a) where
  toInputPath self@(Existing x) =
    let InputPath _ path = toInputPath x
     in InputPath [SomeInput (Input self)] path

instance (Identifiable a, Path a, Show a) => Argument (Existing a) where
  toArg = InputPathArg . toInputPath

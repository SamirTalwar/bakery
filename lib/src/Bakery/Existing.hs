module Bakery.Existing (Existing, existing) where

import Bakery.Bakeable
import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Path (InputPath (..), Path (..))
import Data.Data (Proxy (..))

newtype Existing a = Existing a
  deriving newtype (Eq, Show)

existing :: Bakeable a => a -> BakeT Baking (Existing a)
existing x = defineRecipe (Existing x) x

instance Identifiable a => Identifiable (Existing a) where
  namespace _ = namespace (Proxy :: Proxy a)
  name (Existing x) = name x
  identifier (Existing x) = identifier x

instance Bakeable a => Bakeable (Existing a) where
  type Recipe (Existing a) = a
  normalize (Existing x) = Existing <$> normalize x
  parseName targetName = (Existing <$>) <$> parseName targetName
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

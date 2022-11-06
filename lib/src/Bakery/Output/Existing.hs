module Bakery.Output.Existing (Existing, existing) where

import Bakery.A
import Bakery.Bakeable
import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell.Argument (Argument (..))
import Bakery.Shell.Path (Path (..))
import Data.Data (Proxy (..))

newtype Existing a = Existing a
  deriving newtype (Eq, Show)

existing :: Bakeable a => a -> BakeT Baking (Existing a)
existing x = defineRecipe (Existing x) (ExistingRecipe x)

instance Identifiable a => Identifiable (Existing a) where
  namespace _ = namespace (Proxy :: Proxy a)
  name (Existing x) = name x
  identifier (Existing x) = identifier x

instance Bakeable a => Bakeable (Existing a) where
  newtype Recipe (Existing a) = ExistingRecipe a
  normalize (Existing x) = Existing <$> normalize x
  parseName targetName = (Existing <$>) <$> parseName targetName
  deriveInputs _ _ = []
  exists (Existing x) = exists x
  follow (ExistingRecipe recipe) target =
    exists (Existing recipe) >>= \case
      True -> pure target
      False -> fail ("Expected " <> show recipe <> " to exist.")

instance (Identifiable a, Show a) => HasInputs (Existing a) where
  getInputs self = [An (Input self)]

instance Path m a => Path m (Existing a) where
  toPath (Existing x) = toPath x

instance Argument m a => Argument m (Existing a) where
  toArg (Existing x) = toArg x

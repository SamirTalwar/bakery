{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Bakery.Bakeable
  ( Bakeable (..),
    BakeT (..),
    defineRecipe,
    defaultRecipe,
    deriveOutputs,
  )
where

import Bakery.A
import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Output
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Typeable (Proxy (..))

class (Eq a, Show a, Identifiable a) => Bakeable a where
  type Recipe a
  normalize :: a -> Baking a
  normalize = pure
  parseName :: Text -> Baking (Maybe a)
  deriveInputs :: Proxy a -> Recipe a -> Inputs
  exists :: a -> Baking Bool
  follow :: Recipe a -> a -> Baking a

newtype BakeT m a = BakeT {runBake :: m (Bake m a)}

data Bake (m :: Type -> Type) (a :: Type) where
  Value :: a -> Bake m a
  Recipe :: Output a -> Bake m a
  Both :: Bake m a -> Bake m b -> Bake m b

instance Monad m => Functor (BakeT m) where
  fmap f bake = BakeT $ do
    baked <- runBake bake
    pure $ fmap' baked
    where
      fmap' (Value x) = Value (f x)
      fmap' (Recipe output) = Recipe (fmap f output)
      fmap' (Both x y) = Both x (fmap' y)

instance Monad m => Applicative (BakeT m) where
  pure = BakeT . pure . Value
  (<*>) = ap

instance Monad m => Monad (BakeT m) where
  (>>=) :: forall a b. Monad m => BakeT m a -> (a -> BakeT m b) -> BakeT m b
  bake >>= f = BakeT do
    baked <- runBake bake
    runBake $ bind' baked
    where
      bind' :: Bake m a -> BakeT m b
      bind' (Value x) = f x
      bind' r@(Recipe (Output {outputTarget})) =
        BakeT $ Both r <$> runBake (f outputTarget)
      bind' (Both x y) =
        BakeT $ Both x <$> runBake (bind' y)

instance MonadTrans BakeT where
  lift = BakeT . (Value <$>)

instance MonadIO (BakeT IO) where
  liftIO = lift

deriveOutputs :: Monad m => BakeT m a -> m Outputs
deriveOutputs bake = deriveOutputs' <$> runBake bake
  where
    deriveOutputs' :: Bake m a -> Outputs
    deriveOutputs' (Value _) = []
    deriveOutputs' (Recipe output) = [An output]
    deriveOutputs' (Both x y) = deriveOutputs' x <> deriveOutputs' y

defineRecipe :: forall a. Bakeable a => a -> Recipe a -> BakeT Baking a
defineRecipe target recipe' = BakeT do
  normalized <- normalize target
  pure . Recipe $
    Output
      (identifier normalized)
      normalized
      (deriveInputs (Proxy :: Proxy a) recipe')
      (exists normalized)
      (follow recipe' normalized)

defaultRecipe :: forall a. Bakeable a => a -> BakeT Baking ()
defaultRecipe input =
  BakeT . pure $
    Recipe
      Output
        { outputId = defaulted (identifier input),
          outputTarget = (),
          outputInputs = [An (Input input)],
          outputExists = exists input,
          outputAction = pure ()
        }

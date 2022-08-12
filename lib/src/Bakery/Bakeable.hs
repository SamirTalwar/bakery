{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    BakeT (..),
    defineRecipe,
    deriveOutputs,
  )
where

import Bakery.Baking
import Bakery.Identifier
import Bakery.Input
import Bakery.Output
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Kind (Type)
import Data.Typeable (Proxy (..))

class (Eq a, Show a, Identifiable a) => Bakeable a where
  type Recipe a
  normalize :: a -> Baking a
  normalize = pure
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
  bake >>= f = BakeT do
    baked <- runBake bake
    runBake $ bind' baked
    where
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
deriveOutputs bake = do
  baked <- runBake bake
  deriveOutputs' baked

deriveOutputs' :: Monad m => Bake m a -> m Outputs
deriveOutputs' (Value _) = pure []
deriveOutputs' (Recipe output) = pure [SomeOutput output]
deriveOutputs' (Both x y) = (<>) <$> deriveOutputs' x <*> deriveOutputs' y

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

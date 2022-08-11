{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    Bake (..),
    Bake' (..),
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

newtype Bake m a = Bake {runBake :: m (Bake' m a)}

data Bake' (m :: Type -> Type) (a :: Type) where
  Value :: a -> Bake' m a
  Recipe :: Output a -> Bake' m a
  Both :: Bake' m a -> Bake' m b -> Bake' m b

instance Monad m => Functor (Bake m) where
  fmap f bake = Bake $ do
    baked <- runBake bake
    pure $ fmap' baked
    where
      fmap' (Value x) = Value (f x)
      fmap' (Recipe output) = Recipe (fmap f output)
      fmap' (Both x y) = Both x (fmap' y)

instance Monad m => Applicative (Bake m) where
  pure = Bake . pure . Value
  (<*>) = ap

instance Monad m => Monad (Bake m) where
  bake >>= f = Bake do
    baked <- runBake bake
    runBake $ bind' baked
    where
      bind' (Value x) = f x
      bind' recipe@(Recipe (Output {outputTarget})) =
        Bake $ Both recipe <$> runBake (f outputTarget)
      bind' (Both x y) =
        Bake $ Both x <$> runBake (bind' y)

instance MonadTrans Bake where
  lift = Bake . (Value <$>)

instance MonadIO (Bake IO) where
  liftIO = lift

deriveOutputs :: Monad m => Bake m a -> m Outputs
deriveOutputs bake = do
  baked <- runBake bake
  deriveOutputs' baked

deriveOutputs' :: Monad m => Bake' m a -> m Outputs
deriveOutputs' (Value _) = pure []
deriveOutputs' (Recipe output) = pure [SomeOutput output]
deriveOutputs' (Both x y) = (<>) <$> deriveOutputs' x <*> deriveOutputs' y

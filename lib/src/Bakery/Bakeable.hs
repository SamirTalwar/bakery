{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable where

import Control.Monad (ap)
import Data.Typeable (Typeable)

class (Eq a, Show a, Typeable a, Show (Recipe a)) => Bakeable a where
  data Recipe a
  follow :: Recipe a -> IO a

data Bake a where
  Value :: a -> Bake a
  Recipe :: Bakeable a => a -> Recipe a -> Bake a
  Both :: Bake a -> Bake b -> Bake b
  Map :: (a -> b) -> Bake a -> Bake b

instance Functor Bake where
  fmap f (Value x) = Value (f x)
  fmap f r@(Recipe _ _) = Map f r
  fmap f (Both x y) = Both x (fmap f y)
  fmap f (Map g x) = Map (f . g) x

instance Applicative Bake where
  pure = Value
  (<*>) = ap

instance Monad Bake where
  Value x >>= f = f x
  r@(Recipe x _) >>= f = Both r (f x)
  Both x y >>= f = Both x (y >>= f)
  b@(Map _ _) >>= f = Both b (b >>= f)

data Output where
  Output :: forall a. Bakeable a => a -> Recipe a -> Output

instance Show Output where
  show (Output x _) = show x

outputs :: forall a. Bake a -> [Output]
outputs (Value _) = []
outputs (Recipe out r) = [Output out r]
outputs (Both x y) = outputs x <> outputs y
outputs (Map _ x) = [Output out undefined | Output out _ <- outputs x]

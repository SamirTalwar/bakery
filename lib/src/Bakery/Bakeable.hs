{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    InShell (..),
    Bake (..),
    Output (..),
    deriveOutputs,
  )
where

import Control.Monad (ap)
import Data.Typeable (Typeable)

class (Eq a, Show a, Typeable a, Show (Recipe a)) => Bakeable a where
  data Recipe a
  exists :: a -> IO Bool
  follow :: Recipe a -> IO a

class Bakeable a => InShell a where
  inShell :: a -> String

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

deriveOutputs :: forall a. Bake a -> [Output]
deriveOutputs (Value _) = []
deriveOutputs (Recipe out r) = [Output out r]
deriveOutputs (Both x y) = deriveOutputs x <> deriveOutputs y
deriveOutputs (Map _ x) = [Output out undefined | Output out _ <- deriveOutputs x]

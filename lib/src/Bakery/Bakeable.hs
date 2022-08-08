{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    Bake (..),
    deriveOutputs,
  )
where

import Bakery.Identifier
import Bakery.Input
import Bakery.Output
import Control.Monad (ap)
import Data.Typeable (Proxy (..))

class (Eq a, Show a, Identifiable a) => Bakeable a where
  type Recipe a
  deriveInputs :: Proxy a -> Recipe a -> Inputs
  exists :: a -> IO Bool
  follow :: Recipe a -> a -> IO a

data Bake a where
  Value :: a -> Bake a
  Recipe :: Id -> a -> Inputs -> IO a -> Bake a
  Both :: Bake a -> Bake b -> Bake b

deriving stock instance Functor Bake

instance Applicative Bake where
  pure = Value
  (<*>) = ap

instance Monad Bake where
  Value x >>= f = f x
  r@(Recipe _ x _ _) >>= f = Both r (f x)
  Both x y >>= f = Both x (y >>= f)

deriveOutputs :: forall a. Bake a -> Outputs
deriveOutputs (Value _) = []
deriveOutputs (Recipe recipeId target inputs action) = [SomeOutput (Output recipeId target inputs action)]
deriveOutputs (Both x y) = deriveOutputs x <> deriveOutputs y

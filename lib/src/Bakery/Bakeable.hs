{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    Bake (..),
    Id (..),
    Input (..),
    Inputs,
    Output (..),
    Outputs,
    deriveOutputs,
  )
where

import Control.Monad (ap)
import Data.Typeable (Proxy (..), Typeable)

class (Eq a, Show a, Typeable a) => Bakeable a where
  type Recipe a
  identifier :: a -> Id
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

data Id = Id {idType :: String, idTarget :: String}
  deriving stock (Eq)

instance Show Id where
  show Id {idType, idTarget} = idType <> ":" <> idTarget

data Input where
  Input :: forall a. Bakeable a => a -> Input

instance Show Input where
  show (Input x) = show x

type Inputs = [Input]

data Output where
  Output :: Id -> a -> Inputs -> IO a -> Output

instance Eq Output where
  Output x _ _ _ == Output y _ _ _ =
    x == y

instance Show Output where
  show (Output outputId _ inputs _) = show outputId <> " <- " <> show inputs

type Outputs = [Output]

deriveOutputs :: forall a. Bake a -> Outputs
deriveOutputs (Value _) = []
deriveOutputs (Recipe recipeId out inputs r) = [Output recipeId out inputs r]
deriveOutputs (Both x y) = deriveOutputs x <> deriveOutputs y

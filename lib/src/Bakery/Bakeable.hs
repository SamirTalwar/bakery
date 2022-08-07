{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    Bake (..),
    Id (..),
    Identifiable (..),
    Input (..),
    Inputs,
    SomeInput (..),
    Output (..),
    Outputs,
    SomeOutput (..),
    deriveOutputs,
  )
where

import Control.Monad (ap)
import Data.Typeable (Proxy (..))

class Identifiable a where
  identifier :: a -> Id

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

data Id = Id {idType :: String, idTarget :: String}
  deriving stock (Eq)

instance Show Id where
  show Id {idType, idTarget} = idType <> ":" <> idTarget

data Input a where
  Input :: forall a. Bakeable a => a -> Input a

data SomeInput = forall a. SomeInput (Input a)

instance Show (Input a) where
  show (Input x) = show x

instance Show SomeInput where
  show (SomeInput x) = show x

type Inputs = [SomeInput]

data Output a where
  Output :: Id -> a -> Inputs -> IO a -> Output a

data SomeOutput = forall a. SomeOutput (Output a)

instance Show (Output a) where
  show (Output outputId _ inputs _) = show outputId <> " <- " <> show inputs

instance Show SomeOutput where
  show (SomeOutput x) = show x

instance Identifiable (Output a) where
  identifier (Output outputId _ _ _) = outputId

instance Identifiable SomeOutput where
  identifier (SomeOutput x) = identifier x

type Outputs = [SomeOutput]

deriveOutputs :: forall a. Bake a -> Outputs
deriveOutputs (Value _) = []
deriveOutputs (Recipe recipeId out inputs r) = [SomeOutput (Output recipeId out inputs r)]
deriveOutputs (Both x y) = deriveOutputs x <> deriveOutputs y

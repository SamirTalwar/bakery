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
  Map :: (a -> b) -> Bake a -> Bake b

instance Functor Bake where
  fmap f (Value x) = Value (f x)
  -- This is nonsense and probably breaks a Functor law.
  -- We need to have a 'Map' value because 'Recipe' is not a 'Functor'.
  -- It's a data family, so 'Functor' would have to be added to every
  -- implementation, which I do not want to do.
  --
  -- We can potentially get around this by storing a function and not
  -- a 'Recipe', but that seems a little /too/ flexible.
  fmap f r@(Recipe _ _ _ _) = Map f r
  fmap f (Both x y) = Both x (fmap f y)
  fmap f (Map g x) = Map (f . g) x

instance Applicative Bake where
  pure = Value
  (<*>) = ap

instance Monad Bake where
  Value x >>= f = f x
  r@(Recipe _ x _ _) >>= f = Both r (f x)
  Both x y >>= f = Both x (y >>= f)
  b@(Map _ _) >>= f = Both b (b >>= f)

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
-- See above.
deriveOutputs (Map _ x) = [Output outputId out inputs undefined | Output outputId out inputs _ <- deriveOutputs x]

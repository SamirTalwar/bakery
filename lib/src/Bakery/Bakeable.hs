{-# LANGUAGE FlexibleContexts #-}

module Bakery.Bakeable
  ( Bakeable (..),
    InShell (..),
    Bake (..),
    Input (..),
    Output (..),
    deriveOutputs,
  )
where

import Control.Monad (ap)
import Data.Typeable (Typeable, eqT, (:~:) (..))

class (Eq a, Show a, Typeable a, Show (Recipe a)) => Bakeable a where
  data Recipe a
  deriveInputs :: Recipe a -> [Input]
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
  -- This is nonsense and probably breaks a Functor law.
  -- We need to have a 'Map' value because 'Recipe' is not a 'Functor'.
  -- It's a data family, so 'Functor' would have to be added to every
  -- implementation, which I do not want to do.
  --
  -- We can potentially get around this by storing a function and not
  -- a 'Recipe', but that seems a little /too/ flexible.
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

data Input where
  Input :: forall a. Bakeable a => a -> Input

instance Show Input where
  show (Input x) = show x

data Output where
  Output :: forall a. Bakeable a => a -> [Input] -> Recipe a -> Output

instance Eq Output where
  Output @x x _ _ == Output @y y _ _ =
    case eqT @x @y of
      Just Refl -> x == y
      Nothing -> False

instance Show Output where
  show (Output x inputs _) = show x <> " <- " <> show inputs

deriveOutputs :: forall a. Bake a -> [Output]
deriveOutputs (Value _) = []
deriveOutputs (Recipe out r) = [Output out (deriveInputs r) r]
deriveOutputs (Both x y) = deriveOutputs x <> deriveOutputs y
-- See above.
deriveOutputs (Map _ x) = [Output out inputs undefined | Output out inputs _ <- deriveOutputs x]

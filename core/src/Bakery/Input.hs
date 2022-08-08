module Bakery.Input
  ( Input (..),
    Inputs,
    SomeInput (..),
  )
where

import Bakery.Identifier (Identifiable)

data Input a where
  Input :: (Identifiable a, Show a) => a -> Input a

data SomeInput = forall a. SomeInput (Input a)

instance Show (Input a) where
  show (Input x) = show x

instance Show SomeInput where
  show (SomeInput x) = show x

type Inputs = [SomeInput]

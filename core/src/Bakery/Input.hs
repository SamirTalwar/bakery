{-# LANGUAGE FlexibleInstances #-}

module Bakery.Input
  ( Input (..),
    Inputs,
  )
where

import Bakery.A
import Bakery.Identifier (Identifiable)

data Input a where
  Input :: (Identifiable a, Show a) => a -> Input a

instance Show (Input a) where
  show (Input x) = show x

type Inputs = [An Input]

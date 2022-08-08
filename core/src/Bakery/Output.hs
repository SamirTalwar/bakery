module Bakery.Output
  ( Output (..),
    Outputs,
    SomeOutput (..),
  )
where

import Bakery.Identifier
import Bakery.Input

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

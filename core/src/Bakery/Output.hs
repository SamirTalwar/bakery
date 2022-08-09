module Bakery.Output
  ( Output (..),
    Outputs,
    SomeOutput (..),
  )
where

import Bakery.Identifier
import Bakery.Input

-- We have to capture all this information because we cannot rely on a
-- 'Bakeable' constraint. Aside from 'Bakeable' not existing in this package,
-- constraints are lost in 'SomeOutput'.
data Output a = Output
  { outputId :: Id,
    outputTarget :: a,
    outputInputs :: Inputs,
    outputExists :: IO Bool,
    outputAction :: IO a
  }

data SomeOutput = forall a. SomeOutput (Output a)

deriving stock instance Functor Output

instance Show (Output a) where
  show Output {outputId} = show outputId

instance Show SomeOutput where
  show (SomeOutput x) = show x

instance Identifiable (Output a) where
  identifier Output {outputId} = outputId

instance Identifiable SomeOutput where
  identifier (SomeOutput x) = identifier x

type Outputs = [SomeOutput]

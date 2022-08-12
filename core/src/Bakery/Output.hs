module Bakery.Output
  ( Output (..),
    Outputs,
    SomeOutput (..),
  )
where

import Bakery.Baking
import Bakery.Identifier
import Bakery.Input

-- We have to capture all this information because we cannot rely on a
-- 'Bakeable' constraint. Aside from 'Bakeable' not existing in this package,
-- constraints are lost in 'SomeOutput'.
data Output a = Output
  { outputId :: Id,
    outputTarget :: a,
    outputInputs :: Inputs,
    outputExists :: Baking Bool,
    outputAction :: Baking a
  }

data SomeOutput = forall a. SomeOutput (Output a)

deriving stock instance Functor Output

instance Show (Output a) where
  show Output {outputId} = show outputId

instance Show SomeOutput where
  show (SomeOutput x) = show x

type Outputs = [SomeOutput]

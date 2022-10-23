module Bakery.Shell.Operation
  ( type (#>) (..),
    StdIn (..),
    StdOut (..),
  )
where

import Bakery.Input (Inputs)
import Data.ByteString (ByteString)
import Pipes qualified
import Pipes.Safe (SafeT)

data i #> o = Operation Inputs (Pipes.Pipe i o (SafeT IO) ())

data StdIn = StdIn ByteString | StdInEnd
  deriving (Eq)

instance Semigroup StdIn where
  a <> StdInEnd = a
  StdInEnd <> b = b
  StdIn a <> StdIn b = StdIn (a <> b)

instance Monoid StdIn where
  mempty = StdInEnd

data StdOut = StdOut ByteString | StdOutEnd
  deriving (Eq)

instance Semigroup StdOut where
  a <> StdOutEnd = a
  StdOutEnd <> b = b
  StdOut a <> StdOut b = StdOut (a <> b)

instance Monoid StdOut where
  mempty = StdOutEnd

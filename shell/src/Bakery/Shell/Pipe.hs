module Bakery.Shell.Pipe
  ( StdIn (..),
    StdOut (..),
  )
where

import Data.ByteString (ByteString)

newtype StdIn = StdIn {unStdIn :: ByteString}
  deriving (Eq, Semigroup, Monoid)

newtype StdOut = StdOut {unStdOut :: ByteString}
  deriving (Eq, Semigroup, Monoid)

module Bakery.Shell.Pipe
  ( StdIn (..),
    StdOut (..),
  )
where

import Data.Text (Text)

newtype StdIn where
  StdIn :: Text -> StdIn

newtype StdOut where
  StdOut :: Text -> StdOut

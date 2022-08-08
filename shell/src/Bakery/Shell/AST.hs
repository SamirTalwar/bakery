module Bakery.Shell.AST (type (#>) (..)) where

import Bakery.Shell.Argument (Arg)
import Bakery.Shell.Path (InputPath, OutputPath)
import Bakery.Shell.Pipe (StdIn, StdOut)
import Data.List.NonEmpty (NonEmpty)

infixr 7 #>

data i #> o where
  NullStdIn :: () #> StdIn
  NullStdOut :: StdOut #> ()
  Run :: NonEmpty Arg -> StdIn #> StdOut
  Read :: InputPath -> () #> StdIn
  Write :: OutputPath -> StdOut #> ()
  Compose :: a #> b -> b #> c -> a #> c

deriving stock instance Show (i #> o)

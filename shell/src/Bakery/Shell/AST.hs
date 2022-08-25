module Bakery.Shell.AST (type (#>) (..), Shell (..)) where

import Bakery.Input (Inputs)
import Bakery.Shell.Argument (Arg)
import Bakery.Shell.Path (OutputPath)
import Bakery.Shell.Pipe (StdIn, StdOut)
import Data.List.NonEmpty (NonEmpty)

infixr 7 #>

data i #> o = Pipe Inputs (Shell i o)

data Shell i o where
  NullStdIn :: Shell () StdIn
  NullStdOut :: Shell StdOut ()
  Run :: NonEmpty Arg -> Shell StdIn StdOut
  Read :: FilePath -> Shell () StdIn
  Write :: OutputPath -> Shell StdOut ()
  Compose :: Shell a b -> Shell b c -> Shell a c

deriving stock instance Show (i #> o)

deriving stock instance Show (Shell i o)

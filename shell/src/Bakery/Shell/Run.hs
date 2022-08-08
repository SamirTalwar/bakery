module Bakery.Shell.Run (run) where

import Bakery.Shell.AST (type (#>) (..))
import Bakery.Shell.Argument (Arg, Argument (..))
import Bakery.Shell.Pipe (StdIn, StdOut)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

run :: (Argument a, RunType r) => a -> r
run arg = run' (NonEmpty.singleton (toArg arg))

class RunType r where
  run' :: NonEmpty Arg -> r

instance (Argument a, RunType r) => RunType (a -> r) where
  run' args arg = run' (toArg arg `NonEmpty.cons` args)

instance RunType (StdIn #> StdOut) where
  run' args = Run (NonEmpty.reverse args)

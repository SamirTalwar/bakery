module Bakery.Shell.Run (run) where

import Bakery.Input (Inputs)
import Bakery.Shell.AST (Shell (..), type (#>) (..))
import Bakery.Shell.Argument (Arg (..), Argument (..))
import Bakery.Shell.Pipe (StdIn, StdOut)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

run :: (Argument a, RunType r) => a -> r
run arg = run' (argInputs arg) (NonEmpty.singleton (toArg arg))

class RunType r where
  run' :: Inputs -> NonEmpty Arg -> r

instance (Argument a, RunType r) => RunType (a -> r) where
  run' inputs args arg = run' (argInputs arg ++ inputs) (toArg arg `NonEmpty.cons` args)

instance RunType (StdIn #> StdOut) where
  run' inputs args = Pipe (List.reverse inputs) $ Run (NonEmpty.reverse args)

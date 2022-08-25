module Bakery.Shell.Builder
  ( (|>),
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.AST (Shell (..), type (#>) (..))
import Bakery.Shell.Path (OutputPath, Path (..))
import Bakery.Shell.Pipe (StdIn, StdOut)

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Pipe aInputs a |> Pipe bInputs b =
  Pipe (aInputs <> bInputs) $ Compose a b

nullStdIn :: () #> StdIn
nullStdIn = Pipe [] NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = Pipe [] NullStdOut

readF :: (HasInputs a, Path a) => a -> () #> StdIn
readF input = Pipe (getInputs input) (Read (toPath input))

writeF :: OutputPath -> StdOut #> ()
writeF = Pipe [] . Write

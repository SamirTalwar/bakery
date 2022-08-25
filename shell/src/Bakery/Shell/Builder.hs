module Bakery.Shell.Builder
  ( (|>),
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
  )
where

import Bakery.Shell.AST (Shell (..), type (#>) (..))
import Bakery.Shell.Path (InputPath (..), OutputPath, Path (..))
import Bakery.Shell.Pipe (StdIn, StdOut)

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Pipe aInputs a |> Pipe bInputs b =
  Pipe (aInputs <> bInputs) $ Compose a b

nullStdIn :: () #> StdIn
nullStdIn = Pipe [] NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = Pipe [] NullStdOut

readF :: Path a => a -> () #> StdIn
readF input =
  let InputPath i path = toInputPath input
   in Pipe i (Read path)

writeF :: OutputPath -> StdOut #> ()
writeF = Pipe [] . Write

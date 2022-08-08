module Bakery.Shell.Builder
  ( (|>),
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
  )
where

import Bakery.Shell.AST (type (#>) (..))
import Bakery.Shell.Path (OutputPath, Path (..))
import Bakery.Shell.Pipe (StdIn, StdOut)

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
(|>) = Compose

nullStdIn :: () #> StdIn
nullStdIn = NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = NullStdOut

readF :: Path a => a -> () #> StdIn
readF = Read . toInputPath

writeF :: OutputPath -> StdOut #> ()
writeF = Write

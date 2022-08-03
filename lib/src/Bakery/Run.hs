{-# LANGUAGE UndecidableInstances #-}

module Bakery.Run
  ( -- * Construction
    nullStdIn,
    nullStdOut,
    write,
    run,
    (|>),

    -- * Evaluation
    type (#>),
    evaluateShell,
  )
where

import Bakery.Path
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Process.Typed qualified as Process

infixr 7 #>

data i #> o where
  NullStdIn :: () #> StdIn
  NullStdOut :: StdOut #> ()
  Run :: NonEmpty Arg -> StdIn #> StdOut
  Write :: Path -> StdOut #> ()
  Compose :: a #> b -> b #> c -> a #> c

deriving stock instance Show (i #> o)

nullStdIn :: () #> StdIn
nullStdIn = NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = NullStdOut

write :: Path -> StdOut #> ()
write = Write

data StdIn = StdIn Text

data StdOut = StdOut Text

class Argument a where
  toArg :: a -> Arg

instance Argument String where
  toArg = StringArg

data Arg where
  StringArg :: String -> Arg

deriving stock instance Show Arg

fromArg :: Arg -> String
fromArg (StringArg arg) = arg

class RunOutput r where
  run' :: NonEmpty Arg -> r

instance (Argument a, RunOutput r) => RunOutput (a -> r) where
  run' args arg = run' (toArg arg `NonEmpty.cons` args)

instance RunOutput (StdIn #> StdOut) where
  run' args = Run (NonEmpty.reverse args)

run :: (Argument a, RunOutput r) => a -> r
run arg = run' (NonEmpty.singleton (toArg arg))

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
(|>) = Compose

evaluateShell :: () #> () -> IO ()
evaluateShell program = evaluateShell' program ()

evaluateShell' :: a #> b -> a -> IO b
evaluateShell' NullStdIn () =
  pure $ StdIn Text.empty
evaluateShell' NullStdOut (StdOut _) =
  pure ()
evaluateShell' (Run (cmd :| args)) (StdIn stdin) =
  let config =
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg cmd) (map fromArg args)
   in StdOut <$> Process.withProcessWait config \process -> do
        Text.IO.hPutStr (Process.getStdin process) stdin
        Text.IO.hGetContents (Process.getStdout process)
evaluateShell' (Write (Path path)) (StdOut text) =
  Text.IO.writeFile (Text.unpack path) text
evaluateShell' (Compose a b) i =
  evaluateShell' a i >>= evaluateShell' b

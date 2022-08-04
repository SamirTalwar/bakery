{-# LANGUAGE UndecidableInstances #-}

module Bakery.Run
  ( -- * Construction
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
    run,
    (|>),

    -- * Evaluation
    type (#>),
    evaluateShell,
  )
where

import Bakery.Bakeable (InShell (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.IO.Handle (hClose)
import System.Process.Typed qualified as Process

infixr 7 #>

data i #> o where
  NullStdIn :: () #> StdIn
  NullStdOut :: StdOut #> ()
  Run :: NonEmpty Arg -> StdIn #> StdOut
  Read :: FilePath -> () #> StdIn
  Write :: FilePath -> StdOut #> ()
  Compose :: a #> b -> b #> c -> a #> c

deriving stock instance Show (i #> o)

nullStdIn :: () #> StdIn
nullStdIn = NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = NullStdOut

readF :: InShell a => a -> () #> StdIn
readF = Read . inShell

writeF :: InShell a => a -> StdOut #> ()
writeF = Write . inShell

data StdIn = StdIn Text

data StdOut = StdOut Text

class Argument a where
  toArg :: a -> Arg

instance {-# OVERLAPPING #-} Argument String where
  toArg = Arg

instance {-# OVERLAPPABLE #-} InShell a => Argument a where
  toArg = Arg . inShell

newtype Arg = Arg {fromArg :: String}
  deriving newtype (Show)

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
        let hStdin = Process.getStdin process
            hStdout = Process.getStdout process
        Text.IO.hPutStr hStdin stdin
        hClose hStdin
        Text.IO.hGetContents hStdout
evaluateShell' (Read path) () =
  StdIn <$> Text.IO.readFile path
evaluateShell' (Write path) (StdOut text) =
  Text.IO.writeFile path text
evaluateShell' (Compose a b) i =
  evaluateShell' a i >>= evaluateShell' b

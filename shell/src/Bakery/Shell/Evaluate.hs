module Bakery.Shell.Evaluate (evaluate) where

import Bakery.Shell.AST (Shell (..), type (#>) (..))
import Bakery.Shell.Argument (fromArg)
import Bakery.Shell.Path (OutputPath (..), unknownOutputPathFailure)
import Bakery.Shell.Pipe (StdIn (..), StdOut (..))
import Control.Exception (catch)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.IO.Handle (hClose)
import System.Process.Typed qualified as Process

-- The argument order is important, so that 'a' is constrained by 'a #> b'.
evaluate :: i #> o -> i -> IO o
evaluate (Pipe _ shell) = evaluateShell shell

evaluateShell :: Shell i o -> i -> IO o
evaluateShell NullStdIn () =
  pure $ StdIn Text.empty
evaluateShell NullStdOut (StdOut _) =
  pure ()
evaluateShell (Run (cmd :| args)) (StdIn stdin) =
  let config =
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg cmd) (map fromArg args)
   in StdOut
        <$> ( Process.withProcessWait_ config \process ->
                do
                  let hStdin = Process.getStdin process
                      hStdout = Process.getStdout process
                  Text.IO.hPutStr hStdin stdin
                  hClose hStdin
                  Text.IO.hGetContents hStdout
            )
        `catch` \(Process.ExitCodeException exitCode _ _ _) ->
          fail $ "The command failed with exit code " <> show exitCode <> "."
evaluateShell (Read path) () =
  StdIn <$> Text.IO.readFile path
evaluateShell (Write (KnownOutputPath path)) (StdOut text) =
  Text.IO.writeFile path text
evaluateShell (Write UnknownOutputPath) (StdOut _) =
  fail unknownOutputPathFailure
evaluateShell (Compose a b) i =
  evaluateShell a i >>= evaluateShell b

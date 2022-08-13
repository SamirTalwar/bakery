module Bakery.Shell.Evaluate (evaluate) where

import Bakery.Shell.AST (type (#>) (..))
import Bakery.Shell.Argument (fromArg)
import Bakery.Shell.Path (InputPath (..), OutputPath (..), unknownOutputPathFailure)
import Bakery.Shell.Pipe (StdIn (..), StdOut (..))
import Control.Exception (catch)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.IO.Handle (hClose)
import System.Process.Typed qualified as Process

-- The argument order is important, so that 'a' is constrained by 'a #> b'.
evaluate :: a #> b -> a -> IO b
evaluate NullStdIn () =
  pure $ StdIn Text.empty
evaluate NullStdOut (StdOut _) =
  pure ()
evaluate (Run (cmd :| args)) (StdIn stdin) =
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
evaluate (Read (InputPath _ path)) () =
  StdIn <$> Text.IO.readFile path
evaluate (Write (KnownOutputPath path)) (StdOut text) =
  Text.IO.writeFile path text
evaluate (Write UnknownOutputPath) (StdOut _) =
  fail unknownOutputPathFailure
evaluate (Compose a b) i =
  evaluate a i >>= evaluate b

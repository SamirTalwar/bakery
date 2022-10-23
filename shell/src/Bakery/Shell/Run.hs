module Bakery.Shell.Run (run) where

import Bakery.Input (HasInputs (..), Inputs)
import Bakery.Shell.Argument (Arg (..), Argument (..), fromArg)
import Bakery.Shell.Builder (nullStdIn, nullStdOut, (|>))
import Bakery.Shell.Chunk
import Bakery.Shell.Operation (type (#>) (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void)
import GHC.IO.Handle (hClose)
import Pipes
import Pipes.ByteString qualified
import Pipes.Safe (bracket)
import System.Exit (ExitCode (..))
import System.Process.Typed qualified as Process

run :: (Argument a, HasInputs a, RunType r) => a -> r
run arg = run' (getInputs arg) (NonEmpty.singleton (toArg arg))

class RunType r where
  run' :: Inputs -> NonEmpty Arg -> r

instance (Argument a, HasInputs a, RunType r) => RunType (a -> r) where
  run' inputs reversedArgs arg = run' (inputs <> getInputs arg) (toArg arg `NonEmpty.cons` reversedArgs)

instance RunType (() #> Void) where
  run' inputs reversedArgs = nullStdIn |> run' inputs reversedArgs |> nullStdOut

instance RunType (Chunk ByteString #> Void) where
  run' inputs reversedArgs = run' inputs reversedArgs |> nullStdOut

instance RunType (() #> Chunk ByteString) where
  run' inputs reversedArgs = nullStdIn |> run' inputs reversedArgs

instance RunType (Chunk ByteString #> Chunk ByteString) where
  run' inputs reversedArgs =
    Operation inputs $ bracket
      (Process.startProcess config)
      ( \process -> do
          hClose $ Process.getStdin process
          hClose $ Process.getStdout process
          exitCode <- Process.waitExitCode process
          case exitCode of
            ExitSuccess -> pure ()
            ExitFailure (-13) -> pure () -- ignore SIGPIPE errors
            ExitFailure code -> fail $ "The command failed with exit code " <> show code <> "."
      )
      \process -> do
        let stdinHandle = Process.getStdin process
        consume (liftIO . ByteString.hPut stdinHandle) (liftIO (hClose stdinHandle))
        capped (Pipes.ByteString.fromHandle (Process.getStdout process))
    where
      cmd :| args = NonEmpty.reverse reversedArgs
      config =
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg cmd) (map fromArg args)

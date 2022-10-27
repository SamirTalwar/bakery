module Bakery.Shell.Run
  ( run,
    run_,
    cmd,
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Bakery.Shell.Argument (Arg (..), Argument (..), fromArg)
import Bakery.Shell.Chunk
import Bakery.Shell.Operation ((|>), type (#>) (..))
import Bakery.Shell.Prelude (nullStdIn, nullStdOut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import Data.Void (Void)
import GHC.IO.Handle (hClose)
import Pipes
import Pipes.ByteString qualified
import Pipes.Safe (bracket)
import System.Exit (ExitCode (..))
import System.Process.Typed qualified as Process

-- | Constructs a "run" operation, which invokes a command, reading the input as
-- STDIN and writing STDOUT to the output.
run :: Arguments -> Chunk ByteString #> Chunk ByteString
run (Arguments inputs command args) = runOperation inputs command args

data Arguments = Arguments Inputs Arg [Arg]

-- | Constructs a command to be 'run'.
--
-- This function is variadic; you can keep providing arguments until you are
-- done, and they can be of different types. For example:
--
-- > run (cmd "seq" 1 10)
cmd :: (Argument a, HasInputs a, CmdType r) => a -> r
cmd command = cmdConstruct (getInputs command) (toArg command) []

class CmdType r where
  cmdConstruct :: Inputs -> Arg -> [Arg] -> r

instance (Argument a, HasInputs a, CmdType r) => CmdType (a -> r) where
  cmdConstruct inputs command reversedArgs arg = cmdConstruct (inputs <> getInputs arg) command (toArg arg : reversedArgs)

instance CmdType Arguments where
  cmdConstruct inputs command reversedArgs = Arguments inputs command (List.reverse reversedArgs)

-- | A variation of 'run' which provides null input, discards output, or both.
--
-- This function is variadic; you can keep providing arguments until you are
-- done, and they can be of different types. For example:
--
-- > run_ "seq" 1 10
--
-- This is separate from 'run' so as to reduce the chance of spurious ambiguity
-- errors. This version is best used standalone; 'run' is better for chaining.
run_ :: (Argument a, HasInputs a, RunType r) => a -> r
run_ arg = runConstruct (getInputs arg) (toArg arg) []

class RunType r where
  runConstruct :: Inputs -> Arg -> [Arg] -> r

instance (Argument a, HasInputs a, RunType r) => RunType (a -> r) where
  runConstruct inputs command reversedArgs arg = runConstruct (inputs <> getInputs arg) command (toArg arg : reversedArgs)

instance RunType (() #> Void) where
  runConstruct inputs command reversedArgs = nullStdIn |> runConstruct inputs command reversedArgs |> nullStdOut

instance RunType (Chunk ByteString #> Void) where
  runConstruct inputs command reversedArgs = runConstruct inputs command reversedArgs |> nullStdOut

instance RunType (() #> Chunk ByteString) where
  runConstruct inputs command reversedArgs = nullStdIn |> runConstruct inputs command reversedArgs

instance RunType (Chunk ByteString #> Chunk ByteString) where
  runConstruct inputs command reversedArgs = runOperation inputs command (List.reverse reversedArgs)

runOperation :: Inputs -> Arg -> [Arg] -> Chunk ByteString #> Chunk ByteString
runOperation inputs command args = Operation inputs $ bracket start stop stream
  where
    start =
      Process.startProcess $
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg command) (map fromArg args)
    stop process = do
      hClose $ Process.getStdin process
      hClose $ Process.getStdout process
      exitCode <- Process.waitExitCode process
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure (-13) -> pure () -- ignore SIGPIPE errors
        ExitFailure code -> fail $ "The command failed with exit code " <> show code <> "."
    stream process = do
      let stdinHandle = Process.getStdin process
      consume (liftIO . ByteString.hPut stdinHandle) (liftIO (hClose stdinHandle))
      capped (Pipes.ByteString.fromHandle (Process.getStdout process))

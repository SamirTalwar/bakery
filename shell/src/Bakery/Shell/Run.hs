module Bakery.Shell.Run
  ( run,
    cmd,
    (~),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Bakery.Shell.Argument (Arg (..), Argument (..), fromArg)
import Bakery.Shell.Chunk
import Bakery.Shell.Shell (Shell)
import Bakery.Shell.Shell qualified as Shell
import Bakery.Shell.TrackingInputs (registerInputs)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import GHC.IO.Handle (hClose)
import Pipes.ByteString qualified
import Pipes.Safe (bracket)
import System.Exit (ExitCode (..))
import System.Process.Typed qualified as Process

-- | Constructs a "run" operation, which invokes a command, reading the input as
-- STDIN and writing STDOUT to the output.
run :: (MonadMask m, MonadIO m) => Command m -> Shell m (Chunk ByteString) (Chunk ByteString) ()
run (Command inputs commandM reversedArgsM) = do
  registerInputs inputs
  command <- fromArg <$> Shell.lift unknownArg commandM
  args <- map fromArg <$> mapM (Shell.lift unknownArg) (List.reverse reversedArgsM)
  let start = liftIO do
        Process.startProcess $
          Process.setStdin Process.createPipe $
            Process.setStdout Process.createPipe $
              Process.proc command args
      stop process = liftIO do
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
  Shell.fromPipe (bracket start stop stream)

data Command m = Command Inputs (m Arg) [m Arg]

-- | Constructs a command to be 'run'.
cmd :: (Argument m a, HasInputs a) => a -> Command m
cmd command = Command (getInputs command) (toArg command) []

infixl 7 ~

-- | Adds an argument to a command.
(~) :: (Argument m a, HasInputs a) => Command m -> a -> Command m
Command inputs command reversedArgs ~ arg = Command (inputs <> getInputs arg) command (toArg arg : reversedArgs)

unknownArg :: Arg
unknownArg = ErrorArg "Tried to process an argument during input tracking."

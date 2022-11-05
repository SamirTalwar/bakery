module Bakery.Shell.Prelude.File
  ( readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.Chunk
import Bakery.Shell.Path
import Bakery.Shell.Shell
import Bakery.Shell.TrackingInputs (registerInput)
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Void (Void)
import Pipes.ByteString qualified
import Pipes.Safe.Prelude qualified as Pipes.Safe
import System.IO (IOMode (..), hClose)

-- | An operation that reads the given file and emits its contents.
readF :: (MonadMask m, MonadIO m, HasInputs a, Path a) => a -> Shell m () (Chunk ByteString) ()
readF input = do
  registerInput input
  fromPipe $ capped (Pipes.Safe.withFile (toPath input) ReadMode Pipes.ByteString.fromHandle)

-- | An operation that writes its input to the given file.
writeF :: (MonadMask m, MonadIO m) => OutputPath -> Shell m (Chunk ByteString) Void ()
writeF (KnownOutputPath path) =
  fromPipe $ Pipes.Safe.withFile path WriteMode \handle ->
    consume
      (liftIO . ByteString.hPut handle)
      (liftIO $ hClose handle)
writeF UnknownOutputPath =
  fromPipe $ liftIO $ throwIO UnknownOutputPathException

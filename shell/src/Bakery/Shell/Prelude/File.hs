module Bakery.Shell.Prelude.File
  ( readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.Chunk
import Bakery.Shell.Path (Path (..))
import Bakery.Shell.Shell (Shell)
import Bakery.Shell.Shell qualified as Shell
import Bakery.Shell.TrackingInputs (registerInput)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Void (Void)
import Pipes.ByteString qualified
import Pipes.Safe.Prelude qualified as Pipes.Safe
import System.IO (IOMode (..), hClose)

-- | An operation that reads the given file and emits its contents.
readF :: (MonadMask m, MonadIO m, HasInputs a, Path m a) => a -> Shell m () (Chunk ByteString) ()
readF inputPath = do
  registerInput inputPath
  path <- Shell.lift "" $ toPath inputPath
  Shell.fromPipe $ capped (Pipes.Safe.withFile path ReadMode Pipes.ByteString.fromHandle)

-- | An operation that writes its input to the given file.
writeF :: (MonadMask m, MonadIO m, Path m a) => a -> Shell m (Chunk ByteString) Void ()
writeF outputPath = do
  path <- Shell.lift "" $ toPath outputPath
  Shell.fromPipe $ Pipes.Safe.withFile path WriteMode \handle ->
    consume
      (liftIO . ByteString.hPut handle)
      (liftIO $ hClose handle)

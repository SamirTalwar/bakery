module Bakery.Shell.Prelude.File
  ( readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.Chunk
import Bakery.Shell.Operation (Operation (..), type (#>))
import Bakery.Shell.Path (OutputPath (..), Path (..), unknownOutputPathFailure)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Void (Void)
import Pipes.ByteString qualified
import Pipes.Safe.Prelude qualified as Pipes.Safe
import System.IO (IOMode (..), hClose)

-- | An operation that reads the given file and emits its contents.
readF :: (HasInputs a, Path a) => a -> () #> Chunk ByteString
readF input =
  Operation (getInputs input) $
    capped (Pipes.Safe.withFile (toPath input) ReadMode Pipes.ByteString.fromHandle)

-- | An operation that writes its input to the given file.
writeF :: OutputPath -> Chunk ByteString #> Void
writeF (KnownOutputPath path) =
  Operation [] $ Pipes.Safe.withFile path WriteMode \handle ->
    consume
      (liftIO . ByteString.hPut handle)
      (liftIO $ hClose handle)
writeF UnknownOutputPath =
  Operation [] $ fail unknownOutputPathFailure

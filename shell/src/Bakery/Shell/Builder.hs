module Bakery.Shell.Builder
  ( (|>),
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.Chunk
import Bakery.Shell.Operation (type (#>) (..))
import Bakery.Shell.Path (OutputPath (..), Path (..), unknownOutputPathFailure)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Void (Void)
import Pipes ((>->))
import Pipes.ByteString qualified
import Pipes.Prelude qualified as P
import Pipes.Safe.Prelude qualified as Pipes.Safe
import System.IO (IOMode (..), hClose)

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

nullStdIn :: () #> Chunk ByteString
nullStdIn = Operation [] $ capped (pure ())

nullStdOut :: Chunk ByteString #> Void
nullStdOut = Operation [] P.drain

readF :: (HasInputs a, Path a) => a -> () #> Chunk ByteString
readF input =
  Operation (getInputs input) $
    capped (Pipes.Safe.withFile (toPath input) ReadMode Pipes.ByteString.fromHandle)

writeF :: OutputPath -> Chunk ByteString #> Void
writeF (KnownOutputPath path) =
  Operation [] $ Pipes.Safe.withFile path WriteMode \handle ->
    consume
      (liftIO . ByteString.hPut handle)
      (liftIO $ hClose handle)
writeF UnknownOutputPath =
  Operation [] $ fail unknownOutputPathFailure

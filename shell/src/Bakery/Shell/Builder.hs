module Bakery.Shell.Builder
  ( (|>),
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
  )
where

import Bakery.Input (HasInputs (..))
import Bakery.Shell.Operation (StdIn (..), StdOut (..), type (#>) (..))
import Bakery.Shell.Path (OutputPath (..), Path (..), unknownOutputPathFailure)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Pipes (cat, for, yield, (>->))
import Pipes.ByteString qualified
import Pipes.Prelude qualified as P
import Pipes.Safe.Prelude qualified as Pipes.Safe
import System.IO (IOMode (..), hClose)

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

nullStdIn :: () #> StdIn
nullStdIn = Operation [] $ yield StdInEnd

nullStdOut :: StdOut #> ()
nullStdOut = Operation [] P.drain

readF :: (HasInputs a, Path a) => a -> () #> StdIn
readF input =
  Operation (getInputs input) $
    (Pipes.Safe.withFile (toPath input) ReadMode Pipes.ByteString.fromHandle >-> P.map StdIn)
      <> yield StdInEnd

writeF :: OutputPath -> StdOut #> ()
writeF (KnownOutputPath path) =
  Operation [] $ Pipes.Safe.withFile path WriteMode \handle ->
    for cat \case
      StdOut bytes -> liftIO $ ByteString.hPut handle bytes
      StdOutEnd -> liftIO $ hClose handle
writeF UnknownOutputPath =
  Operation [] $ fail unknownOutputPathFailure

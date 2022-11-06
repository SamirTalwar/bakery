module Bakery.Shell.Prelude.Null
  ( nullStdIn,
    nullStdOut,
    NullIO (..),
  )
where

import Bakery.Shell.Chunk
import Bakery.Shell.Shell (Shell, (|>))
import Bakery.Shell.Shell qualified as Shell
import Data.Void (Void)
import Pipes.Prelude qualified as P

-- | An operation that produces an empty stream of bytes.
nullStdIn :: Functor m => Shell m () (Chunk o) ()
nullStdIn = Shell.fromPipe $ capped (pure ())

-- | An operation that discards all inputs.
nullStdOut :: Functor m => Shell m i Void ()
nullStdOut = Shell.fromPipe P.drain

-- | Use 'n' to produce an empty input and discard all output from any compatible operation.
class NullIO m i o i' o' where
  n :: Shell m i o () -> Shell m i' o' ()

instance Monad m => NullIO m i o i Void where
  n pipe = pipe |> nullStdOut

instance Monad m => NullIO m (Chunk i) o () Void where
  n pipe = nullStdIn |> pipe |> nullStdOut

instance Monad m => NullIO m (Chunk i) (Chunk o) () (Chunk o) where
  n pipe = nullStdIn |> pipe

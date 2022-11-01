module Bakery.Shell.Prelude.Null
  ( nullStdIn,
    nullStdOut,
  )
where

import Bakery.Shell.Chunk
import Bakery.Shell.Pipe
import Data.ByteString (ByteString)
import Data.Void (Void)
import Pipes.Prelude qualified as P

-- | An operation that produces an empty stream of bytes.
nullStdIn :: () #> Chunk ByteString
nullStdIn = fromPipe $ capped (pure ())

-- | An operation that discards all inputs.
nullStdOut :: Chunk ByteString #> Void
nullStdOut = fromPipe P.drain

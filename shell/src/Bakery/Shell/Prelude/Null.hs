module Bakery.Shell.Prelude.Null
  ( nullStdIn,
    nullStdOut,
  )
where

import Bakery.Shell.Chunk
import Bakery.Shell.Operation (type (#>))
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Pipes.Prelude qualified as P

-- | An operation that produces an empty stream of bytes.
nullStdIn :: () #> Chunk ByteString
nullStdIn = lift $ capped (pure ())

-- | An operation that discards all inputs.
nullStdOut :: Chunk ByteString #> Void
nullStdOut = lift P.drain

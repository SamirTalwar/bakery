module Bakery.Shell.Prelude.Null
  ( nullStdIn,
    nullStdOut,
  )
where

import Bakery.Shell.Chunk
import Bakery.Shell.Operation (type (#>) (..))
import Data.ByteString (ByteString)
import Data.Void (Void)
import Pipes.Prelude qualified as P

nullStdIn :: () #> Chunk ByteString
nullStdIn = Operation [] $ capped (pure ())

nullStdOut :: Chunk ByteString #> Void
nullStdOut = Operation [] P.drain

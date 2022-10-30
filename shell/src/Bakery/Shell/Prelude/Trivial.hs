module Bakery.Shell.Prelude.Trivial
  ( cat,
    empty,
    each,
    capped,
  )
where

import Bakery.Shell.Chunk (Chunk)
import Bakery.Shell.Chunk qualified as Chunk
import Bakery.Shell.Operation
import Control.Monad.Trans (lift)
import Pipes qualified as P

-- | An operation that simply copies values from input to output.
cat :: a #> a
cat = lift P.cat

-- | An operation that discards all values and never emits anything.
empty :: a #> b
empty = lift $ pure ()

-- | An operation derived from a Foldable.
each :: Foldable f => f a -> () #> a
each = lift . P.each

-- | Wraps operations in `Chunk`.
capped :: a #> b -> a #> Chunk b
capped (Operation inputs pipe) = Operation inputs (Chunk.capped pipe)

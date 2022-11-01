module Bakery.Shell.Prelude.Trivial
  ( cat,
    empty,
    each,
  )
where

import Bakery.Shell.Pipe
import Pipes qualified as P

-- | An operation that simply copies values from input to output.
cat :: a #> a
cat = fromPipe P.cat

-- | An operation that discards all values and never emits anything.
empty :: a #> b
empty = fromPipe $ pure ()

-- | An operation derived from a Foldable.
each :: Foldable f => f a -> () #> a
each = fromPipe . P.each

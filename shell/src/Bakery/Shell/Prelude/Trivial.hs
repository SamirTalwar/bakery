module Bakery.Shell.Prelude.Trivial
  ( cat,
    empty,
    each,
  )
where

import Bakery.Shell.Shell
import Pipes qualified as P

-- | An operation that simply copies values from input to output.
cat :: Functor m => Shell m a a ()
cat = fromPipe P.cat

-- | An operation that discards all values and never emits anything.
empty :: Functor m => Shell m a b ()
empty = fromPipe $ pure ()

-- | An operation derived from a Foldable.
each :: Functor m => Foldable f => f a -> Shell m () a ()
each = fromPipe . P.each

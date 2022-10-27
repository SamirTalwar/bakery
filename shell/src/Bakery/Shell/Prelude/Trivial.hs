module Bakery.Shell.Prelude.Trivial
  ( cat,
    empty,
  )
where

import Bakery.Shell.Operation
import Pipes qualified as P

-- | An operation that simply copies values from input to output.
cat :: a #> a
cat = Operation [] P.cat

-- | An operation that discards all values and never emits anything.
empty :: a #> b
empty = Operation [] $ pure ()

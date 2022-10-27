module Bakery.Shell.Prelude.Trivial
  ( cat,
    empty,
  )
where

import Bakery.Shell.Operation
import Pipes qualified as P

cat :: a #> a
cat = Operation [] P.cat

empty :: a #> b
empty = Operation [] $ pure ()

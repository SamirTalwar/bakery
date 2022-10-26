module Bakery.Shell.Prelude
  ( cat,
  )
where

import Bakery.Shell.Operation
import Pipes qualified as P

cat :: a #> a
cat = Operation [] P.cat

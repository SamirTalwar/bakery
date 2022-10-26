module Bakery.Shell.Prelude
  ( cat,
    filter,
    map,
  )
where

import Bakery.Shell.Operation
import Pipes qualified as P
import Pipes.Prelude qualified as P
import Prelude hiding (filter, map)

cat :: a #> a
cat = Operation [] P.cat

map :: (a -> b) -> a #> b
map = Operation [] . P.map

filter :: (a -> Bool) -> a #> a
filter = Operation [] . P.filter

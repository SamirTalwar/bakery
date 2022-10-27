module Bakery.Shell.Prelude.Transformation
  ( filter,
    map,
  )
where

import Bakery.Shell.Operation
import Pipes.Prelude qualified as P
import Prelude hiding (filter, map)

map :: (a -> b) -> a #> b
map = Operation [] . P.map

filter :: (a -> Bool) -> a #> a
filter = Operation [] . P.filter

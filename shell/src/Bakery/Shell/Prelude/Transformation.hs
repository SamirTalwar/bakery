module Bakery.Shell.Prelude.Transformation
  ( filter,
    map,
    drop,
    dropWhile,
    take,
    takeWhile,
  )
where

import Bakery.Shell.Operation
import Pipes.Prelude qualified as P
import Prelude hiding (drop, dropWhile, filter, map, take, takeWhile)

map :: (a -> b) -> a #> b
map = Operation [] . P.map

filter :: (a -> Bool) -> a #> a
filter = Operation [] . P.filter

drop :: Int -> a #> a
drop = Operation [] . P.drop

dropWhile :: (a -> Bool) -> a #> a
dropWhile = Operation [] . P.dropWhile

take :: Int -> a #> a
take = Operation [] . P.take

takeWhile :: (a -> Bool) -> a #> a
takeWhile = Operation [] . P.takeWhile

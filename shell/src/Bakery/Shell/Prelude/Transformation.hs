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
import Bakery.Shell.Prelude.Trivial
import Pipes.Prelude qualified as P
import Prelude hiding (drop, dropWhile, filter, map, take, takeWhile)

map :: (a -> b) -> a #> b
map = Operation [] . P.map

filter :: (a -> Bool) -> a #> a
filter = Operation [] . P.filter

drop :: Int -> a #> a
drop n
  | n <= 0 = cat
  | otherwise = Operation [] $ P.drop n

dropWhile :: (a -> Bool) -> a #> a
dropWhile = Operation [] . P.dropWhile

take :: Int -> a #> a
take n
  | n <= 0 = empty
  | otherwise = Operation [] $ P.take n

takeWhile :: (a -> Bool) -> a #> a
takeWhile = Operation [] . P.takeWhile

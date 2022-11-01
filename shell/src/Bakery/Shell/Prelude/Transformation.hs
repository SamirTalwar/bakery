module Bakery.Shell.Prelude.Transformation
  ( filter,
    map,
    drop,
    dropWhile,
    take,
    takeWhile,
  )
where

import Bakery.Shell.Pipe
import Bakery.Shell.Prelude.Trivial
import Pipes.Prelude qualified as P
import Prelude hiding (drop, dropWhile, filter, map, take, takeWhile)

-- | An operation that transforms values according to the given function.
map :: (a -> b) -> a #> b
map = fromPipe . P.map

-- | An operation that filters values according to the given predicate.
filter :: (a -> Bool) -> a #> a
filter = fromPipe . P.filter

-- | An operation that drops the given number of values.
drop :: Int -> a #> a
drop n
  | n <= 0 = cat
  | otherwise = fromPipe $ P.drop n

-- | An operation that drops values until the given predicate is not met.
dropWhile :: (a -> Bool) -> a #> a
dropWhile = fromPipe . P.dropWhile

-- | An operation that takes the given number of values.
take :: Int -> a #> a
take n
  | n <= 0 = empty
  | otherwise = fromPipe $ P.take n

-- | An operation that takes values until the given predicate is not met.
takeWhile :: (a -> Bool) -> a #> a
takeWhile = fromPipe . P.takeWhile

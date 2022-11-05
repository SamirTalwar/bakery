module Bakery.Shell.Prelude.Transformation
  ( filter,
    map,
    drop,
    dropWhile,
    take,
    takeWhile,
  )
where

import Bakery.Shell.Prelude.Trivial
import Bakery.Shell.Shell
import Pipes.Prelude qualified as P
import Prelude hiding (drop, dropWhile, filter, map, take, takeWhile)

-- | An operation that transforms values according to the given function.
map :: Functor m => (a -> b) -> Shell m a b ()
map = fromPipe . P.map

-- | An operation that filters values according to the given predicate.
filter :: Functor m => (a -> Bool) -> Shell m a a ()
filter = fromPipe . P.filter

-- | An operation that drops the given number of values.
drop :: Functor m => Int -> Shell m a a ()
drop n
  | n <= 0 = cat
  | otherwise = fromPipe $ P.drop n

-- | An operation that drops values until the given predicate is not met.
dropWhile :: Functor m => (a -> Bool) -> Shell m a a ()
dropWhile = fromPipe . P.dropWhile

-- | An operation that takes the given number of values.
take :: Functor m => Int -> Shell m a a ()
take n
  | n <= 0 = empty
  | otherwise = fromPipe $ P.take n

-- | An operation that takes values until the given predicate is not met.
takeWhile :: Functor m => (a -> Bool) -> Shell m a a ()
takeWhile = fromPipe . P.takeWhile

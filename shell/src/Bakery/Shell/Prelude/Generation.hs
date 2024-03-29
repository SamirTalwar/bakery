module Bakery.Shell.Prelude.Generation
  ( repeat,
    replicate,
  )
where

import Bakery.Shell.Shell
import Pipes qualified
import Prelude hiding (cycle, repeat, replicate)

-- | An operation that repeats the given value indefinitely.
repeat :: Functor m => a -> Shell m () a ()
repeat = fromPipe . pipeRepeat
  where
    pipeRepeat value = Pipes.yield value >> pipeRepeat value

-- | An operation that repeats the given value a number of times.
replicate :: Functor m => Int -> a -> Shell m () a ()
replicate n' = fromPipe . pipeReplicate n'
  where
    pipeReplicate n value
      | n <= 0 = pure ()
      | otherwise =
          Pipes.yield value >> pipeReplicate (n - 1) value

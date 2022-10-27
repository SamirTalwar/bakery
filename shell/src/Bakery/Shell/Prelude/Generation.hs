module Bakery.Shell.Prelude.Generation
  ( repeat,
    replicate,
  )
where

import Bakery.Shell.Operation
import Pipes qualified
import Prelude hiding (cycle, repeat, replicate)

repeat :: a -> () #> a
repeat = Operation [] . pipeRepeat
  where
    pipeRepeat value = Pipes.yield value >> pipeRepeat value

replicate :: Int -> a -> () #> a
replicate n' = Operation [] . pipeReplicate n'
  where
    pipeReplicate n value
      | n <= 0 = pure ()
      | otherwise =
          Pipes.yield value >> pipeReplicate (n - 1) value

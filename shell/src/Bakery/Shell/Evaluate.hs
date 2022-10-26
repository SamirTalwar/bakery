module Bakery.Shell.Evaluate
  ( evaluate,
    evaluate_,
  )
where

import Bakery.Shell.Operation (type (#>) (..))
import Control.Monad (void)
import Data.Void (Void)
import Pipes (each, (>->))
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)

evaluate :: i #> o -> [i] -> IO [o]
evaluate (Operation _ pipe) values = runSafeT (toListM (each values >-> pipe))

evaluate_ :: () #> Void -> IO ()
evaluate_ operation = void $ evaluate operation [()]

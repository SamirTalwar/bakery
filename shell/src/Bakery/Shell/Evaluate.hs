module Bakery.Shell.Evaluate
  ( evaluate,
    evaluate_,
  )
where

import Bakery.Shell.Operation (type (#>) (..))
import Data.Void (Void)
import Pipes (runEffect, yield, (>->))
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)

evaluate :: Monoid o => () #> o -> IO o
evaluate (Operation _ pipe) = mconcat <$> runSafeT (toListM (yield () >-> pipe))

evaluate_ :: () #> Void -> IO ()
evaluate_ (Operation _ pipe) = runSafeT (runEffect (yield () >-> pipe))

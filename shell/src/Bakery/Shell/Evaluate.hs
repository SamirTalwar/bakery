module Bakery.Shell.Evaluate (evaluate) where

import Bakery.Shell.Operation (type (#>) (..))
import Pipes (yield, (>->))
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)

evaluate :: Monoid o => () #> o -> IO o
evaluate (Operation _ pipe) = mconcat <$> runSafeT (toListM (yield () >-> pipe))

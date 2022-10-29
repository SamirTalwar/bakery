module Bakery.Shell.Evaluate
  ( evaluate,
    evaluate_,
  )
where

import Bakery.Shell.Operation (Operation (..))
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Pipes (each, (>->))
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)

evaluate :: (MonadMask m, MonadIO m) => Operation i o m () -> [i] -> m [o]
evaluate (Operation _ pipe) values = runSafeT (toListM (each values >-> pipe))

evaluate_ :: (MonadMask m, MonadIO m) => Operation () Void m () -> m ()
evaluate_ operation = void $ evaluate operation [()]

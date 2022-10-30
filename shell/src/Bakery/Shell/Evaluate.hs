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
import Pipes (Pipe, each, (>->))
import Pipes.Prelude (toListM)
import Pipes.Safe (SafeT, runSafeT)

evaluate :: (MonadMask m, MonadIO m) => Operation (Pipe i o (SafeT m)) () -> [i] -> m [o]
evaluate (Operation _ pipe) values = runSafeT $ toListM (each values >-> pipe)

evaluate_ :: (MonadMask m, MonadIO m) => Operation (Pipe () Void (SafeT m)) () -> m ()
evaluate_ operation = void $ evaluate operation [()]

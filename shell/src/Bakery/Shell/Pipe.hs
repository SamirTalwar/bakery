module Bakery.Shell.Pipe
  ( type (#>),
    fromPipe,
    (|>),
    (<|),
    evaluate,
    evaluate_,
  )
where

import Bakery.Shell.TrackingInputs
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Pipes (Pipe, (>->))
import Pipes qualified
import Pipes.Prelude qualified as Pipes
import Pipes.Safe (SafeT, runSafeT)

-- | Represents a streaming operation from 'i' to 'o'.
type i #> o = TrackingInputs (Pipe i o (SafeT IO)) ()

-- | Lifts a 'Pipe' into the '(#>)' type.
fromPipe :: Pipe i o (SafeT IO) () -> i #> o
fromPipe = liftWithFakeValue ()

infixr 5 |>

-- | Composes two pipes, left to right.
(|>) :: a #> b -> b #> c -> a #> c
(|>) = combineInputs (<>) (>->)

infixl 5 <|

-- | Composes two pipes, right to left.
(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

-- | Given a sequence of inputs, evaluates a streaming operation and collects the outputs as a list.
evaluate :: (MonadMask m, MonadIO m) => TrackingInputs (Pipe i o (SafeT m)) () -> [i] -> m [o]
evaluate operation values = runSafeT $ Pipes.toListM (Pipes.each values >-> withoutInputs operation)

-- | Evaluates a streaming operation with no inputs or outputs, only effects.
evaluate_ :: (MonadMask m, MonadIO m) => TrackingInputs (Pipe () Void (SafeT m)) () -> m ()
evaluate_ operation = void $ evaluate operation [()]

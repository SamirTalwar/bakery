module Bakery.Shell.Shell
  ( Shell (..),
    fromPipe,
    (|>),
    (<|),
    evaluate,
    evaluate_,
  )
where

import Bakery.Input (HasInputs)
import Bakery.Shell.TrackingInputs
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Void (Void)
import Pipes (Pipe, (>->))
import Pipes qualified
import Pipes.Prelude qualified as Pipes
import Pipes.Safe (SafeT, runSafeT)

-- | A shell is a streaming operation from 'i' to 'o', with effects from 'm'.
newtype Shell m i o r = Shell {unShell :: TrackingInputs (Pipe i o (SafeT m)) r}
  deriving newtype (Functor, Applicative, Monad, MonadTrackingInputs, HasInputs)

-- | Lifts a 'Pipe' into the 'Shell' type.
fromPipe :: Pipe i o (SafeT m) () -> Shell m i o ()
fromPipe = Shell . liftWithFakeValue ()

infixr 5 |>

-- | Composes two shells, left to right.
(|>) :: (Monad m, Semigroup r) => Shell m a b r -> Shell m b c r -> Shell m a c r
Shell a |> Shell b = Shell $ combineInputs (<>) (>->) a b

infixl 5 <|

-- | Composes two shells, right to left.
(<|) :: (Monad m, Semigroup r) => Shell m b c r -> Shell m a b r -> Shell m a c r
(<|) = flip (|>)

-- | Given a sequence of inputs, evaluates a shell and collects the outputs as a list.
evaluate :: (MonadMask m, MonadIO m) => Shell m i o () -> [i] -> m [o]
evaluate (Shell shell) values = runSafeT $ Pipes.toListM (Pipes.each values >-> withoutInputs shell)

-- | Evaluates a shell with no inputs or outputs, only effects.
evaluate_ :: (MonadMask m, MonadIO m) => Shell m () Void () -> m ()
evaluate_ shell = void $ evaluate shell [()]

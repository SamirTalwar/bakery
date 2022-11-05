module Bakery.Shell.TrackingInputs
  ( TrackingInputs,
    liftWithFakeValue,
    combineInputs,
    withoutInputs,
    MonadTrackingInputs (..),
  )
where

import Bakery.Input (HasInputs (..), Inputs)

-- | A monad transformer that tracks inputs.
data TrackingInputs m a = TrackingInputs
  { operationInputs :: Inputs,
    _operationFakeValue :: a,
    operationInner :: m a
  }
  deriving stock (Functor)

class MonadTrackingInputs m where
  -- | Register a new input for tracking.
  registerInput :: HasInputs a => a -> m ()

  -- | Register a number of inputs for tracking.
  registerInputs :: Inputs -> m ()

-- | Lifts a monad into the monad transformer.
--
-- A "fake" value of the correct return type needs to be provided. This fake
-- value will be used to run the monad without actually running anything, in
-- order to collect all inputs.
liftWithFakeValue :: a -> m a -> TrackingInputs m a
liftWithFakeValue = TrackingInputs []

-- | Combine two 'TrackingInputs' values, given the relevant combination
-- functions.
combineInputs ::
  (a -> b -> c) ->
  (m a -> n b -> p c) ->
  TrackingInputs m a ->
  TrackingInputs n b ->
  TrackingInputs p c
combineInputs
  combineFakeValues
  combineInner
  (TrackingInputs xInputs xFakeValue xInner)
  (TrackingInputs yInputs yFakeValue yInner) =
    TrackingInputs (xInputs <> yInputs) (xFakeValue `combineFakeValues` yFakeValue) (xInner `combineInner` yInner)

-- | Lower the monad transformer to the inner monad.
withoutInputs :: TrackingInputs m a -> m a
withoutInputs = operationInner

instance Applicative m => MonadTrackingInputs (TrackingInputs m) where
  registerInput input = registerInputs (getInputs input)
  registerInputs inputs = TrackingInputs inputs () (pure ())

instance Applicative m => Applicative (TrackingInputs m) where
  pure x = TrackingInputs [] x $ pure x
  TrackingInputs fInputs fakeF f <*> TrackingInputs xInputs fakeX x = TrackingInputs (fInputs <> xInputs) (fakeF fakeX) (f <*> x)

instance Monad m => Monad (TrackingInputs m) where
  TrackingInputs xInputs fakeX x >>= f =
    -- Use the fake value to retrieve inputs from the rest of the operation
    -- (this only works if the inputs are not dynamic)
    TrackingInputs (xInputs <> yInputs) fakeY do
      -- then discard the inputs when processing the real thing
      x >>= withoutInputs . f
    where
      TrackingInputs yInputs fakeY _ = f fakeX

instance HasInputs (TrackingInputs m r) where
  getInputs = operationInputs

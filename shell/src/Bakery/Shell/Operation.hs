module Bakery.Shell.Operation
  ( type (#>),
    fromPipe,
    Operation,
    runOperation,
    registerInput,
    registerInputs,
    (|>),
    (<|),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)

type i #> o = Operation (Pipe i o (SafeT IO)) ()

fromPipe :: Pipe i o (SafeT IO) () -> i #> o
fromPipe = Operation [] ()

data Operation m a = Operation
  { operationInputs :: Inputs,
    _operationFakeValue :: a,
    operationInner :: m a
  }
  deriving stock (Functor)

runOperation :: Operation m a -> m a
runOperation = operationInner

registerInput :: Applicative m => HasInputs a => a -> Operation m ()
registerInput input = registerInputs (getInputs input)

registerInputs :: Applicative m => Inputs -> Operation m ()
registerInputs inputs = Operation inputs () (pure ())

instance Applicative m => Applicative (Operation m) where
  pure x = Operation [] x $ pure x
  Operation fInputs fakeF f <*> Operation xInputs fakeX x = Operation (fInputs <> xInputs) (fakeF fakeX) (f <*> x)

instance Monad m => Monad (Operation m) where
  Operation xInputs fakeX x >>= f =
    -- Use the fake value to retrieve inputs from the rest of the operation
    -- (this only works if the inputs are not dynamic)
    Operation (xInputs <> yInputs) fakeY do
      -- then discard the inputs when processing the real thing
      x >>= runOperation . f
    where
      Operation yInputs fakeY _ = f fakeX

instance HasInputs (Operation m r) where
  getInputs = operationInputs

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs () a |> Operation bInputs () b =
  Operation (aInputs <> bInputs) () $ a >-> b

infixl 5 <|

(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

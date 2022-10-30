module Bakery.Shell.Operation
  ( type (#>),
    Operation,
    runOperation,
    registerInput,
    registerInputs,
    (|>),
    (<|),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))
import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)

type i #> o = Operation (Pipe i o (SafeT IO)) ()

data Operation m a = Operation Inputs (m a)
  deriving stock (Functor)

runOperation :: Operation m a -> m a
runOperation (Operation _ x) = x

registerInput :: Applicative m => HasInputs a => a -> Operation m ()
registerInput input = registerInputs (getInputs input)

registerInputs :: Applicative m => Inputs -> Operation m ()
registerInputs inputs = Operation inputs (pure ())

instance Applicative m => Applicative (Operation m) where
  pure = Operation [] . pure
  Operation fInputs f <*> Operation xInputs x = Operation (fInputs <> xInputs) (f <*> x)

instance Monad m => Monad (Operation m) where
  Operation xInputs x >>= f = Operation xInputs do
    x' <- x
    -- this just discard the subsequent inputs, which is probably not what we want
    let Operation _ y = f x'
    y

instance MonadTrans Operation where
  lift = Operation []

instance MonadIO m => MonadIO (Operation m) where
  liftIO = Operation [] . liftIO

instance HasInputs (Operation m r) where
  getInputs (Operation inputs _) = inputs

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

infixl 5 <|

(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

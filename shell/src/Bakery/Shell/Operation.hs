module Bakery.Shell.Operation
  ( type (#>),
    Operation (..),
    (|>),
    (<|),
  )
where

import Bakery.Input (HasInputs (..), Inputs)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))
import Pipes (Pipe, (>->))
import Pipes.Safe (SafeT)

type i #> o = Operation i o IO ()

data Operation i o m r = Operation Inputs (Pipe i o (SafeT m) r)
  deriving stock (Functor)

instance Applicative m => Applicative (Operation i o m) where
  pure x = Operation [] (pure x)
  Operation fInputs f <*> Operation xInputs x = Operation (fInputs <> xInputs) (f <*> x)

instance Monad m => Monad (Operation i o m) where
  Operation xInputs x >>= f = Operation xInputs do
    x' <- x
    -- this just discard the subsequent inputs, which is probably not what we want
    let Operation _ y = f x'
    y

instance MonadTrans (Operation i o) where
  lift = Operation [] . lift . lift

instance MonadIO m => MonadIO (Operation i o m) where
  liftIO = Operation [] . liftIO

instance HasInputs (Operation i o m r) where
  getInputs (Operation inputs _) = inputs

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
Operation aInputs a |> Operation bInputs b =
  Operation (aInputs <> bInputs) $ a >-> b

infixl 5 <|

(<|) :: (b #> c) -> (a #> b) -> a #> c
(<|) = flip (|>)

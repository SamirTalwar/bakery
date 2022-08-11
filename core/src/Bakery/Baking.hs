module Bakery.Baking (Baking (..)) where

import Bakery.Env
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)

newtype Baking a = Baking {runBaking :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)

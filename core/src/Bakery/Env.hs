module Bakery.Env (Env (..)) where

import System.IO (Handle)

newtype Env = Env {logger :: Maybe Handle}

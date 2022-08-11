module Bakery.Env (Env (..)) where

import System.IO (Handle)

data Env = Env
  { logger :: Maybe Handle,
    root :: FilePath
  }

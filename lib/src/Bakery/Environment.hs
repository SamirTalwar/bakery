module Bakery.Environment
  ( currentDirectory,
  )
where

import Bakery.Bakeable (BakeT (..))
import Bakery.Baking
import Bakery.Env qualified as Env
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks)

currentDirectory :: BakeT Baking FilePath
currentDirectory = lift . Baking $ asks Env.root

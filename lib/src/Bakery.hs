module Bakery
  ( bake,
    recipe,
    module Bakery.File,
    module Bakery.Run,
  )
where

import Bakery.Bakeable
import Bakery.File
import Bakery.Run

bake :: Bakeable a => Recipe a -> IO ()
bake r = follow r *> pure ()

recipe :: a -> (a -> Recipe a) -> Recipe a
recipe = flip ($)

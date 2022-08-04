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
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (eqT, Typeable)
import System.Environment qualified
import System.IO (hPutStrLn, stderr)

bake :: Bake a -> IO ()
bake thing = do
  args <- System.Environment.getArgs
  let outs = outputs thing
  logText "Outputs:"
  mapM_ logValue outs
  logText ""

  if null args
    then case last outs of
      Output out r ->  do
        logText ("Baking " <> show out <> "...")
        follow r *> pure ()
    else
      let targets = map file args
       in mapM_ (bake' outs) targets
  where
    bake' :: forall a. (Eq a, Show a, Typeable a) => [Output] -> a -> IO ()
    bake' [] target = fail $ "Cannot bake " <> show target
    bake' (Output @t out r : outs) target =
      case eqT @t @a of
        Just Refl | target == out -> do
          logText ("Baking " <> show out <> "...")
          follow r *> pure ()
        _ -> bake' outs target

recipe :: Bakeable a => a -> (a -> Recipe a) -> Bake a
recipe output produce = Recipe output $ produce output

logText :: String -> IO ()
logText = hPutStrLn stderr

logValue :: Show a => a -> IO ()
logValue = logText . show

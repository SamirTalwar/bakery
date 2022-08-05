module Bakery.Bake
  ( bake,
    recipe,
  )
where

import Bakery.Bakeable (Bake (..), Bakeable (..), Input (..), Output (..), deriveOutputs)
import Bakery.File qualified
import Control.Monad (forM, forM_)
import Data.Functor (($>))
import Data.List qualified as List
import Data.Typeable (Typeable, cast)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

bake :: Bake a -> IO ()
bake thing = do
  args <- getArgs
  let outputs = deriveOutputs thing
  logText "Outputs:"
  mapM_ logValue outputs
  logText ""

  if null args
    then -- consider the last recipe to be the default
      bakeOutputs outputs [last outputs]
    else do
      -- for now, we treat all targets on the command line as files
      targetOutputs <- mapM (findTarget outputs . Bakery.File.file) args
      bakeOutputs outputs targetOutputs
  where
    bakeOutputs :: [Output] -> [Output] -> IO ()
    bakeOutputs allOutputs targetOutputs = do
      requiredOutputs <- required allOutputs targetOutputs
      logText "Plan:"
      forM_ requiredOutputs \(Output x _ _) -> logValue x
      logText ""
      mapM_ bakeOutput requiredOutputs

    required :: [Output] -> [Output] -> IO [Output]
    required allOutputs targetOutputs = do
      requiredTargets <- forM targetOutputs $ \(Output _ inputs _) ->
        mapM (\(Input input) -> findTarget allOutputs input) inputs
      pure . List.nub $ concat requiredTargets

    findTarget :: MonadFail m => forall a. (Eq a, Show a, Typeable a) => [Output] -> a -> m Output
    findTarget outputs target =
      let targetOutput = List.find (isTarget target) outputs
       in maybe (fail $ "Cannot bake " <> show target) pure targetOutput

    isTarget :: forall a. (Eq a, Show a, Typeable a) => a -> Output -> Bool
    isTarget target (Output out _ _) =
      case cast out of
        Just out' | out' == target -> True
        _ -> False

    bakeOutput :: Output -> IO ()
    bakeOutput (Output out _ r) = do
      logText ("Baking " <> show out <> "...")
      follow r $> ()

recipe :: Bakeable a => a -> (a -> Recipe a) -> Bake a
recipe output produce = Recipe output $ produce output

logText :: String -> IO ()
logText = hPutStrLn stderr

logValue :: Show a => a -> IO ()
logValue = logText . show

module Bakery.Bake
  ( bake,
    recipe,
  )
where

import Bakery.Bakeable
import Bakery.Baking
import Bakery.Env
import Bakery.File qualified
import Bakery.Identifier
import Bakery.Input
import Bakery.Output
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Function (on)
import Data.List qualified as List
import Data.Typeable (Proxy (..))
import System.Directory qualified as Directory
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)

bake :: Bake Baking a -> IO ()
bake thing = do
  args <- getArgs
  logger <- fmap (const stderr) <$> lookupEnv "BAKE_LOG"
  root <- Directory.getCurrentDirectory
  let env = Env {logger, root}
  flip Reader.runReaderT env . runBaking $
    actuallyBake thing args

actuallyBake :: Bake Baking a -> [String] -> Baking ()
actuallyBake thing args = do
  outputs <- deriveOutputs thing
  logText "Outputs:"
  mapM_ (logText . (\(SomeOutput Output {outputId, outputInputs}) -> show outputId <> " <- " <> show outputInputs)) outputs
  logText ""

  if null args
    then -- consider the last recipe to be the default
      bakeOutputs outputs [last outputs]
    else do
      -- for now, we treat all targets on the command line as files
      targetOutputs <- mapM (\arg -> normalize (Bakery.File.file arg) >>= findTarget outputs) args
      bakeOutputs outputs targetOutputs
  where
    bakeOutputs :: Outputs -> Outputs -> Baking ()
    bakeOutputs allOutputs targetOutputs = do
      requiredOutputs <- required allOutputs targetOutputs
      logText "Plan:"
      forM_ requiredOutputs (logValue . identifier)
      logText ""
      mapM_ bakeOutput requiredOutputs

    required :: Outputs -> Outputs -> Baking Outputs
    required allOutputs targetOutputs = do
      requiredTargets <- forM targetOutputs $ \targetOutput@(SomeOutput Output {outputInputs}) -> do
        dependencies <- mapM (\(SomeInput (Input input)) -> findTarget allOutputs input) outputInputs
        pure $ dependencies ++ [targetOutput]
      pure . List.nubBy equalById $ concat requiredTargets
      where
        equalById = (==) `on` identifier

    findTarget :: (Identifiable a, Show a) => Outputs -> a -> Baking SomeOutput
    findTarget outputs target =
      let targetOutput = List.find (isTarget target) outputs
       in maybe (fail $ "Cannot bake " <> show target) pure targetOutput

    isTarget :: Identifiable a => a -> SomeOutput -> Bool
    isTarget target output =
      identifier target == identifier output

    bakeOutput :: SomeOutput -> Baking ()
    bakeOutput (SomeOutput output@Output {outputAction, outputExists}) = do
      logText ("Baking " <> show output <> "...")
      void outputAction
      doesExist <- outputExists
      unless doesExist . fail $ "Did not produce " <> show output <> "."

recipe :: forall a. Bakeable a => a -> Recipe a -> Bake Baking a
recipe target recipe' = Bake do
  normalized <- normalize target
  pure . Recipe $
    Output
      (identifier normalized)
      normalized
      (deriveInputs (Proxy :: Proxy a) recipe')
      (exists normalized)
      (follow recipe' normalized)

{-# ANN logText ("HLint: ignore Avoid lambda using `infix`" :: String) #-}
logText :: String -> Baking ()
logText text = Baking do
  handle <- Reader.asks logger
  liftIO $ maybe (pure ()) (\h -> hPutStrLn h text) handle

logValue :: Show a => a -> Baking ()
logValue = logText . show

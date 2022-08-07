module Bakery.Bake
  ( bake,
    recipe,
  )
where

import Bakery.Bakeable (Bake (..), Bakeable (..), Input (..), Output (..), Outputs, deriveOutputs)
import Bakery.File qualified
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Functor (($>))
import Data.List qualified as List
import Data.Typeable (Proxy (..))
import System.Environment (getArgs, lookupEnv)
import System.IO (Handle, hPutStrLn, stderr)

newtype Env = Env {logger :: Maybe Handle}

type Baking a = ReaderT Env IO a

bake :: Bake a -> IO ()
bake thing = do
  args <- getArgs
  logger <- fmap (const stderr) <$> lookupEnv "BAKE_LOG"
  let env = Env {logger}
  runReaderT (bake' thing args) env

bake' :: Bake a -> [String] -> Baking ()
bake' thing args = do
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
    bakeOutputs :: Outputs -> Outputs -> Baking ()
    bakeOutputs allOutputs targetOutputs = do
      requiredOutputs <- required allOutputs targetOutputs
      logText "Plan:"
      forM_ requiredOutputs \(Output outputId _ _ _) -> logValue outputId
      logText ""
      mapM_ bakeOutput requiredOutputs

    required :: Outputs -> Outputs -> Baking Outputs
    required allOutputs targetOutputs = do
      requiredTargets <- forM targetOutputs $ \targetOutput@(Output _ _ inputs _) -> do
        dependencies <- mapM (\(Input input) -> findTarget allOutputs input) inputs
        pure $ dependencies ++ [targetOutput]
      pure . List.nub $ concat requiredTargets

    findTarget :: (MonadFail m, Bakeable a) => Outputs -> a -> m Output
    findTarget outputs target =
      let targetOutput = List.find (isTarget target) outputs
       in maybe (fail $ "Cannot bake " <> show target) pure targetOutput

    isTarget :: Bakeable a => a -> Output -> Bool
    isTarget target (Output outputId _ _ _) =
      identifier target == outputId

    bakeOutput :: Output -> Baking ()
    bakeOutput (Output outputId _ _ action) = do
      logText ("Baking " <> show outputId <> "...")
      liftIO action $> ()

recipe :: forall a. Bakeable a => a -> Recipe a -> Bake a
recipe target recipe' =
  Recipe
    (identifier target)
    target
    (deriveInputs (Proxy :: Proxy a) recipe')
    (follow recipe' target)

{-# ANN logText ("HLint: ignore Avoid lambda using `infix`" :: String) #-}
logText :: String -> Baking ()
logText text = do
  handle <- Reader.asks logger
  liftIO $ maybe (pure ()) (\h -> hPutStrLn h text) handle

logValue :: Show a => a -> Baking ()
logValue = logText . show

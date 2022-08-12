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
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)

bake :: BakeT Baking a -> IO ()
bake thing = do
  args <- map Text.pack <$> getArgs
  logger <- fmap (const stderr) <$> lookupEnv "BAKE_LOG"
  root <- Directory.getCurrentDirectory
  let env = Env {logger, root}
  flip Reader.runReaderT env . runBaking $
    actuallyBake thing args

actuallyBake :: BakeT Baking a -> [Text] -> Baking ()
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
      targets <- mapM parseTarget args
      targetOutputs <- mapM (findTarget outputs) targets
      bakeOutputs outputs targetOutputs
  where
    -- Yes, this parser is currently a joke. We shall fix that.
    parseTarget :: Text -> Baking Id
    parseTarget text =
      case Text.findIndex (== ':') text of
        Nothing -> identifier <$> (maybe reject pure =<< parseName @Bakery.File.File text)
        Just separatorIndex ->
          let (targetNamespace, targetName) = second (Text.drop 1) $ Text.splitAt separatorIndex text
           in case targetNamespace of
                "file" -> identifier <$> (maybe reject pure =<< parseName @Bakery.File.File targetName)
                _ -> reject
      where
        reject :: Baking a
        reject = fail $ "I don't know what " <> show text <> " is."

    findTarget :: Outputs -> Id -> Baking SomeOutput
    findTarget outputs target =
      let targetOutput = List.find (\(SomeOutput Output {outputId}) -> outputId == target) outputs
       in maybe (fail $ "Cannot bake " <> show target) pure targetOutput

    bakeOutputs :: Outputs -> Outputs -> Baking ()
    bakeOutputs allOutputs targetOutputs = do
      requiredOutputs <- required allOutputs targetOutputs
      logText "Plan:"
      forM_ requiredOutputs (\(SomeOutput Output {outputId}) -> logValue outputId)
      logText ""
      mapM_ bakeOutput requiredOutputs

    bakeOutput :: SomeOutput -> Baking ()
    bakeOutput (SomeOutput output@Output {outputAction, outputExists}) = do
      logText ("Baking " <> show output <> "...")
      void outputAction
      doesExist <- outputExists
      unless doesExist . fail $ "Did not produce " <> show output <> "."

    required :: Outputs -> Outputs -> Baking Outputs
    required allOutputs targetOutputs = do
      requiredTargets <- forM targetOutputs $ \targetOutput@(SomeOutput Output {outputInputs}) -> do
        dependencies <- mapM (\(SomeInput (Input input)) -> findTarget allOutputs (identifier input)) outputInputs
        pure $ dependencies ++ [targetOutput]
      pure . List.nubBy equalById $ concat requiredTargets
      where
        equalById = (==) `on` (\(SomeOutput Output {outputId}) -> outputId)

recipe :: forall a. Bakeable a => a -> Recipe a -> BakeT Baking a
recipe = defineRecipe

{-# ANN logText ("HLint: ignore Avoid lambda using `infix`" :: String) #-}
logText :: String -> Baking ()
logText text = Baking do
  handle <- Reader.asks logger
  liftIO $ maybe (pure ()) (\h -> hPutStrLn h text) handle

logValue :: Show a => a -> Baking ()
logValue = logText . show

module Bakery.Bake
  ( bake,
    recipe,
  )
where

import Bakery.A
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
import Data.Bifunctor (bimap)
import Data.Data (Proxy (..))
import Data.Function (on)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)

bakeable :: Namespace -> Maybe (Is Bakeable)
bakeable (Namespace "file") = Just $ Is @Bakery.File.File
bakeable _ = Nothing

recipe :: forall a. Bakeable a => a -> Recipe a -> BakeT Baking a
recipe = defineRecipe

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
  mapM_ (logText . (\(An Output {outputId, outputInputs}) -> show outputId <> " <- " <> show outputInputs)) outputs
  logText ""

  if null args
    then -- consider the last recipe to be the default
      bakeOutputs outputs [last outputs]
    else do
      -- for now, we treat all targets on the command line as files
      targets <- mapM parseTarget args
      targetOutputs <- mapM (findTarget outputs) targets
      bakeOutputs outputs targetOutputs

-- Yes, this parser is currently a joke. We shall fix that.
parseTarget :: Text -> Baking Id
parseTarget text =
  case Text.findIndex (== ':') text of
    Nothing -> identifier <$> (maybe reject pure =<< parseName @Bakery.File.File text)
    Just separatorIndex ->
      let (targetNamespace, targetName) = bimap Namespace (Text.drop 1) $ Text.splitAt separatorIndex text
       in case bakeable targetNamespace of
            Nothing -> reject
            Just isBakeable -> parseName' isBakeable targetName
  where
    reject :: Baking a
    reject = fail $ "I don't know what " <> show text <> " is."

-- HLint incorrectly tries to remove brackets around '(Is @b)'.
-- Ormolu doesn't know how to deal with type applications in constructors yet.
{- HLINT ignore parseName' "Redundant bracket" -}
parseName' :: Is Bakeable -> Text -> Baking Id
{- ORMOLU_DISABLE -}
parseName' (Is @a) targetName = identifier <$> (maybe reject pure =<< parseName @a targetName)
  where
    namespaceString = Text.unpack $ unNamespace $ namespace (Proxy @a)
    reject = fail $ show targetName <> " cannot be parsed in the namespace \"" <> namespaceString <> "\"."
{- ORMOLU_ENABLE -}

findTarget :: Outputs -> Id -> Baking (An Output)
findTarget outputs target =
  let targetOutput = List.find (\(An Output {outputId}) -> outputId == target) outputs
   in maybe (fail $ "Cannot bake " <> show target) pure targetOutput

bakeOutputs :: Outputs -> Outputs -> Baking ()
bakeOutputs allOutputs targetOutputs = do
  requiredOutputs <- required allOutputs targetOutputs
  logText "Plan:"
  forM_ requiredOutputs (\(An Output {outputId}) -> logValue outputId)
  logText ""
  mapM_ bakeOutput requiredOutputs

bakeOutput :: An Output -> Baking ()
bakeOutput (An output@Output {outputAction, outputExists}) = do
  logText ("Baking " <> show output <> "...")
  void outputAction
  doesExist <- outputExists
  unless doesExist . fail $ "Did not produce " <> show output <> "."

required :: Outputs -> Outputs -> Baking Outputs
required allOutputs targetOutputs = do
  requiredTargets <- forM targetOutputs $ \targetOutput@(An Output {outputInputs}) -> do
    dependencies <- mapM (\(An (Input input)) -> findTarget allOutputs (identifier input)) outputInputs
    pure $ dependencies ++ [targetOutput]
  pure . List.nubBy equalById $ concat requiredTargets
  where
    equalById = (==) `on` (\(An Output {outputId}) -> outputId)

{- HLINT ignore logText "Avoid lambda using `infix`" -}
logText :: String -> Baking ()
logText text = Baking do
  handle <- Reader.asks logger
  liftIO $ maybe (pure ()) (\h -> hPutStrLn h text) handle

logValue :: Show a => a -> Baking ()
logValue = logText . show

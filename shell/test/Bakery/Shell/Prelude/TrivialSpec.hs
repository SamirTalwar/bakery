module Bakery.Shell.Prelude.TrivialSpec where

import Bakery.Shell
import Control.Monad.IO.Class (liftIO)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "cat" do
    it "pipes data from input to output" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate cat values
      result === values

  describe "empty" do
    it "does nothing" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate empty values
      result === ([] :: [String])

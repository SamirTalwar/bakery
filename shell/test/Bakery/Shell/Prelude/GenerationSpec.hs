module Bakery.Shell.Prelude.GenerationSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Control.Monad.IO.Class (liftIO)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "repeat" do
    it "repeats a value indefinitely" $ hedgehog do
      value <- forAll $ Gen.int Range.constantBounded
      limit <- forAll $ Gen.int (Range.constant 0 10)
      result <- liftIO $ evaluate (B.repeat value |> B.take limit) []
      result === Prelude.replicate limit value

  describe "replicate" do
    it "repeats a value a specified number of times" $ hedgehog do
      value <- forAll $ Gen.int Range.constantBounded
      limit <- forAll $ Gen.int (Range.constant 0 10)
      result <- liftIO $ evaluate (B.replicate limit value) []
      result === Prelude.replicate limit value

    it "is empty when replicating 0 times" $ hedgehog do
      value <- forAll $ Gen.int Range.constantBounded
      result <- liftIO $ evaluate (B.replicate 0 value) []
      result === []

    it "is empty when replicating a negative number of times" $ hedgehog do
      value <- forAll $ Gen.int Range.constantBounded
      limit <- forAll $ Gen.int (Range.constant minBound 0)
      result <- liftIO $ evaluate (B.replicate limit value) []
      result === []

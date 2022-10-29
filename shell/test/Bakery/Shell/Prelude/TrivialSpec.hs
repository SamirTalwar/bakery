module Bakery.Shell.Prelude.TrivialSpec where

import Bakery.Shell
import Bakery.Shell.Chunk qualified as Chunk
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

  describe "each" do
    it "converts a list to an operation" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate (each values) []
      result === values

  describe "capped" do
    it "wraps values in Chunk" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate (capped (each values)) []
      result === (Prelude.map Chunk.Value values <> [Chunk.End])

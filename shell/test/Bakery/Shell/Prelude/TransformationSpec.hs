module Bakery.Shell.Prelude.TransformationSpec where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Control.Monad.IO.Class (liftIO)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "map" do
    it "transforms values" $ hedgehog do
      let f x = x * 3 + 1
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate (B.map f) values
      result === Prelude.map f values

  describe "filter" do
    it "removes values" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      result <- liftIO $ evaluate (B.filter odd) values
      result === Prelude.filter odd values

  describe "drop" do
    it "drops values" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      n <- forAll $ Gen.int Range.constantBounded
      result <- liftIO $ evaluate (B.drop n) values
      result === Prelude.drop n values

  describe "dropWhile" do
    it "drops values until a predicate is not met" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      n <- forAll $ Gen.int Range.constantBounded
      result <- liftIO $ evaluate (B.dropWhile (< n)) values
      result === Prelude.dropWhile (< n) values

  describe "take" do
    it "takes values" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      n <- forAll $ Gen.int Range.constantBounded
      result <- liftIO $ evaluate (B.take n) values
      result === Prelude.take n values

  describe "takeWhile" do
    it "takes values until a predicate is not met" $ hedgehog do
      values <- forAll $ Gen.list (Range.linear 0 10) (Gen.int Range.constantBounded)
      n <- forAll $ Gen.int Range.constantBounded
      result <- liftIO $ evaluate (B.takeWhile (> n)) values
      result === Prelude.takeWhile (> n) values

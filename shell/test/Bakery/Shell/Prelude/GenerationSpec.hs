module Bakery.Shell.Prelude.GenerationSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Test.Hspec

spec :: Spec
spec = do
  describe "repeat" do
    it "repeats a value indefinitely" do
      result <- evaluate (B.repeat (99 :: Int) |> B.take 5) []
      result `shouldBe` [99, 99, 99, 99, 99]

  describe "replicate" do
    it "repeats a value a specified number of times" do
      result <- evaluate (B.replicate 3 "hello") []
      result `shouldBe` ["hello", "hello", "hello"]

    it "is empty when replicating 0 times" do
      result <- evaluate (B.replicate 0 "no") []
      result `shouldBe` []

    it "is empty when replicating a negative number of times" do
      result <- evaluate (B.replicate (-3) "definitely not") []
      result `shouldBe` []

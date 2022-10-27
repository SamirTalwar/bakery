module Bakery.Shell.Prelude.TransformationSpec where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Test.Hspec

spec :: Spec
spec = do
  describe "map" do
    it "transforms values" do
      let values :: [Int]
          values = [1 .. 20]
      result <- evaluate (B.map (\x -> x * 3 + 1)) values
      result `shouldBe` [4, 7 .. 61]

  describe "filter" do
    it "removes values" do
      let values :: [Int]
          values = [1 .. 20]
      result <- evaluate (B.filter odd) values
      result `shouldBe` [1, 3 .. 19]

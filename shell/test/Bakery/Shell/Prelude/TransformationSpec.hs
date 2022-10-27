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

  describe "drop" do
    it "drops values" do
      let values :: [Int]
          values = [10 .. 20]
      result <- evaluate (B.drop 3) values
      result `shouldBe` [13 .. 20]

  describe "dropWhile" do
    it "drops values until a predicate is not met" do
      let values :: [Int]
          values = [3, 4, 5, 6, 7, 6, 5, 4, 3]
      result <- evaluate (B.dropWhile (< 6)) values
      result `shouldBe` [6, 7, 6, 5, 4, 3]

  describe "take" do
    it "takes values" do
      let values :: [Int]
          values = [-10 .. 10]
      result <- evaluate (B.take 6) values
      result `shouldBe` [-10 .. -5]

  describe "takeWhile" do
    it "takes values until a predicate is not met" do
      let values :: [Int]
          values = [9, 7, 5, 3, 1, 9, 7, 5, 3, 1]
      result <- evaluate (B.takeWhile (> 2)) values
      result `shouldBe` [9, 7, 5, 3]

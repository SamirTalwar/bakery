module Bakery.Shell.Prelude.TrivialSpec where

import Bakery.Shell
import Test.Hspec

spec :: Spec
spec = do
  describe "cat" do
    it "pipes data from input to output" do
      let values :: [Int]
          values = [1, 2, 3]
      result <- evaluate cat values
      result `shouldBe` values

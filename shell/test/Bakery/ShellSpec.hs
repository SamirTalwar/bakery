module Bakery.ShellSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "cat" do
    it "pipes data from input to output" do
      let values :: [Int]
          values = [1, 2, 3]
      result <- evaluate cat values
      result `shouldBe` values

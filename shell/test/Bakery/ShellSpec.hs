module Bakery.ShellSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Test.Hspec

spec :: Spec
spec = do
  describe "a shell" do
    it "streams operations" do
      let values :: [Int]
          values = [1 .. 20]
          operation = B.filter even |> B.map (* 2)
      result <- evaluate operation values
      result `shouldBe` [4, 8 .. 40]

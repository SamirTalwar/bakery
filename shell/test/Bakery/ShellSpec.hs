module Bakery.ShellSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Prelude qualified as B
import Control.Monad.Trans (lift)
import Pipes qualified
import System.IO.Error (tryIOError)
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

    it "propagates errors" do
      let operation :: () #> Int
          operation = lift do
            Pipes.yield 1
            Pipes.yield 2
            Pipes.yield 3
            fail "Oh no!"
      result <- tryIOError $ evaluate operation []
      result `shouldBe` Left (userError "Oh no!")

    it "can run multiple operations over the streamed input" do
      let values :: [Int]
          values = [1 .. 20]
          operationA = B.take 10 |> B.filter even |> B.map (* 2)
          operationB = B.filter odd |> B.map (* 3)
          operation = do
            operationA -- only takes the first ten elements
            operationB -- works on the remainder
      result <- evaluate operation values
      result `shouldBe` [4, 8 .. 20] <> [33, 39 .. 57]

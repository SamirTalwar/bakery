{-# LANGUAGE ScopedTypeVariables #-}

module Bakery.Shell.RunSpec (spec) where

import Bakery.Shell
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Bytestring
import Data.Foldable (fold)
import Test.Hspec

spec :: Spec
spec = do
  describe "run" do
    it "runs a program" do
      chunks :: [Chunk ByteString] <- liftIO $ evaluate (run_ "echo" "one" "two" "three") [()]
      let result = fold $ mconcat chunks
      result `shouldBe` Bytestring.pack "one two three\n"

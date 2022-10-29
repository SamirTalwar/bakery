{-# LANGUAGE ScopedTypeVariables #-}

module Bakery.Shell.RunSpec (spec) where

import Bakery.Shell
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Foldable (fold)
import Test.Hspec

spec :: Spec
spec = do
  describe "run" do
    it "runs a program, capturing STDOUT" do
      chunks :: [Chunk ByteString] <- evaluate (run_ "echo" "one" "two" "three") [()]
      let result = fold $ mconcat chunks
      result `shouldBe` ByteString.pack "one two three\n"

    it "runs a program and passes in STDIN" do
      let input = ByteString.pack "one\ntwo\nthree\nfour\nfive\n"
      let output = ByteString.pack "five\nfour\none\nthree\ntwo\n"
      chunks :: [Chunk ByteString] <- evaluate (capped (each [input]) |> run_ "sort") []
      let result = fold $ mconcat chunks
      result `shouldBe` output

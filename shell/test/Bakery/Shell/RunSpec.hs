{-# LANGUAGE ScopedTypeVariables #-}

module Bakery.Shell.RunSpec (spec) where

import Bakery.Shell
import Bakery.Shell.Chunk qualified as Chunk
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Foldable (fold)
import Pipes qualified as P
import Test.Hspec

spec :: Spec
spec = do
  describe "run" do
    it "runs a program, capturing STDOUT" do
      chunks :: [Chunk ByteString] <- evaluate (run (cmd "echo" ~ "one" ~ "two" ~ "three")) [Chunk.End]
      let result = fold $ mconcat chunks
      result `shouldBe` ByteString.pack "one two three\n"

    it "runs a program and passes in STDIN" do
      let input = ByteString.pack "one\ntwo\nthree\nfour\nfive\n"
      let output = ByteString.pack "five\nfour\none\nthree\ntwo\n"
      let inputStream = fromPipe (Chunk.capped (P.each [input]))
      chunks :: [Chunk ByteString] <- evaluate (inputStream |> run (cmd "sort")) []
      let result = fold $ mconcat chunks
      result `shouldBe` output

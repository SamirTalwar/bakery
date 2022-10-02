{-# LANGUAGE ScopedTypeVariables #-}

module Bakery.Shell.StreamSpec where

import Bakery.Shell.Stream
import Data.Functor.Identity (Identity (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "an non-effectful stream" do
    it "can be processed" do
      let stream :: Stream () Identity Int = 1 #: 2 #: 3 #: stop
      let output = runIdentity $ toListM stream
      output `shouldBe` [1, 2, 3]

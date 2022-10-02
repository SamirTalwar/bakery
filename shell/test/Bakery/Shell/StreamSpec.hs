{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bakery.Shell.StreamSpec (spec) where

import Bakery.Shell.Stream
import Data.Functor.Identity (Identity (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

{-# ANN spec "HLint: ignore Monoid law, left identity" #-}
{-# ANN spec "HLint: ignore Monoid law, right identity" #-}
spec :: Spec
spec = do
  describe "an non-effectful stream" do
    it "can be processed" do
      let stream :: Stream () Identity Int = 1 #: 2 #: 3 #: stop
      let output = runIdentity $ toListM stream
      output `shouldBe` [1, 2, 3]

    it "concatenates" do
      let a :: Stream () Identity Int = 1 #: 2 #: 3 #: stop
      let b :: Stream () Identity Int = mempty
      let c :: Stream () Identity Int = 4 #: 5 #: 6 #: stop
      let stream = a <> b <> c
      let output = runIdentity $ toListM stream
      output `shouldBe` [1 .. 6]

    it "follows the monoid associativity law" $ hedgehog do
      a <- fromList @Identity <$> forAll genIntList
      b <- fromList @Identity <$> forAll genIntList
      c <- fromList @Identity <$> forAll genIntList
      toList ((a <> b) <> c) === toList (a <> (b <> c))

    it "follows the monoid identity law on the left" $ hedgehog do
      x <- fromList @Identity <$> forAll genIntList
      toList (mempty <> x) === toList x

    it "follows the monoid identity law on the right" $ hedgehog do
      x <- fromList @Identity <$> forAll genIntList
      toList (x <> mempty) === toList x

toList :: Stream () Identity o -> [o]
toList = runIdentity . toListM

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)

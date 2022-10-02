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
{-# ANN spec "HLint: ignore Functor law" #-}
{-# ANN spec "HLint: ignore Use <$>" #-}
{-# ANN spec "HLint: ignore Monad law, left identity" #-}
{-# ANN spec "HLint: ignore Monad law, right identity" #-}
{-# ANN spec "HLint: ignore Use >=>" #-}
spec :: Spec
spec = do
  describe "an non-effectful stream" do
    it "can be processed" do
      let xs :: Producer Identity Int = 1 #: 2 #: 3 #: stop
      let output = runIdentity $ toListM xs
      output `shouldBe` [1, 2, 3]

  describe "monoid behavior" do
    it "concatenates" do
      let as :: Producer Identity Int = 1 #: 2 #: 3 #: stop
      let bs :: Producer Identity Int = mempty
      let cs :: Producer Identity Int = 4 #: 5 #: 6 #: stop
      toList (as <> bs <> cs) `shouldBe` [1 .. 6]

    it "follows the monoid associativity law" $ hedgehog do
      as <- fromList @Identity <$> forAll genIntList
      bs <- fromList @Identity <$> forAll genIntList
      cs <- fromList @Identity <$> forAll genIntList
      toList ((as <> bs) <> cs) === toList (as <> (bs <> cs))

    it "follows the monoid identity law on the left" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (mempty <> xs) === toList xs

    it "follows the monoid identity law on the right" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (xs <> mempty) === toList xs

    it "follows the functor identity law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (fmap id xs) === toList xs

  describe "monadic behavior" do
    it "follows the functor composition law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      let g = (+ 3)
      let f = (* 2)
      toList (fmap (g . f) xs) === toList (fmap g (fmap f xs))

    it "follows the applicative functor identity law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (pure id <*> xs) === toList xs

    it "follows the applicative functor composition law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      let g = pure (+ 7)
      let f = pure (* 3)
      toList (pure (.) <*> g <*> f <*> xs) === toList (g <*> (f <*> xs))

    it "follows the applicative functor homomorphism law" $ hedgehog do
      x <- forAll $ Gen.int Range.linearBounded
      let f = (+ 21)
      toList (pure f <*> pure x) === toList (pure (f x))

    it "follows the applicative functor interchange law" $ hedgehog do
      x <- forAll $ Gen.int Range.linearBounded
      let f = pure (* 4)
      toList (f <*> pure x) === toList (pure ($ x) <*> f)

    it "follows the monad identity law on the left" $ hedgehog do
      x <- forAll $ Gen.int Range.linearBounded
      let f y = y #: y * 2 #: y #: stop
      toList (pure x >>= f) === toList (f x)

    it "follows the monad identity law on the right" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (xs >>= pure) === toList xs

    it "follows the monad associativity law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      let f y = y #: y * 2 #: y #: stop
      let g y = y #: stop
      toList (xs >>= \x -> f x >>= g) === toList ((xs >>= f) >>= g)

toList :: Producer Identity o -> [o]
toList = runIdentity . toListM

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)

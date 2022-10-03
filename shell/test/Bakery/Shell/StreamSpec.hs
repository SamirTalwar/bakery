{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bakery.Shell.StreamSpec (spec) where

import Bakery.Shell.Stream
import Control.Monad.Trans (lift)
import Control.Monad.Writer (Writer, runWriter, tell)
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

  describe "an effectful stream" do
    it "can be processed" do
      let write :: Consumer (Writer [String]) String =
            demand \x -> (lift (tell [x]) |> blackHole) <> write
          xs :: Effect (Writer [String]) =
            "a" #: "b" #: "c" #: stop |> write
          (_, output) = runWriter $ run xs
      output `shouldBe` ["a", "b", "c"]

  describe "composition" do
    it "propagates values from yielding to demanding" do
      let xs :: Producer Identity Int = 1 #: 2 #: stop
          ys :: Stream Int Identity Int = demand \x -> x + 5 #: demand \x' -> x' + 10 #: stop
          result = xs |> ys
      toList result `shouldBe` [6, 12]

    it "stops when the downstream stops" do
      let xs :: Producer Identity Int = 1 #: 2 #: 3 #: stop
          ys :: Stream Int Identity Int = demand \x -> x + 1 #: stop
          result = xs |> ys
      toList result `shouldBe` [2]

    it "stops when the upstream stops" do
      let xs :: Producer Identity Int = 1 #: 2 #: 3 #: stop
          ys :: Stream Int Identity Int = demand \x -> x * 2 #: ys
          result = xs |> ys
      toList result `shouldBe` [2, 4, 6]

    it "propagates values through demands" do
      let xs :: Producer Identity Int = 100 #: 200 #: stop
          ys :: Stream Int Identity Int = 1 #: 2 #: demand (#: ys)
          zs :: Stream Int Identity Int = demand \x -> x * 2 #: zs
          result = xs |> ys |> zs
      toList result `shouldBe` [2, 4, 200, 2, 4, 400, 2, 4]

  describe "monoid behavior" do
    it "concatenates" do
      let as :: Producer Identity Int = 1 #: 2 #: 3 #: stop
          bs :: Producer Identity Int = mempty
          cs :: Producer Identity Int = 4 #: 5 #: 6 #: stop
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
          f = (* 2)
      toList (fmap (g . f) xs) === toList (fmap g (fmap f xs))

    it "follows the applicative functor identity law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      toList (pure id <*> xs) === toList xs

    it "follows the applicative functor composition law" $ hedgehog do
      xs <- fromList @Identity <$> forAll genIntList
      let g = pure (+ 7)
          f = pure (* 3)
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
          g y = y #: stop
      toList (xs >>= \x -> f x >>= g) === toList ((xs >>= f) >>= g)

    it "follows the monad transformer identity law" $ hedgehog do
      x <- forAll $ Gen.int Range.linearBounded
      toList (lift (pure x)) === toList (pure x)

    it "follows the monad transformer composition law" $ hedgehog do
      x <- forAll $ Identity <$> Gen.int Range.linearBounded
      let f y = pure (y * 2)
      toList (lift (x >>= f)) === toList (lift x >>= (lift . f))

toList :: Producer Identity o -> [o]
toList = runIdentity . toListM

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 10) (Gen.int Range.linearBounded)

module Bakery.ShellSpec (spec) where

import Bakery.A
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell
import Bakery.Shell.Argument
import Bakery.Shell.Prelude qualified as B
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Void (Void)
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
      let shell :: Shell IO () Int ()
          shell = fromPipe do
            Pipes.yield 1
            Pipes.yield 2
            Pipes.yield 3
            fail "Oh no!"
      result <- tryIOError $ evaluate shell []
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

  describe "input tracking" do
    it "tracks any inputs referenced" do
      let shell :: Shell IO () Void ()
          shell = run_ (TrackedArg "./one") (TrackedArg "./two")
          inputs = getInputs shell
      case inputs of
        [An inputA, An inputB] -> do
          -- abusing `show` to check whether the arguments are legitimate
          show inputA `shouldBe` show (Input (TrackedArg "./one"))
          show inputB `shouldBe` show (Input (TrackedArg "./two"))
        _ -> fail ("Unexpected inputs: " <> show inputs)

    it "tracks inputs over multiple operations" do
      let shell :: Shell IO (Chunk ByteString) (Chunk ByteString) ()
          shell = do
            run (cmd (TrackedArg "./one") (TrackedArg "./two"))
            run (cmd (TrackedArg "./three"))
          inputs = getInputs shell
      case inputs of
        [An inputA, An inputB, An inputC] -> do
          -- abusing `show` to check whether the arguments are legitimate
          show inputA `shouldBe` show (Input (TrackedArg "./one"))
          show inputB `shouldBe` show (Input (TrackedArg "./two"))
          show inputC `shouldBe` show (Input (TrackedArg "./three"))
        _ -> fail ("Unexpected inputs: " <> show inputs)

newtype TrackedArg a = TrackedArg a

instance Show a => Show (TrackedArg a) where
  show (TrackedArg x) = show x

instance Show a => Identifiable (TrackedArg a) where
  namespace _ = Namespace (Text.pack "tracked arg")
  name x = Name (Text.pack (show x))

instance Argument (TrackedArg String) where
  toArg (TrackedArg x) = StringArg x

instance Show a => HasInputs (TrackedArg a) where
  getInputs x = [An (Input x)]

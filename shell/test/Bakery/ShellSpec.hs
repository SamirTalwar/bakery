module Bakery.ShellSpec (spec) where

import Bakery.A
import Bakery.Identifier
import Bakery.Input
import Bakery.Shell.Argument
import Bakery.Shell.Chunk
import Bakery.Shell.Prelude qualified as B
import Bakery.Shell.Run
import Bakery.Shell.Shell (Shell, (|>))
import Bakery.Shell.Shell qualified as Shell
import Data.ByteString (ByteString)
import Data.Text qualified as Text
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
      result <- Shell.evaluate operation values
      result `shouldBe` [4, 8 .. 40]

    it "propagates errors" do
      let operation :: Shell IO () Int ()
          operation = Shell.fromPipe do
            Pipes.yield 1
            Pipes.yield 2
            Pipes.yield 3
            fail "Oh no!"
      result <- tryIOError $ Shell.evaluate operation []
      result `shouldBe` Left (userError "Oh no!")

    it "can run multiple operations over the streamed input" do
      let values :: [Int]
          values = [1 .. 20]
          operationA = B.take 10 |> B.filter even |> B.map (* 2)
          operationB = B.filter odd |> B.map (* 3)
          operation = do
            operationA -- only takes the first ten elements
            operationB -- works on the remainder
      result <- Shell.evaluate operation values
      result `shouldBe` [4, 8 .. 20] <> [33, 39 .. 57]

  describe "input tracking" do
    it "tracks any inputs referenced" do
      let operation :: Shell IO (Chunk ByteString) (Chunk ByteString) ()
          operation = run (cmd (TrackedArg "./one") ~ TrackedArg "./two")
          inputs = getInputs operation
      case inputs of
        [An inputA, An inputB] -> do
          -- abusing `show` to check whether the arguments are legitimate
          show inputA `shouldBe` show (Input (TrackedArg "./one"))
          show inputB `shouldBe` show (Input (TrackedArg "./two"))
        _ -> fail ("Unexpected inputs: " <> show inputs)

    it "tracks inputs over multiple operations" do
      let operation :: Shell IO (Chunk ByteString) (Chunk ByteString) ()
          operation = do
            run (cmd "run" ~ TrackedArg "./one" ~ TrackedArg "./two")
            run (cmd (TrackedArg "./three"))
          inputs = getInputs operation
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

instance Applicative m => Argument m (TrackedArg String) where
  toArg (TrackedArg x) = pure $ StringArg x

instance Show a => HasInputs (TrackedArg a) where
  getInputs x = [An (Input x)]

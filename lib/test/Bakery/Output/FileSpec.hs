module Bakery.Output.FileSpec (spec) where

import Bakery.Bakeable
import Bakery.Baking
import Bakery.Env
import Bakery.Output.File.Internal
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "File" do
  it "accepts a valid path" do
    let path = "/foo/bar"
    result <- flip runReaderT (Env Nothing "") $ runBaking $ parseName (Text.pack path)
    result `shouldBe` Just (File path)

  it "rejects an empty path" do
    let path = ""
    result <- flip runReaderT (Env Nothing "") $ runBaking $ parseName @File path
    result `shouldBe` Nothing

  it "normalizes relative paths where possible" do
    let path = "foo/bar/.././baz/"
    result <- flip runReaderT (Env Nothing "") $ runBaking $ normalize (File path)
    result `shouldBe` File "./foo/bar/../baz"

  it "normalizes absolute paths where possible" do
    let path = "/foo/../bar/./baz/./"
    result <- flip runReaderT (Env Nothing "") $ runBaking $ normalize (File path)
    result `shouldBe` File "/foo/../bar/baz"

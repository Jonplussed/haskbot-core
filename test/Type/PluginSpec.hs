module Type.PluginSpec (main, spec) where

import qualified Data.Text        as T
import           Text.Parsec.Char (space, string)
import           Text.Parsec.Prim (parse)

import           Test.Hspec

import           Spec.Expectation (shouldSucceed)
import           Spec.Helper      (withPlugin)
import           Type.Plugin

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let name   = "myPlugin"
      help   = T.pack "help text"
      parser = space >> string "my plugin input"
      plugin = newPlugin name help parser

  describe "Plugin type" $ do
    describe "plName" $ do

      it "gets the name of the plugin" $ do
        plName plugin `shouldBe` name

    describe "plHelpText" $ do

      it "gets the help text of the plugin" $ do
        plHelpText plugin `shouldBe` help

    describe "newPlugin" $ do

      it "creates a new plugin" $ do
        -- difficult to test as plugin isn't a show instance
        plName (newPlugin name help parser) `shouldBe` name

    describe "runPlugin" $ do

      let expectedInput = "myPlugin my plugin input"

      it "parses the plugin name and runs the plugin's parser" $ do
        shouldSucceed $ parse (runPlugin plugin) expectedInput expectedInput

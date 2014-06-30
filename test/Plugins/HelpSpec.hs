module Plugins.HelpSpec (main, spec) where

import Data.Text        (unpack)

import Test.Hspec

import Plugins.Help     (listAllText, pluginFor)
import Spec.Expectation
import Spec.Helper
import Type.Plugin      (plHelpText)
import Type.User        (fromStrings)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "help plugin" $ do

    let user   = fromStrings "uuid" "name"
        plugin = pluginFor user
        runPl  = withPlugin plugin

    context "without any input text" $ do

      it "lists all available commands" $ do
        runPl "help" `shouldOutput` listAllText user

    context "with a command as input text" $ do

      it "lists the help text for that command" $ do
        runPl "help help" `shouldOutput` unpack (plHelpText plugin)

    context "with nonsensical input text" $ do

      it "lists all available commands" $ do
        runPl "help not_a_command" `shouldOutput` listAllText user
        runPl "help " `shouldOutput` listAllText user

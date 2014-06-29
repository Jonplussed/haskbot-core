module Plugins.FlipSpec (spec) where

import Test.Hspec

import Plugins.Flip     (plugin)
import Spec.Expectation (shouldFail)
import Spec.Helper      (testPluginResponses, withPlugin)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tableFlip" $ do
    context "with any input text" $ do
      testPluginResponses plugin
        [ ("flip table",     "(╯°□°）╯︵ ┻━┻")
        , ("flip foo bar!",  "(╯°□°）╯︵ ¡ɹɐq ooɟ")
        , ("flip ",          "(╯°□°）╯︵ ")
        ]

    context "without any input text" $ do
      it "should fail" $ do
        shouldFail $ withPlugin plugin "flip"


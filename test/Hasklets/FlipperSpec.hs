module Hasklets.FlipperSpec (spec) where

import Hasklets.Flipper (angryFlip)
import Hasklets.SpecHelper (confirmResponses)
import Test.Hspec

spec :: Spec
spec = do
  describe "angryFlip" $ do
    confirmResponses
      [ ("flip table",     "(╯°□°）╯︵ ┻━┻")
      , ("flip foo bar!",  "(╯°□°）╯︵ ¡ɹɐq ooɟ")
      ]

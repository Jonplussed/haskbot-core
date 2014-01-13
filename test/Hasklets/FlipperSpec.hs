module Hasklets.FlipperSpec (main, spec) where

import Hasklets.Flipper (angryFlip)
import Hasklets.SpecHelper (confirmResponses)
import Test.Hspec

type Username = String
type MatchData = String

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "angryFlip hasklet" $ do
    confirmResponses
      [ ("flip table",     "(╯°□°）╯︵ ┻━┻")
      , ("flip foo bar!",  "(╯°□°）╯︵ ¡ɹɐq ooɟ")
      ]

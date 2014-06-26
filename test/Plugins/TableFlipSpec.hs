module Plugins.TableFlipSpec (spec) where

import Test.Hspec

import Plugins.TableFlip  (tableFlip)
import Plugins.SpecHelper (responsesFor)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tableFlip" $ do
    responsesFor tableFlip
      [ ("flip table",     "(╯°□°）╯︵ ┻━┻")
      , ("flip foo bar!",  "(╯°□°）╯︵ ¡ɹɐq ooɟ")
      ]

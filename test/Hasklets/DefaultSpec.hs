module Hasklets.DefaultSpec (spec) where

import Hasklets.Default (passiveSigh)
import Hasklets.SpecHelper (confirmResponses)
import Test.Hspec

spec :: Spec
spec = do
  describe "passiveSigh" $ do
    let r =  "/me sighs passive-aggressively at [username]"

    confirmResponses
      [ (" ", r)
      , ("foo bar baz", r)
      , ("#!$%&", r)
      ]

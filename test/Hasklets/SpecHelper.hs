module Hasklets.SpecHelper (confirmResponses) where

import Chat.Message (message)
import Control.Monad (forM_)
import Hasklet.All (unleashUpon)
import Test.Hspec

--
-- public functions
--

confirmResponses :: [(String, String)] -> Spec
confirmResponses rs = do
  forM_ rs $ \(t,r) ->
    it ("correctly responds to \"" ++ t ++ "\"...") $ do
      unleashUpon (message usernameStub t) `shouldBe` r

--
-- private functions
--

-- If using "confirmResponses", the username is stubbed
-- as [username] to simplify testing
usernameStub :: String
usernameStub = "[username]"

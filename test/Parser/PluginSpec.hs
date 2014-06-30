module Parser.PluginSpec (main, spec) where

import qualified Text.Parsec.Prim as P

import           Test.Hspec

import           Parser.Plugin
import           Spec.Expectation
import           Spec.Helper
import           Type.User

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "pluginsFor" $ do

    let parse = parseWith $ pluginsFor user
        user  = fromStrings "id" "name"

    it "runs through plugins until a plugin's command succeeds" $ do
      shouldSucceed $ parse "help help"      -- known plugin
      shouldSucceed $ parse "flip something" -- known plugin
      shouldFail $ parse "notCurrentlyAPlugin"



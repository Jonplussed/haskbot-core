module Spec.Helper
( testPluginResponses
, withPlugin
) where

import Control.Monad     (forM_)
import Text.Parsec.Prim  (parse)
import Text.Parsec.Error (ParseError)

import Test.Hspec

import Spec.Expectation  (TestParser, shouldOutput)
import Type.Plugin       (Plugin, runPlugin)

type Input  = String
type Output = String

testPluginResponses :: Plugin -> [(Input, Output)] -> Spec
testPluginResponses plugin resps = do
  forM_ resps $ \(input, output) ->
    it ("correctly responds to \"" ++ input ++ "\"") $ do
      withPlugin plugin input `shouldOutput` output

withPlugin :: Plugin -> String -> TestParser String
withPlugin plugin input = parse (runPlugin plugin) input input

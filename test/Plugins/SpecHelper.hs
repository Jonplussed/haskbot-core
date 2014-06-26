module Plugins.SpecHelper (responsesFor) where

import Control.Monad     (forM_)
import Text.Parsec.Prim  (parse)
import Text.Parsec.Error (ParseError)

import Test.Hspec

import Parser.Commons   (Plugin)

type Input  = String
type Output = String

-- this works so long as we're not testing the contents of errors
-- but considering any parse error a failure
instance Eq ParseError where
  error1 == error2 = True

-- public functions

responsesFor :: Plugin -> [(Input, Output)] -> Spec
responsesFor plugin resps = do
  forM_ resps $ \(input, output) ->
    it ("correctly responds to \"" ++ input ++ "\"") $ do
      parse plugin input input `shouldBe` Right output

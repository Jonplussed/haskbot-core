module Parser.CombinatorSpec (main, spec) where

import qualified Text.Parsec.Prim  as P

import           Test.Hspec

import           Parser.Combinator
import           Spec.Expectation
import           Spec.Helper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let parseWith parser string = P.parse parser string string

  describe "parser combinators" $ do
    describe "botName" $ do

      let parse = parseWith botName

      it "parses and returns the name of the chatbot" $ do
        parse "haskbot" `shouldOutput` "haskbot"

      it "should fail with anything else" $ do
        shouldFail $ parse "haskbo"

    describe "text" $ do

      let parse = parseWith text

      it "parses out a leading space and returns the text" $ do
        parse " test text" `shouldOutput` "test text"

      it "fails without a leading space" $ do
        shouldFail $ parse "test text"

      it "fails without following characters" $ do
        shouldFail $ parse " "

    describe "args" $ do

      let parse = parseWith args

      it "parses out a leading space and returns the list of args" $ do
        parse " test text" `shouldOutput` ["test", "text"]

      it "fails without a leading space" $ do
        shouldFail $ parse "test text"

      it "fails without following characters" $ do
        shouldFail $ parse " "

    describe "optArgs" $ do

      let parse = parseWith optArgs

      it "parses out a leading space and returns the list of args" $ do
        parse " test text" `shouldOutput` ["test", "text"]

      it "parses no args with just a leading space" $ do
        parse " " `shouldOutput` []

      it "parses no args with no text" $ do
        parse "" `shouldOutput` []

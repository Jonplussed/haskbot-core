module Parser.CommonSpec (main, spec) where

import qualified Text.Parsec.Prim as P

import           Test.Hspec

import           Parser.Common
import           Spec.Expectation
import           Spec.Helper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "withArgs" $ do

    let parse = parseWith $ withArgs concat

    it "parses the args of input and passes them into a function" $ do
      parse " test text" `shouldOutput` "testtext"

    it "fails without a leading space" $ do
      shouldFail $ parse "test text"

    it "fails without following characters" $ do
      shouldFail $ parse " "

  describe "withOptArgs" $ do

    let parse = parseWith $ withOptArgs concat

    it "parses the args of input and passes them into a function" $ do
      parse " test text" `shouldOutput` "testtext"

    it "parses no args with just a leading space" $ do
      parse " " `shouldOutput` ""

    it "parses no args with no text" $ do
      parse "" `shouldOutput` ""

  describe "withText" $ do

    let parse = parseWith $ withText id

    it "parses the text of input and passes it into a function" $ do
      parse " test text" `shouldOutput` "test text"

    it "fails without a leading space" $ do
      shouldFail $ parse "test text"

    it "fails without following characters" $ do
      shouldFail $ parse " "

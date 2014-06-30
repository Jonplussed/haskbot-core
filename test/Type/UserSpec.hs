module Type.UserSpec (main, spec) where

import Test.Hspec
import Type.User

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let uuidStr = "UUID"
      uuid    = UUID uuidStr
      nameStr = "name"
      name    = UserName nameStr
      user    = User uuid name

  describe "UserName type" $ do
    describe "getUserName" $ do
      it "gets the user name string of the UserName" $ do
        getUserName name `shouldBe` nameStr

  describe "UUID type" $ do
    describe "getUUID" $ do
      it "gets the UUID string of the UUID" $ do
        getUUID uuid `shouldBe` uuidStr

  describe "User type" $ do
    describe "fromStrings" $ do
      it "creates a new user from a UUID and name" $ do
        fromStrings uuidStr nameStr `shouldBe` user

    describe "uniqueID" $ do
      it "gets the UUID of the user" $ do
        uniqueID user `shouldBe` uuid

    describe "userName" $ do
      it "gets the username of the user" $ do
        userName user `shouldBe` name

  describe "HasUser typeclass" $ do
    describe "getUser" $ do
      it "returns a user from a datatype" $ do
        getUser user `shouldBe` user

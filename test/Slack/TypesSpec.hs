{-# LANGUAGE OverloadedStrings #-}

module Slack.TypesSpec (main, spec) where

import Test.Hspec

import Slack.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Token" $ do
    describe "getToken and setToken" $ do

      it "they get and set the text value of a token" $ do
        getToken (setToken "value") `shouldBe` "value"

  describe "TeamID" $ do
    describe "getTeamID and setTeamID" $ do

      it "they get and set the text value of a token" $ do
        getTeamID (setTeamID "value") `shouldBe` "value"

  describe "ChannelID" $ do
    describe "getChanID and setChanID" $ do

      it "they get and set the text value of a token" $ do
        getChanID (setChanID "value") `shouldBe` "value"

  describe "ChannelName" $ do
    describe "setChanName" $ do

      it "omits a prefixed '#'" $ do
        getChanName (setChanName "#value") `shouldBe` "value"

      it "keeps all of text not prefixed with '#'" $ do
        getChanName (setChanName "value") `shouldBe` "value"

    describe "getPoundChan" $ do

      it "returns the channel name text with a prefixed '#'" $ do
        getPoundChan (setChanName "value") `shouldBe` "#value"

    describe "getChanName" $ do

      it "returns the channel name text without a prefix" $ do
        getChanName (setChanName "value") `shouldBe` "value"

  describe "UserID" $ do
    describe "getUserID and setUserID" $ do

      it "they get and set the text value of a token" $ do
        getUserID (setUserID "value") `shouldBe` "value"

  describe "UserName" $ do
    describe "setUserName" $ do

      it "omits a prefixed '@'" $ do
        getUserName (setUserName "@value") `shouldBe` "value"

      it "keeps all of text not prefixed with '@'" $ do
        getUserName (setUserName "value") `shouldBe` "value"

    describe "getAtUserName" $ do

      it "returns the channel name text with a prefixed '@'" $ do
        getAtUserName (setUserName "value") `shouldBe` "@value"

    describe "getUserName" $ do

      it "returns the channel name text without a prefix" $ do
        getUserName (setUserName "value") `shouldBe` "value"

  describe "Command" $ do
    describe "setCommand" $ do

      it "omits a prefixed '/'" $ do
        getCommand (setCommand "/value") `shouldBe` "value"

      it "keeps all of text not prefixed with '/'" $ do
        getCommand (setCommand "value") `shouldBe` "value"

    describe "getSlashCom" $ do

      it "returns the channel name text with a prefixed '/'" $ do
        getSlashCom (setCommand "value") `shouldBe` "/value"

    describe "getCommand" $ do

      it "returns the channel name text without a prefix" $ do
        getCommand (setCommand "value") `shouldBe` "value"

  describe "Channel" $ do
    describe "getAddress" $ do
      let chan = setChanName "in_channel"
          user = setUserName "direct_msg"

      it "returns the @username for a direct message" $ do
        getAddress (DirectMsg user) `shouldBe` "@direct_msg"

      it "returns the #channel for an in-channel reply" $ do
        getAddress (Channel chan) `shouldBe` "#in_channel"

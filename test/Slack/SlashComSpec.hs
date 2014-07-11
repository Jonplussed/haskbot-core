{-# LANGUAGE OverloadedStrings #-}

module Slack.SlashComSpec (main, spec) where

import Test.Hspec

import Slack.SlashCom
import Slack.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "SlashCom" $ do
    let slashCom = SlashCom (setToken    "token")
                            (setTeamID   "team_id")
                            (setChanID   "channel_id")
                            (setChanName "channel_name")
                            (setUserID   "user_id")
                            (setUserName "user_name")
                            (setCommand  "command")
                                         "text"

    describe "fromParams" $ do

      it "creates a SlashCom from the given params" $ do
        pendingWith "How do I test WAI request calls?"

    describe "replySameChan" $ do

      it "returns the channel where the SlashCom was sent" $ do
        replySameChan slashCom `shouldBe` Channel (setChanName "channel_name")

    describe "replyViaDM" $ do

      it "returns a DM channel to the SlashCom sender" $ do
        replyViaDM slashCom `shouldBe` DirectMsg (setUserName "user_name")

    describe "token" $ do

      it "returns the SlashCom's token" $ do
        token slashCom `shouldBe` setToken "token"

    describe "teamID" $ do

      it "returns the SlashCom's teamID" $ do
        teamID slashCom `shouldBe` setTeamID "team_id"

    describe "channelID" $ do

      it "returns the SlashCom's channelID" $ do
        channelID slashCom `shouldBe` setChanID "channel_id"

    describe "channelName" $ do

      it "returns the SlashCom's channelName" $ do
        channelName slashCom `shouldBe` setChanName "channel_name"

    describe "userID" $ do

      it "returns the SlashCom's userID" $ do
        userID slashCom `shouldBe` setUserID "user_id"

    describe "userName" $ do

      it "returns the SlashCom's userName" $ do
        userName slashCom `shouldBe` setUserName "user_name"

    describe "command" $ do

      it "returns the SlashCom's command" $ do
        command slashCom `shouldBe` setCommand "command"

    describe "text" $ do

      it "returns the SlashCom's text" $ do
        text slashCom `shouldBe` "text"

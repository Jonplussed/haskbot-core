{-# LANGUAGE OverloadedStrings #-}

module Slack.SlashCom
( SlashCom
, teamId
, channelId
, channelName
, userId
, userName
, command
, text
, newSlashCom
, fromParams
) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

import qualified Web.Scotty as W

import Slack.Types

data SlashCom = SlashCom { token       :: Token
                         , teamId      :: TeamId
                         , channelId   :: ChannelId
                         , channelName :: ChannelName
                         , userId      :: UserId
                         , userName    :: UserName
                         , command     :: Command
                         , text        :: Text
                         } deriving (Eq, Show)

newSlashCom
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> SlashCom
newSlashCom token
            teamId
            channelId
            channelName
            userId
            userName
            command
            text =
  SlashCom (Token token)
           (TeamId teamId)
           (ChannelId channelId)
           (ChannelName channelName)
           (UserId userId)
           (UserName userName)
           (Command command)
           text

fromParams :: W.ActionM SlashCom
fromParams = newSlashCom <$> W.param "token"
                         <*> W.param "team_id"
                         <*> W.param "channel_id"
                         <*> W.param "channel_name"
                         <*> W.param "user_id"
                         <*> W.param "user_name"
                         <*> W.param "command"
                         <*> W.param "text"

response :: W.ActionM ()
response = W.text "200 OK"

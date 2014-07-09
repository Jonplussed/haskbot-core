{-# LANGUAGE OverloadedStrings #-}

module Slack.SlashCom
( SlashCom (..)
, channel
, fromParams
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T

import Web.Scotty (ActionM, param)

import Slack.Types
import Slack.Channel

data SlashCom = SlashCom { token       :: Token
                         , teamId      :: TeamId
                         , channelId   :: ChannelId
                         , channelName :: ChannelName
                         , userId      :: UserId
                         , userName    :: UserName
                         , command     :: Command
                         , text        :: T.Text
                         } deriving (Eq, Show)

newSlashCom :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
            -> T.Text -> T.Text -> SlashCom
newSlashCom token teamId channelId channelName userId userName command text =
  SlashCom (Token token)
           (TeamId teamId)
           (ChannelId channelId)
           (ChannelName channelName)
           (UserId userId)
           (UserName userName)
           (Command $ T.tail command)
           text

channel :: SlashCom -> Channel
channel = fromText . getChanName . channelName

fromParams :: ActionM SlashCom
fromParams = newSlashCom <$> param "token"
                         <*> param "team_id"
                         <*> param "channel_id"
                         <*> param "channel_name"
                         <*> param "user_id"
                         <*> param "user_name"
                         <*> param "command"
                         <*> param "text"

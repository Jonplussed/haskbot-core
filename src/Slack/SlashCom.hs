{-# LANGUAGE OverloadedStrings #-}

module Slack.SlashCom
( SlashCom (..)
, fromParams
, replySameChan
, replyViaDM
) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

import Web.Scotty.Trans (param)

import Haskbot.Environment (ActionH)
import Slack.Types

data SlashCom = SlashCom { token       :: {-# UNPACK #-} !Token
                         , teamID      :: {-# UNPACK #-} !TeamID
                         , channelID   :: {-# UNPACK #-} !ChannelID
                         , channelName :: {-# UNPACK #-} !ChannelName
                         , userID      :: {-# UNPACK #-} !UserID
                         , userName    :: {-# UNPACK #-} !UserName
                         , command     :: {-# UNPACK #-} !Command
                         , text        :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

-- public functions

fromParams :: ActionH SlashCom
fromParams = newSlashCom <$> param "token"
                         <*> param "team_id"
                         <*> param "channel_id"
                         <*> param "channel_name"
                         <*> param "user_id"
                         <*> param "user_name"
                         <*> param "command"
                         <*> param "text"

replySameChan :: SlashCom -> Channel
replySameChan = Channel . channelName

replyViaDM :: SlashCom -> Channel
replyViaDM = DirectMsg . userName

-- private functions

newSlashCom :: Text -> Text -> Text -> Text
            -> Text -> Text -> Text -> Text
            -> SlashCom
newSlashCom a b c d e f g =
  SlashCom (setToken a)
           (setTeamID b)
           (setChanID c)
           (setChanName d)
           (setUserID e)
           (setUserName f)
           (setCommand g)

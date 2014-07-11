{-# LANGUAGE OverloadedStrings #-}

module Slack.SlashCom
( SlashCom (..)
, fromParams
, replySameChan
, replyViaDM
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T

import Web.Scotty (ActionM, param)

import Slack.Types

data SlashCom = SlashCom { token       :: Token
                         , teamID      :: TeamID
                         , channelID   :: ChannelID
                         , channelName :: ChannelName
                         , userID      :: UserID
                         , userName    :: UserName
                         , command     :: Command
                         , text        :: T.Text
                         } deriving (Eq, Show)

-- public functions

fromParams :: ActionM SlashCom
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

newSlashCom :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
            -> T.Text -> T.Text -> SlashCom
newSlashCom a b c d e f g =
  SlashCom (setToken a)
           (setTeamID b)
           (setChannelID c)
           (setChannelName d)
           (setUserID e)
           (setUserName f)
           (setCommand g)

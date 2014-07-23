module Slack.Haskbot.SlashCommand
( SlashCom (..)
) where

import Data.Text
import Slack.Haskbot.Types

data SlashCom = SlashCom { token       :: {-# UNPACK #-} !Token
                         , teamID      :: {-# UNPACK #-} !TeamID
                         , channelID   :: {-# UNPACK #-} !ChannelID
                         , channelName :: {-# UNPACK #-} !ChannelName
                         , userID      :: {-# UNPACK #-} !UserID
                         , userName    :: {-# UNPACK #-} !UserName
                         , command     :: {-# UNPACK #-} !Command
                         , text        :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

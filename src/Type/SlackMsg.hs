module Type.SlackMsg
( TeamId (..)
, ChannelId (..)
, ChannelName (..)
, UserId (..)
, UserName (..)
, Command (..)
, SlackMsg
, teamId
, channelId
, channelName
, userId
, userName
, command
, text
, newSlackMsg
) where

import Data.Text

newtype TeamId      = TeamId      { getTeamId      :: Text } deriving (Eq, Show)
newtype ChannelId   = ChannelId   { getChannelId   :: Text } deriving (Eq, Show)
newtype ChannelName = ChannelName { getChannelName :: Text } deriving (Eq, Show)
newtype UserId      = UserId      { getUserId      :: Text } deriving (Eq, Show)
newtype UserName    = UserName    { getUserName    :: Text } deriving (Eq, Show)
newtype Command     = Command     { getCommand     :: Text } deriving (Eq, Show)

data SlackMsg = SlackMsg { teamId      :: TeamId
                         , channelId   :: ChannelId
                         , channelName :: ChannelName
                         , userId      :: UserId
                         , userName    :: UserName
                         , command     :: Command
                         , text        :: String
                         } deriving (Eq, Show)

newSlackMsg
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> String
  -> SlackMsg
newSlackMsg teamId
            channelId
            channelName
            userId
            userName
            command
            text =
  SlackMsg (TeamId teamId)
           (ChannelId channelId)
           (ChannelName channelName)
           (UserId userId)
           (UserName userName)
           (Command command)
           text

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

import qualified Data.Text as T

newtype TeamId      = TeamId      { getTeamId      :: String } deriving (Eq, Show)
newtype ChannelId   = ChannelId   { getChannelId   :: String } deriving (Eq, Show)
newtype ChannelName = ChannelName { getChannelName :: String } deriving (Eq, Show)
newtype UserId      = UserId      { getUserId      :: String } deriving (Eq, Show)
newtype UserName    = UserName    { getUserName    :: String } deriving (Eq, Show)
newtype Command     = Command     { getCommand     :: String } deriving (Eq, Show)

data SlackMsg = SlackMsg { teamId      :: TeamId
                         , channelId   :: ChannelId
                         , channelName :: ChannelName
                         , userId      :: UserId
                         , userName    :: UserName
                         , command     :: Command
                         , text        :: T.Text
                         } deriving (Eq, Show)

newSlackMsg
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
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
           (T.pack text)

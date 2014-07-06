module Type.SlackMsg
( TeamId
, getTeamId
, ChannelId
, getChannelId
, ChannelName
, getChannelName
, TimeStamp
, getTimeStamp
, UserId
, getUserId
, UserName
, getUserName
, SlackMsg
, teamId
, channelId
, channelName
, timeStamp
, userId
, userName
, text
, fromStrings
) where

import qualified Data.Text as T

newtype TeamId      = TeamId      { getTeamId      :: String } deriving (Eq, Show)
newtype ChannelId   = ChannelId   { getChannelId   :: String } deriving (Eq, Show)
newtype ChannelName = ChannelName { getChannelName :: String } deriving (Eq, Show)
newtype TimeStamp   = TimeStamp   { getTimeStamp   :: String } deriving (Eq, Show)
newtype UserId      = UserId      { getUserId      :: String } deriving (Eq, Show)
newtype UserName    = UserName    { getUserName    :: String } deriving (Eq, Show)

data SlackMsg = SlackMsg { teamId      :: TeamId
                         , channelId   :: ChannelId
                         , channelName :: ChannelName
                         , timeStamp   :: TimeStamp
                         , userId      :: UserId
                         , userName    :: UserName
                         , text        :: T.Text
                         } deriving (Eq, Show)

fromStrings
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> SlackMsg
fromStrings teamId
            channelId
            channelName
            timeStamp
            userId
            userName
            text =
  SlackMsg (TeamId teamId)
           (ChannelId channelId)
           (ChannelName channelName)
           (TimeStamp timeStamp)
           (UserId userId)
           (UserName userName)
           (T.pack text)

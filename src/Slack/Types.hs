module Slack.Types
( Token       (..)
, TeamId      (..)
, ChannelId   (..)
, ChannelName (..)
, UserId      (..)
, UserName    (..)
, Command     (..)
) where

import Data.Text (Text)

newtype Token       = Token       { getToken    :: Text } deriving (Eq, Show)
newtype TeamId      = TeamId      { getTeamId   :: Text } deriving (Eq, Show)
newtype ChannelId   = ChannelId   { getChanId   :: Text } deriving (Eq, Show)
newtype ChannelName = ChannelName { getChanName :: Text } deriving (Eq, Show)
newtype UserId      = UserId      { getUserId   :: Text } deriving (Eq, Show)
newtype UserName    = UserName    { getUserName :: Text } deriving (Eq, Show)
newtype Command     = Command     { getCommand  :: Text } deriving (Eq, Show)

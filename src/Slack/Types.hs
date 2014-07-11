module Slack.Types
( Token       (getToken)
, TeamID      (getTeamID)
, ChannelID   (getChanID)
, ChannelName (getChanName)
, UserID      (getUserID)
, UserName    (getUserName)
, Command     (getCommand)
, Channel     (DirectMsg, Channel)
, getAddress
, getAtUserName
, getPoundChan
, getSlashCom
, setToken
, setTeamID
, setChanID
, setChanName
, setUserID
, setUserName
, setCommand
) where

import qualified Data.Text as T

newtype Token       = Token       { getToken    :: T.Text } deriving (Eq, Show)
newtype TeamID      = TeamID      { getTeamID   :: T.Text } deriving (Eq, Show)
newtype ChannelID   = ChannelID   { getChanID   :: T.Text } deriving (Eq, Show)
newtype ChannelName = ChannelName { getChanName :: T.Text } deriving (Eq, Show)
newtype UserID      = UserID      { getUserID   :: T.Text } deriving (Eq, Show)
newtype UserName    = UserName    { getUserName :: T.Text } deriving (Eq, Show)
newtype Command     = Command     { getCommand  :: T.Text } deriving (Eq, Show)

data Channel = DirectMsg UserName
             | Channel ChannelName
             deriving (Eq, Show)

-- constants

prefixChan, prefixCom, prefixUser :: Char
prefixChan = '#'
prefixCom  = '/'
prefixUser = '@'

-- public functions

getAddress :: Channel -> T.Text
getAddress (DirectMsg un) = getAtUserName un
getAddress (Channel ch)   = getPoundChan ch

getAtUserName :: UserName -> T.Text
getAtUserName = T.append (T.singleton prefixUser) . getUserName

getPoundChan :: ChannelName -> T.Text
getPoundChan = T.append (T.singleton prefixChan) . getChanName

getSlashCom :: Command -> T.Text
getSlashCom = T.append (T.singleton prefixCom) . getCommand

setToken :: T.Text -> Token
setToken = Token

setTeamID :: T.Text -> TeamID
setTeamID = TeamID

setChanID :: T.Text -> ChannelID
setChanID = ChannelID

setChanName :: T.Text -> ChannelName
setChanName = prefixedBy prefixChan ChannelName

setUserID :: T.Text -> UserID
setUserID = UserID

setUserName :: T.Text -> UserName
setUserName = prefixedBy prefixUser UserName

setCommand :: T.Text -> Command
setCommand = prefixedBy prefixCom Command

-- private functions

prefixedBy :: Char -> (T.Text -> a) -> T.Text -> a
prefixedBy pre f text
  | T.head text == pre = f $ T.tail text
  | otherwise          = f text

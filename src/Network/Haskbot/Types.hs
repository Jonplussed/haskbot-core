-- | This provides wrappers for the various types of data supplied by the Slack
--   API, so that any processing of the API data remains type-safe. No
--   constructors are directly exported to allow for flexibility with the
--   currently-beta Slack API.
module Network.Haskbot.Types
(
-- * Slack types
-- ** Token
  Token, getToken, setToken
-- ** Team ID
, TeamID, getTeamID, setTeamID
-- ** Channel ID
, ChannelID, getChanID, setChanID
-- ** Channel name
, ChannelName, getChanName, getPoundChan, setChanName
-- ** User ID
, UserID, getUserID, setUserID
-- ** User name
, UserName, getUserName, getAtUserName, setUserName
-- ** Command
, Command, getCommand, getSlashCom, setCommand
-- * Native types
-- ** Channel
, Channel (..), getAddress
) where

import qualified Data.Text as T

-- constants

prefixChan, prefixCom, prefixUser :: Char
prefixChan = '#'
prefixCom  = '/'
prefixUser = '@'

-- public functions

newtype Token
  = Token { getToken :: T.Text -- ^ get the text of a token
          } deriving (Eq, Show)

-- | make a token of the given text
setToken :: T.Text -> Token
setToken = Token

newtype TeamID
  = TeamID { getTeamID :: T.Text -- ^ get the text value of a team ID
           } deriving (Eq, Show)

-- | make a team ID of the given text value
setTeamID :: T.Text -> TeamID
setTeamID = TeamID

newtype ChannelID
  = ChannelID { getChanID :: T.Text -- ^ get the text value of a channel ID
              } deriving (Eq, Show)

-- | make a channel ID of the given text value
setChanID :: T.Text -> ChannelID
setChanID = ChannelID

newtype ChannelName
  = ChannelName { getChanName :: T.Text -- ^ get the text value of a channel name
                } deriving (Eq, Show)

-- | get the text value of a channel name, prefixed with a @#@
getPoundChan :: ChannelName -> T.Text
getPoundChan = T.append (T.singleton prefixChan) . getChanName

-- | make a channel name of the given text value
setChanName :: T.Text -> ChannelName
setChanName = prefixedBy prefixChan ChannelName

newtype UserID
  = UserID { getUserID :: T.Text -- ^ get the text value of a user ID
           } deriving (Eq, Show)

-- | make a user ID of the given text value
setUserID :: T.Text -> UserID
setUserID = UserID

newtype UserName
  = UserName { getUserName :: T.Text -- ^ get the text value of a username
             } deriving (Eq, Show)

-- | get the text value of a username prefixed with a @\@@
getAtUserName :: UserName -> T.Text
getAtUserName = T.append (T.singleton prefixUser) . getUserName

-- | make a username of given text value
setUserName :: T.Text -> UserName
setUserName = prefixedBy prefixUser UserName

newtype Command
  = Command { getCommand :: T.Text -- ^ get the text name of a command
            } deriving (Eq, Show)

-- | get the text name of a command prefixed with a @\/@
getSlashCom :: Command -> T.Text
getSlashCom = T.append (T.singleton prefixCom) . getCommand

-- | make a command with the given name
setCommand :: T.Text -> Command
setCommand = prefixedBy prefixCom Command

-- | Slack channels are either regular channels or direct messages to users
data Channel = DirectMsg {-# UNPACK #-} !UserName
             | Channel   {-# UNPACK #-} !ChannelName
             deriving (Eq, Show)

-- | Get the text representation of a channel, with the appropriate prefix
-- required by Slack
getAddress :: Channel -> T.Text
getAddress (DirectMsg un) = getAtUserName un
getAddress (Channel ch)   = getPoundChan ch

-- private functions

prefixedBy :: Char -> (T.Text -> a) -> T.Text -> a
prefixedBy pre f text
  | T.head text == pre = f $ T.tail text
  | otherwise          = f text

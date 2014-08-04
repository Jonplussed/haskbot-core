{-# LANGUAGE OverloadedStrings #-}

-- | This provides a representation of the request data from a Slack /slash
--   command/ integration. A "Network.Haskbot.Plugin" handler function is given
--   direct access to this data type when a /slash command/ is invoked via
--   Slack.
module Network.Haskbot.SlashCommand
(
-- * The Slash Command type
  SlashCom (..)
-- internal use only
, fromParams
) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Network.Haskbot.Internal.Environment (HaskbotM)
import Network.Haskbot.Internal.Request (Params, reqParam, optParam)
import Network.Haskbot.Types
import Network.Wai (Request)

-- | Encapsulates all data provided by a request from a Slack /slash command/
-- integration
data SlashCom
  = SlashCom
  { token       :: {-# UNPACK #-} !Token
  -- ^ the token corresponding to the /slash command/ integration secret token
  , teamID      :: {-# UNPACK #-} !TeamID
  -- ^ the team ID of the command invoker
  , channelID   :: {-# UNPACK #-} !ChannelID
  -- ^ the channel ID where the command was invoked
  , channelName :: {-# UNPACK #-} !ChannelName
  -- ^ the channel name where the command was invoked
  , userID      :: {-# UNPACK #-} !UserID
  -- ^ the user ID of the command invoker
  , userName    :: {-# UNPACK #-} !UserName
  -- ^ the username of the command invoker
  , command     :: {-# UNPACK #-} !Command
  -- ^ the name of the command invoked
  , optText     ::  Maybe Text
  -- ^ any text following the invoked slash command
  } deriving (Eq, Show)

-- internal functions

fromParams :: Params -> HaskbotM SlashCom
fromParams params =
    newSlashCom <$> reqParam' "token"
                <*> reqParam' "team_id"
                <*> reqParam' "channel_id"
                <*> reqParam' "channel_name"
                <*> reqParam' "user_id"
                <*> reqParam' "user_name"
                <*> reqParam' "command"
                <*> optParam' "text"
  where
    reqParam' = reqParam params
    optParam' = optParam params

-- private functions

newSlashCom :: Text -> Text -> Text -> Text
            -> Text -> Text -> Text -> Maybe Text
            -> SlashCom
newSlashCom a b c d e f g =
  SlashCom (setToken a)
           (setTeamID b)
           (setChanID c)
           (setChanName d)
           (setUserID e)
           (setUserName f)
           (setCommand g)

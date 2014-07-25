{-|
Module      : Slack.Haskbot.SlashCommand
Description : Wrapper for the Slack API /slash command/ integration
Copyright   : (c) Jonathan Childress 2014
License     : MIT
Maintainer  : jon@childr.es
Stability   : experimental
Portability : POSIX

This provides a representation of the request data from a Slack /slash command/
integration. A "Slack.Haskbot.Plugin" handler function is given direct access
to this data type when a /slash command/ is invoked via Slack.
-}

module Slack.Haskbot.SlashCommand
(
  -- * Types
  SlashCom (..)
) where

import Data.Text
import Slack.Haskbot.Types

-- | Encapsulates all data provided by a request from a Slack /slash command/
-- integration
data SlashCom
  = SlashCom
  { token       :: {-# UNPACK #-} !Token       -- ^ the secret token corresponding to the /slash command/ integration token
  , teamID      :: {-# UNPACK #-} !TeamID      -- ^ the team ID of the invoker
  , channelID   :: {-# UNPACK #-} !ChannelID   -- ^ the channel ID where the command was invoked
  , channelName :: {-# UNPACK #-} !ChannelName -- ^ the channel name where the command was invoked
  , userID      :: {-# UNPACK #-} !UserID      -- ^ the user ID of the invoker
  , userName    :: {-# UNPACK #-} !UserName    -- ^ the username of the invoker
  , command     :: {-# UNPACK #-} !Command     -- ^ the name of the command invoked
  , text        :: {-# UNPACK #-} !Text        -- ^ any text following the invoked slash command
  } deriving (Eq, Show)

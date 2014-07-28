-- | Module      : Slack.Haskbot.Incoming
--   Description : Wrapper for the Slack API /incoming/ integration
--   Copyright   : (c) Jonathan Childress 2014
--   License     : MIT
--   Maintainer  : jon@childr.es
--   Stability   : experimental
--   Portability : POSIX
--
--   This provides a simple representation of the request data for a Slack
--   /incoming/ integration- the means via which Haskbot replies to Slack.
--   Currently only simple text replies are supported, but this will be expanded
--   to support fully-slack-formatted messages in the future.
module Slack.Haskbot.Incoming
( Incoming (..)
) where

import Data.Text (Text)
import Slack.Haskbot.Types (Channel)

data Incoming = Incoming { incChan ::                !Channel -- ^ the channel to send the reply
                         , incText :: {-# UNPACK #-} !Text    -- ^ the text of the reply
                         } deriving (Eq, Show)

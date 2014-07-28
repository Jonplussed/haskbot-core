module Slack.Haskbot.Incoming
( Incoming (..)
) where

import Data.Text (Text)
import Slack.Haskbot.Types (Channel)

data Incoming = Incoming { incChan ::                !Channel
                         , incText :: {-# UNPACK #-} !Text
                         } deriving (Eq, Show)

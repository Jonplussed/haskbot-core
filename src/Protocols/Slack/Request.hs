module Protocols.Slack.Request
 ( Request (..)
 , fromPost
 ) where

-- Haskell platform libraries
--
import Control.Applicative ((<$>), (<*>))

-- foreign libraries

import Happstack.Server    (RqData, body, look)

data Request = Request { secretToken :: String
                       , channelName :: String
                       , timeStamp   :: String
                       , userName    :: String
                       , text        :: String
                       } deriving (Eq, Show)

fromPost :: RqData Request
fromPost = Request <$> bl "token"
                      <*> bl "channel_name"
                      <*> bl "timestamp"
                      <*> bl "user_name"
                      <*> bl "text"
  where bl = body . look

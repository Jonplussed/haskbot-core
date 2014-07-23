module Haskbot
( Haskbot
, haskbot
) where

import Slack.Haskbot.Internal.Environment (Haskbot)
import Slack.Haskbot.Internal.Server (webServer)
import Slack.Haskbot.Internal.Plugin (Plugin)

-- public functions

haskbot :: [Plugin] -> Int -> IO ()
haskbot = webServer

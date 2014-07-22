module Haskbot
( haskbot
) where

import Haskbot.Server (webServer)
import Haskbot.Slacklet (Slacklet)

-- public functions

haskbot :: [Slacklet] -> Int -> IO ()
haskbot = webServer

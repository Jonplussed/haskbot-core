module Haskbot
( haskbot
) where

import Haskbot.Plugin (Plugin)
import Haskbot.Server (webServer)

-- public functions

haskbot :: Int -> [Plugin] -> IO ()
haskbot = webServer

module Main
( main
) where

import Server.Web (webServer)

-- public functions

haskbot :: Int -> [Plugin] -> IO ()
haskbot port plugins

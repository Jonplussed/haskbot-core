module Main
( main
) where

import App.WebServer (webServer)

-- public functions

haskbot :: Int -> [Plugin] -> IO ()
haskbot port plugins

module Main
( main
) where

import Haskbot.Server (webServer)

-- public functions

haskbot :: Int -> [Plugin] -> IO ()
haskbot port plugins = do
    env <- getAppEnv plugins
    let haskbot r = runReaderT r env
    webServer haskbot port

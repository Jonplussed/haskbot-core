module Network.Haskbot.Config
( Config (..)
, incUrl
) where

data Config = Config { listenOn    :: Int
                     , incEndpoint :: String
                     , incToken    :: String
                     }

incUrl :: Config -> String
incUrl conf = incEndpoint conf ++ "?token=" ++ incToken conf

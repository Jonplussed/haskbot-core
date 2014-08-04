-- | The configuration type required to bootstrap "Slack.Haskbot"
module Network.Haskbot.Config
(
-- * The Config type
  Config (..)
-- internal use only
, incUrl
) where

data Config =
  Config { listenOn :: Int
         -- ^ the port on which Haskbot listens
         , incEndpoint :: String
         -- ^ the Slack endpoint of your /incoming/ integration, usually in the
         --   form of @https://[your company name].slack.com/services/hooks/incoming-webhook@
         , incToken :: String
         -- ^ the secret token of your /incoming/ integration
         }

-- internal functions

incUrl :: Config -> String
incUrl conf = incEndpoint conf ++ "?token=" ++ incToken conf

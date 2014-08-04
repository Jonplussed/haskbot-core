-- | A minimal Haskbot server can be run via:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Network.Haskbot
-- > import Network.Haskbot.Config
-- > import Network.Haskbot.Plugin
-- > import qualified Network.Haskbot.Plugin.Help as Help
-- >
-- > main :: IO ()
-- > main = haskbot config registry
-- >
-- > config :: Config
-- > config = Config { listenOn    = 3000
-- >                 , incEndpoint = "https://my-company.slack.com/services/hooks/incoming-webhook"
-- >                 , incToken    = "my-incoming-token"
-- >                 }
-- >
-- > registry :: [Plugin]
-- > registry = [ Help.register registry "my-slash-token" ]
--
--   This will run Haskbot on port 3000 with the included
--   "Network.Haskbot.Plugin.Help" plugin installed, where @\"my-slash-token\"@
--   is the secret token of a Slack slash command integration corresponding to
--   the @/haskbot@ command and pointing to the Haskbot server.
--
--   Be sure to create a Slack incoming integration (usually named /Haskbot/)
--   and set the 'incEndpoint' and 'incToken' to their corresponding values, so
--   that Slack can receive replies from Haskbot.
module Network.Haskbot
(
-- * Run a Haskbot server
  haskbot
) where

import Network.Haskbot.Internal.Server (webServer)
import Network.Haskbot.Config (Config)
import Network.Haskbot.Plugin (Plugin)

-- | Run the listed plugins on a Haskbot server with the given config
haskbot :: Config   -- ^ Your custom-created config
        -> [Plugin] -- ^ List of all Haskbot plugins to include
        -> IO ()
haskbot = webServer

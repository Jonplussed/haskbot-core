{-|
Module      : Slack.Haskbot
Description : An easily-extensible Slack chatbot using the Scotty web server
Copyright   : (c) Jonathan Childress 2014
License     : MIT
Maintainer  : jon@childr.es
Stability   : experimental
Portability : POSIX

Haskbot is an easily-extensible chatbot for the Slack messaging service. It
uses Slack's /incoming/ and /slash command/ service integrations to provide a
platform for adding custom slash commands with arbitrary responses to your
team's Slack service.

A minimal Haskbot server can be run via:

> import Slack.Haskbot
> import Slack.Haskbot.Plugin
> import qualified Slack.Haskbot.Plugin.Help as Help
>
> main :: IO ()
> main = haskbot registry 3000
>
> registry :: [Plugin]
> registry = [ Help.register registry "my_secret_token" ]

This will run Haskbot on port 3000 with the included
"Slack.Haskbot.Plugins.Help" plugin installed, where @\"my_secret_token\"@ is
the secret token of a Slack slash command integration corresponding to the
@/haskbot@ command and pointing to the Haskbot server.

Be sure to create a Slack incoming integration (usually named /Haskbot/) with
the local @HASKBOT_ENDPOINT@ environment variable set to the integration's
endpoint URL (including the secret key query string), so that Slack can process
replies from Haskbot.
-}

module Slack.Haskbot
(
-- * The Haskbot monad
  Haskbot
-- * Run a Haskbot server
, haskbot
) where

import Slack.Haskbot.Internal.Environment (Haskbot)
import Slack.Haskbot.Internal.Server (webServer)
import Slack.Haskbot.Internal.Plugin (Plugin)

-- | Run a Haskbot server with the listed plugins on the specified port.
haskbot :: [Plugin] -- ^ List of all Haskbot plugins to include
        -> Int      -- ^ Port on which to run Haskbot server
        -> IO ()
haskbot = webServer

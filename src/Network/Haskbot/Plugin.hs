-- | Module      : Network.Haskbot.Plugin
--   Description : Everything needed to create a Haskbot plugin
--   Copyright   : (c) Jonathan Childress 2014
--   License     : MIT
--   Maintainer  : jon@childr.es
--   Stability   : experimental
--   Portability : POSIX
--
--   Haskbot plugins are functions returning a "Plugin" data type. The "Plugin"
--   type is not exported directly; you should create new plugins via
--   'newPlugin'.
--
--   The recommended process for exporting plugins is to create a new module
--   that exports a single function currying the first three arguments to
--   'newPlugin'. The remaining argument, the Slack secret token of the
--   corresponding Slack /slash command/ service integration, can be supplied
--   in a separate file exporting the list of installed commands for "Haskbot".
--   This enables you to recreate a registry of installed tokens and
--   corresponding secret tokens in a separate file outside of version control.
--
--   A basic /Hello World/ plugin can created via:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module MyPlugins.HelloWorld (register) where
-- >
-- > import Network.Haskbot.Plugin
-- >
-- > name :: NameStr
-- > name = "hello_world"
-- >
-- > helpText :: HelpStr
-- > helpText = "Have Haskbot say _Hello, World!_ in your current channel."
-- >
-- > handler :: HandlerFn
-- > handler slashCom = return $ replySameChan slashCom "Hello, World!"
-- >
-- > register :: TokenStr -> Plugin
-- > register = newPlugin name helpText handler
--
--   To run the plugin, create a new Slack /slash command/ integration
--   corresponding to the command @\/hello_world@ that points to your Haskbot
--   server. Add the plugin's @register@ function to your Haskbot server's
--   plugin registry like detailed in "Slack.Haskbot", giving it the Slack
--   integration's secret token as the remaining argument. Rebuild and run the
--   server. Typing @\/hello_word@ into any Slack channel should return a
--   Haskbot response of /Hellow, world!/
module Network.Haskbot.Plugin
(
-- * Plugins
  Plugin
-- ** PLugin components
, plCommand, plHelpText, plHandler, plToken
-- ** Type aliases
, NameStr, HelpStr, HandlerFn, TokenStr
-- ** Creating a new Plugin
, newPlugin
-- * Slack replies
, replySameChan, replyAsDM
) where

import Data.Text (Text)
import Network.Haskbot.Internal.Environment (Haskbot)
import Network.Haskbot.Internal.Incoming (Incoming (Incoming), addToSendQueue)
import Network.Haskbot.Internal.SlashCommand (SlashCom (..))
import Network.Haskbot.Types

data Plugin = Plugin { plCommand  :: {-# UNPACK #-} !Command
                     , plHelpText :: {-# UNPACK #-} !Text
                     , plHandler  ::                !HandlerFn
                     , plToken    :: {-# UNPACK #-} !Token
                     }

type NameStr   = Text
type HelpStr   = Text
type HandlerFn = SlashCom -> Haskbot (Maybe Incoming)
type TokenStr  = Text

-- | Create a new 'Plugin' from the components
newPlugin :: NameStr   -- ^ The text name of the plugin command
          -> HelpStr   -- ^ Help text displayed in conjunction with the
                       -- "Slack.Haskbot.Plugin.Help" plugin
          -> HandlerFn -- ^ A function that takes a "Slack.Haskbot.SlashCommand"
                       -- and potentially returns a 'Incoming'
          -> TokenStr  -- ^ The secret token of the Slack /slash command/
                       -- integration associated with this plugin
          -> Plugin
newPlugin com help handler token =
  Plugin (setCommand com) help handler (setToken token)

-- | Send a Slack reply to the same channel as where the corresponding /slash
-- command/ was invoked, formatted according to
-- <https://api.slack.com/docs/formatting Slack>
replySameChan :: SlashCom -> Text -> Maybe Incoming
replySameChan sc = Just . Incoming (Channel $ channelName sc)

-- | Send a Slack reply as a DM to the user who invoked the /slash command/,
-- formatted according to <https://api.slack.com/docs/formatting Slack>
replyAsDM :: SlashCom -> Text -> Maybe Incoming
replyAsDM sc = Just . Incoming (DirectMsg $ userName sc)

-- | Haskbot plugins are functions returning a 'Plugin' data type. The 'Plugin'
--   type is not exported directly; you should create new plugins via
--   'newPlugin'.
--
--   The recommended process for exporting plugins is to create a new module
--   that exports a single function currying the first three arguments to
--   'newPlugin'. The remaining argument, the Slack secret token, can be
--   supplied in a separate file exporting the list of installed commands for
--   Haskbot. This enables you to recreate a registry of installed tokens and
--   corresponding secret tokens in a separate file outside of version control.
--
--   A basic /Hello World/ plugin can be created via:
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
--   plugin registry like detailed in "Network.Haskbot", giving it the Slack
--   integration's secret token as the remaining argument. Rebuild and run the
--   server. Typing @\/hello_word@ into any Slack channel should return a
--   Haskbot response of /Hello, world!/
module Network.Haskbot.Plugin
(
-- * The Plugin type
  Plugin (..)
-- * Creating Plugins
-- ** Helpful Type aliases
, NameStr, HelpStr, HandlerFn, TokenStr
-- ** Creating a new Plugin
, newPlugin
-- * Common Slack replies
, replySameChan, replyAsDM
-- * Internal
, isAuthorized
, runPlugin
, selectFrom
) where

import Control.Monad.Reader (lift)
import Data.List (find)
import Data.Text (Text)
import Network.Haskbot.Incoming (Incoming (Incoming), addToSendQueue)
import Network.Haskbot.Internal.Environment (HaskbotM)
import Network.Haskbot.SlashCommand (SlashCom (..), token)
import Network.Haskbot.Types

data Plugin =
  Plugin { plCommand  :: {-# UNPACK #-} !Command
         -- ^ The command that invokes this plugin
         , plHelpText :: {-# UNPACK #-} !Text
         -- ^ Help text displayed for this plugin via
         -- "Network.Haskbot.Plugin.Help"
         , plHandler  ::                !HandlerFn
         -- ^ The function that receives a "Network.Haskbot.SlashCommand"
         -- and maybe returns a "Network.Haskbot.Incoming"
         , plToken    :: {-# UNPACK #-} !Token
         -- ^ The secret token corresponding with this plugin's /slash command/
         -- Slack integration
         }

type NameStr   = Text
type HelpStr   = Text
type HandlerFn = SlashCom -> HaskbotM (Maybe Incoming)
type TokenStr  = Text

newPlugin :: NameStr   -- ^ The text name of the plugin command
          -> HelpStr   -- ^ (see 'plHelpText')
          -> HandlerFn -- ^ (see 'plHandler')
          -> TokenStr  -- ^ The text value of the /slash command/ secret token
          -> Plugin    -- ^ Creates a plugin to be run by Haskbot
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

-- internal functions

runPlugin :: Plugin -> SlashCom -> HaskbotM ()
runPlugin p slashCom = do
  reply <- plHandler p slashCom
  case reply of
    Just r -> addToSendQueue r
    _      -> return ()

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

selectFrom :: [Plugin] -> Command -> Maybe Plugin
selectFrom list com = find (\p -> plCommand p == com) list

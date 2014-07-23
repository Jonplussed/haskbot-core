module Slack.Haskbot.Plugin
( Plugin (..)
, NameStr
, HelpStr
, HandlerFn
, TokenStr
, newPlugin

, Reply  (..)
, replySameChan
, replyAsDM
, noReply
) where

import Data.Text (Text)
import Slack.Haskbot.Internal.Environment (Haskbot)
import Slack.Haskbot.Internal.Incoming (Incoming (Incoming), addToSendQueue)
import Slack.Haskbot.Internal.SlashCommand (SlashCom (..))
import Slack.Haskbot.Types

type NameStr   = Text
type HelpStr   = Text
type TokenStr  = Text
type HandlerFn = SlashCom -> Haskbot Reply

data Reply = ViaHaskbot Incoming
           | NoReply

data Plugin = Plugin { plCommand  :: {-# UNPACK #-} !Command
                     , plHelpText :: {-# UNPACK #-} !Text
                     , plHandler  ::                !HandlerFn
                     , plToken    :: {-# UNPACK #-} !Token
                     }

noReply :: (Monad m) => m Reply
noReply = return NoReply

replySameChan :: SlashCom -> Text -> Reply
replySameChan sc = ViaHaskbot . Incoming (Channel $ channelName sc)

replyAsDM :: SlashCom -> Text -> Reply
replyAsDM sc = ViaHaskbot . Incoming (DirectMsg $ userName sc)

newPlugin :: NameStr -> HelpStr -> HandlerFn -> TokenStr -> Plugin
newPlugin com help handler token =
  Plugin (setCommand com) help handler (setToken token)

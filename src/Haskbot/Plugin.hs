module Haskbot.Plugin
( HandlerFn
, HelpStr
, NameStr
, Plugin (..)
, Reply  (..)
, TokenStr
, isAuthorized
, newPlugin
, noReply
, runPlugin
, selectFrom
, viaHaskbot
) where

import Data.List (find)
import Data.Text (Text)

import Haskbot.Environment (Haskbot)
import Slack.Incoming (Incoming (Incoming), addToSendQueue)
import Slack.SlashCom (SlashCom, token)
import Slack.Types (Channel, Command, Token, setCommand, setToken)

type NameStr   = Text
type HelpStr   = Text
type HandlerFn = SlashCom -> Haskbot Reply
type TokenStr  = Text

data Reply = ViaHaskbot Incoming
           | NoReply

data Plugin = Plugin { plCommand  :: {-# UNPACK #-} !Command
                         , plHelpText :: {-# UNPACK #-} !Text
                         , plHandler  ::                !HandlerFn
                         , plToken    :: {-# UNPACK #-} !Token
                         }

runPlugin :: Plugin -> SlashCom -> Haskbot ()
runPlugin p slashCom = do
    reply <- plHandler p slashCom
    case reply of
      ViaHaskbot incoming -> addToSendQueue incoming
      _                   -> return ()

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

newPlugin :: Text -> Text -> HandlerFn -> Text -> Plugin
newPlugin com help handler token =
  Plugin (setCommand com) help handler (setToken token)

noReply :: (Monad m) => m Reply
noReply = return NoReply

selectFrom :: [Plugin] -> Command -> Maybe Plugin
selectFrom list com = find (\p -> plCommand p == com) list

viaHaskbot :: (Monad m) => Channel -> Text -> m Reply
viaHaskbot chan = return . ViaHaskbot . Incoming chan

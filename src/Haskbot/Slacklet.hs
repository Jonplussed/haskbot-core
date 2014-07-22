module Haskbot.Slacklet
( HandlerFn
, HelpStr
, NameStr
, Slacklet (..)
, Reply  (..)
, TokenStr
, isAuthorized
, newSlacklet
, noReply
, runSlacklet
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

data Slacklet = Slacklet { slCommand  :: {-# UNPACK #-} !Command
                         , slHelpText :: {-# UNPACK #-} !Text
                         , slHandler  ::                !HandlerFn
                         , slToken    :: {-# UNPACK #-} !Token
                         }

runSlacklet :: Slacklet -> SlashCom -> Haskbot ()
runSlacklet p slashCom = do
    reply <- slHandler p slashCom
    case reply of
      ViaHaskbot incoming -> addToSendQueue incoming
      _                   -> return ()

isAuthorized :: Slacklet -> SlashCom -> Bool
isAuthorized slacklet slashCom = slToken slacklet == token slashCom

newSlacklet :: Text -> Text -> HandlerFn -> Text -> Slacklet
newSlacklet com help handler token =
  Slacklet (setCommand com) help handler (setToken token)

noReply :: (Monad m) => m Reply
noReply = return NoReply

selectFrom :: [Slacklet] -> Command -> Maybe Slacklet
selectFrom list com = find (\p -> slCommand p == com) list

viaHaskbot :: (Monad m) => Channel -> Text -> m Reply
viaHaskbot chan = return . ViaHaskbot . Incoming chan

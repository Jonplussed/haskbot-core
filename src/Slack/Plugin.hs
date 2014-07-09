module Slack.Plugin
( Plugin
, Reply (..)
, Handler
, apply
, newPlugin
, selectFrom
) where

import Data.List (find)
import Data.Text (Text)

import Slack.SlashCom (SlashCom, command, token)
import Slack.Types (Command (..), Token (..))
import Slack.Incoming (Incoming, enqueue)

type Handler = SlashCom -> IO Reply

data Reply = ViaHaskbot Incoming
           | NoReply

data Plugin = Plugin { plCommand  :: Command
                     , plHelpText :: Text
                     , plHandler  :: Handler
                     , plToken    :: Token
                     }

apply :: Plugin -> SlashCom -> IO ()
apply plugin slashCom = do
  reply <- plHandler plugin slashCom
  case reply of
    ViaHaskbot incoming -> enqueue incoming
    _                   -> return ()

newPlugin :: Text -> Text -> Handler -> Text -> Plugin
newPlugin com help handler token =
  Plugin (Command com) help handler (Token token)

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

selectFrom :: [Plugin] -> SlashCom -> Maybe Plugin
selectFrom list slashCom = find (\p -> plCommand p == command slashCom) list

module Slack.Plugin
( Handler
, HelpText
, Name
, Plugin (..)
, Reply  (..)
, Token
, apply
, isAuthorized
, newPlugin
, selectFrom
) where

import Data.List (find)
import Data.Text (Text)

import Slack.SlashCom (SlashCom, command, token)
import qualified Slack.Types as T
import Slack.Incoming (Incoming, enqueue)

type Name     = Text
type HelpText = Text
type Handler  = SlashCom -> IO Reply
type Token    = Text

data Reply = ViaHaskbot Incoming
           | NoReply

data Plugin = Plugin { plCommand  :: T.Command
                     , plHelpText :: Text
                     , plHandler  :: Handler
                     , plToken    :: T.Token
                     }

apply :: Plugin -> SlashCom -> IO ()
apply plugin slashCom = do
  reply <- plHandler plugin slashCom
  case reply of
    ViaHaskbot incoming -> enqueue incoming
    _                   -> return ()

newPlugin :: Text -> Text -> Handler -> Text -> Plugin
newPlugin com help handler token =
  Plugin (T.Command com) help handler (T.Token token)

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

selectFrom :: [Plugin] -> SlashCom -> Maybe Plugin
selectFrom list slashCom = find (\p -> plCommand p == command slashCom) list

module Slack.Haskbot.Internal.Plugin
( Plugin (..)
, isAuthorized
, runPlugin
, selectFrom
) where

import Data.List (find)
import Data.Text (Text)
import Slack.Haskbot.Internal.Environment (Haskbot)
import Slack.Haskbot.Internal.Incoming (Incoming (Incoming), addToSendQueue)
import Slack.Haskbot.Plugin (Plugin (..))
import Slack.Haskbot.SlashCommand (SlashCom, token)
import Slack.Haskbot.Types

runPlugin :: Plugin -> SlashCom -> Haskbot ()
runPlugin p slashCom = plHandler p slashCom >>= maybe (return ()) addToSendQueue

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

selectFrom :: [Plugin] -> Command -> Maybe Plugin
selectFrom list com = find (\p -> plCommand p == com) list

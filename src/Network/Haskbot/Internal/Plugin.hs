module Network.Haskbot.Internal.Plugin
( Plugin (..)
, isAuthorized
, runPlugin
, selectFrom
) where

import Data.List (find)
import Data.Text (Text)
import Network.Haskbot.Internal.Environment (Haskbot)
import Network.Haskbot.Internal.Incoming (Incoming (Incoming), addToSendQueue)
import Network.Haskbot.Plugin (Plugin (..))
import Network.Haskbot.SlashCommand (SlashCom, token)
import Network.Haskbot.Types

runPlugin :: Plugin -> SlashCom -> Haskbot ()
runPlugin p slashCom = plHandler p slashCom >>= maybe (return ()) addToSendQueue

isAuthorized :: Plugin -> SlashCom -> Bool
isAuthorized plugin slashCom = plToken plugin == token slashCom

selectFrom :: [Plugin] -> Command -> Maybe Plugin
selectFrom list com = find (\p -> plCommand p == com) list

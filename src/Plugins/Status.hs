module Plugins.Status (pluginFor) where

import Control.Applicative (liftA)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Parser.Common (withArgs)
import Persistence.Redis
import Type.Plugin
import Type.SlackMsg

-- constants

name :: Name
name = "status"

helpText :: HelpText
helpText = T.pack
  "Set your status with `haskbot status [my status]`; see the statuses of\
  \ others with `haskbot status of [username]`"

defStatus :: String
defStatus = "fine and dandy"

-- public functions

pluginFor :: SlackMsg -> Plugin
pluginFor = newPlugin name helpText . parser

-- private functions

parser :: SlackMsg -> InputParser
parser slackMsg = withArgs $ liftA fromValue . getOrSetStatus slackMsg

getOrSetStatus :: SlackMsg -> [String] -> IO Value
getOrSetStatus _ ["of", name] = getStatus $ UserName name
getOrSetStatus slackMsg status    = setStatus slackMsg $ unwords status

statusKey :: UserName -> Key
statusKey (UserName name) = toKey $ "status-" ++ name

getStatus :: UserName -> IO Value
getStatus slackMsgName = getWithDefault defStatus $ statusKey slackMsgName

setStatus :: SlackMsg -> String -> IO Value
setStatus slackMsg status = setWithDefault reply (toValue status) key
  where
    key   = statusKey $ userName slackMsg
    reply = "Your status is now \"" ++ status ++ "\"."


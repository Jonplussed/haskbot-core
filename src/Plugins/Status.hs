module Plugins.Status (pluginFor) where

import Control.Applicative (liftA)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Parser.Common (withArgs)
import Persistence.Redis (Key, Value, toKey, toValue, fromValue, getWithDefault,
                          setWithDefault)
import Type.Plugin (Plugin, Name, HelpText, InputParser, newPlugin)
import Type.User (User, UserName (UserName), userName)

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

pluginFor :: User -> Plugin
pluginFor = newPlugin name helpText . parser

-- private functions

parser :: User -> InputParser
parser user = withArgs $ liftA fromValue . getOrSetStatus user

getOrSetStatus :: User -> [String] -> IO Value
getOrSetStatus _ ["of", name] = getStatus $ UserName name
getOrSetStatus user status    = setStatus user $ unwords status

statusKey :: UserName -> Key
statusKey (UserName name) = toKey $ "status-" ++ name

getStatus :: UserName -> IO Value
getStatus userName = getWithDefault defStatus $ statusKey userName

setStatus :: User -> String -> IO Value
setStatus user status = setWithDefault reply (toValue status) key
  where
    key   = statusKey $ userName user
    reply = "Your status is now \"" ++ status ++ "\"."


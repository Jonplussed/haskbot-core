{-# LANGUAGE OverloadedStrings #-}

module Plugin.Help
( listAllText
, register
) where

import Data.List (find)
import qualified Data.Text as T

import Registry (registry)
import Slack.SlashCom (replyViaDM, text)
import Slack.Types (getCommand, setCommand)
import Slack.Plugin

-- constants

name :: NameStr
name = "help"

helpText :: HelpStr
helpText =
    "List all installed plugins via `haskbot help`. To see the help\
    \ text of a particular plugin, use `haskbot help [plugin name]`."

-- public functions

register :: TokenStr -> Plugin
register = newPlugin name helpText handler

-- private functions

getHelp :: [T.Text] -> T.Text
getHelp []    = listAllText
getHelp (comName:_) =
  case selectFrom registry (setCommand comName) of
    Just plugin -> plHelpText plugin
    Nothing     -> listAllText

handler :: HandlerFn
handler slashCom = viaHaskbot chan reply
  where
    chan  = replyViaDM slashCom
    reply = getHelp . T.words $ text slashCom

listAllText :: T.Text
listAllText =
  T.concat [ "Available commands: "
           , T.intercalate ", " (map pluginName registry)
           , ". To get help for a specific command,\
             \ use `/haskbot help [command]`."
           ]
  where
    pluginName p = T.concat ["`", getCommand (plCommand p), "`"]

{-# LANGUAGE OverloadedStrings #-}

module Plugin.Help
( listAllText
, register
) where

import Data.List (find)
import qualified Data.Text as T

import Registry (registry)
import Slack.Incoming
import Slack.Plugin
import Slack.SlashCom
import Slack.Types (getCommand)

-- constants

name :: Name
name = "help"

helpText :: HelpText
helpText =
    "List all installed plugins via `haskbot help`. To see the help\
    \ text of a particular plugin, use `haskbot help [plugin name]`."

-- public functions

register :: Token -> Plugin
register = newPlugin name helpText handler

-- private functions

handler :: Handler
handler slashCom = return . ViaHaskbot $ Incoming chan reply
  where
    chan  = channel slashCom
    reply = getHelp . T.words $ text slashCom

listAllText :: T.Text
listAllText =
  T.concat [ "Available commands: "
           , allCommands
           , ". To get help for a specific command,\
             \ use `/haskbot help [command]`."
           ]
  where
    allCommands    = T.intercalate ", " $ map plCom registry
    plCom plugin = T.concat ["`", getCommand (plCommand plugin), "`"]

getHelp :: [T.Text] -> T.Text
getHelp []       = listAllText
getHelp (a:args) =
  case plugin of
    Just p  -> plHelpText p
    Nothing -> listAllText
  where
    plugin = find (\p -> getCommand (plCommand p) == a) registry

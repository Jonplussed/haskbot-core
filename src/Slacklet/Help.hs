{-# LANGUAGE OverloadedStrings #-}

module Slacklet.Help (register) where

import Data.List (find)
import qualified Data.Text as T

import Haskbot.Slacklet
import Slack.SlashCom (replyViaDM, text)
import Slack.Types (getCommand, setCommand)

-- constants

name :: NameStr
name = "haskbot"

helpText :: HelpStr
helpText =
    "List all installed Slacklets via `/haskbot`. To see the help text of a\
    \ particular Slacklet, use `/haskbot [Slacklet name]`."

-- public functions

register :: [Slacklet] -> TokenStr -> Slacklet
register = newSlacklet name helpText . handler

-- private functions

getHelp :: [Slacklet] -> [T.Text] -> T.Text
getHelp slacklets []          = listAllText slacklets
getHelp slacklets (comName:_) =
  case selectFrom slacklets (setCommand comName) of
    Just plugin -> slHelpText plugin
    Nothing     -> listAllText slacklets

handler :: [Slacklet] -> HandlerFn
handler slacklets slashCom = viaHaskbot (replyViaDM slashCom) reply
  where reply = getHelp slacklets . T.words $ text slashCom

listAllText :: [Slacklet] -> T.Text
listAllText slacklets =
  T.concat [ "Available commands: "
           , T.intercalate ", " (map name slacklets)
           , ". To get help for a specific command, use `/haskbot [command]`."
           ]
  where name p = T.concat ["`/", getCommand (slCommand p), "`"]

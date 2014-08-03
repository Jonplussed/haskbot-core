{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Plugin.Help (register) where

import Control.Applicative ((<$>))
import Data.List (find)
import qualified Data.Text as T
import Network.Haskbot.Plugin
import Network.Haskbot.SlashCommand (optText)
import Network.Haskbot.Types (getCommand, setCommand)

-- constants

name :: NameStr
name = "haskbot"

helpText :: HelpStr
helpText =
    "List all installed Plugins via `/haskbot`. To see the help text of a\
    \ particular Plugin, use `/haskbot [Plugin name]`."

-- public functions

register :: [Plugin] -> TokenStr -> Plugin
register = newPlugin name helpText . handler

-- private functions

getHelp :: [Plugin] -> Maybe [T.Text] -> T.Text
getHelp plugins (Just [comName]) =
  maybe (listAllText plugins) plHelpText
    (selectFrom plugins $ setCommand comName)
getHelp plugins _                = listAllText plugins

handler :: [Plugin] -> HandlerFn
handler plugins slashCom = return $ replyAsDM slashCom reply
  where reply = getHelp plugins $ T.words <$> optText slashCom

listAllText :: [Plugin] -> T.Text
listAllText plugins =
  T.concat [ "Available commands: "
           , T.intercalate ", " (map name plugins)
           , ". To get help for a specific command, use `/haskbot [command]`."
           ]
  where name p = T.concat ["`/", getCommand (plCommand p), "`"]

{-# LANGUAGE OverloadedStrings #-}

module Plugins.Help
( listAllText
, plugin
) where

import Data.List (find, intercalate)
import Data.Text (unpack)

import Parser.Common (withOptArgs)
import Registry (registry)
import Type.Plugin
import Type.SlackMsg

-- constants

name :: Name
name = "help"

helpText :: HelpText
helpText =
    "List all installed plugins via `haskbot help`. To see the help\
    \ text of a particular plugin, use `haskbot help [plugin name]`."

-- public functions

plugin :: SlackMsg -> Plugin
plugin = newPlugin name helpText . parserFor

-- private functions

parserFor :: SlackMsg -> InputParser
parserFor slackMsg = withOptArgs $ return . getHelpFor slackMsg

listAllText :: SlackMsg -> String
listAllText slackMsg =
    "Available commands: " ++ listFor slackMsg ++ ". To get help for a\
    \ specific command, use `/haskbot help [command]`."
  where
    listFor        = intercalate ", " . map plName' . registry
    plName' plugin = "`" ++ plName plugin ++ "`"

getHelpFor :: SlackMsg -> [String] -> String
getHelpFor slackMsg []       = listAllText slackMsg
getHelpFor slackMsg (a:args) =
  case plugin of
    Just p  -> unpack $ plHelpText p
    Nothing -> listAllText slackMsg
  where
    plugin = find (\p -> plName p == a) $ registry slackMsg

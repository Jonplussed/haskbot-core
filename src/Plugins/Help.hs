{-# LANGUAGE OverloadedStrings #-}

module Plugins.Help
( listAllText
, pluginFor
) where

import Data.List     (find, intercalate)
import Data.Text     (unpack)

import Parser.Common (withOptArgs)
import Registry      (registry)
import Type.Plugin   (Plugin, Name, HelpText, InputParser, plName,
                      plHelpText, newPlugin)
import Type.User     (User)

-- constants

name :: Name
name = "help"

helpText :: HelpText
helpText =
    "List all installed plugins via `haskbot help`. To see the help\
    \ text of a particular plugin, use `haskbot help [plugin name]`."

-- public functions

pluginFor :: User -> Plugin
pluginFor = newPlugin name helpText . parserFor

-- private functions

parserFor :: User -> InputParser
parserFor = withOptArgs . getHelpFor

listAllText :: User -> String
listAllText user =
    "Available commands: " ++ listFor user ++ ". To get help for a specific\
    \ command, use `haskbot help [command]`."
  where
    listFor        = intercalate ", " . map plName' . registry
    plName' plugin = "`" ++ plName plugin ++ "`"

getHelpFor :: User -> [String] -> String
getHelpFor user []       = listAllText user
getHelpFor user (a:args) =
  case plugin of
    Just p  -> unpack $ plHelpText p
    Nothing -> listAllText user
  where
    plugin = find (\p -> plName p == a) $ registry user

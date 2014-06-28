{-# LANGUAGE OverloadedStrings #-}

module Plugins.Help (plugin) where

import qualified Data.Map      as M

import           Parser.Common (commandWithText)
import           Registry      (registry)
import           Type.Plugin   (Plugin, Name, HelpText, InputParser,
                                newPlugin)

-- constants

name :: Name
name = "help"

helpText :: HelpText
helpText = "List all installed plugins via `haskbot help`. To see the help\
           \ text of a particular plugin, use `haskbot help [plugin name]`."

-- public functions

plugin :: Plugin
plugin = newPlugin name helpText parser

-- private functions

parser :: InputParser
parser = commandWithText "help" getHelp

getHelp :: String -> String
getHelp "" = ""

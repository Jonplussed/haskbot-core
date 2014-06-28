{-# LANGUAGE OverloadedStrings #-}

module Plugins.TableFlip (plugin) where

import           Data.Char      (toLower)
import           Data.List      (foldl')
import qualified Data.Map       as M

import           Parser.Common  (commandWithText)
import           Type.Plugin    (Plugin, Name, HelpText, InputParser,
                                 newPlugin)

-- constants

name :: Name
name = "flip"

helpText :: HelpText
helpText = "Displeased with something? Type \"haskbot flip [any other text]\"\
           \ to have Haskbot cathartically toss what ails you."

flippedLets, uprightLets :: String
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

plugin :: Plugin
plugin = newPlugin name helpText parser

-- private functions

parser :: InputParser
parser = commandWithText "flip" output

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

output :: String -> String
output str = "(╯°□°）╯︵ " ++ flipAnything str

flipAnything :: String -> String
flipAnything "table" = "┻━┻"
flipAnything str = foldl' flipLet "" str
  where
    flipLet s l = M.findWithDefault l' l' charMap : s
      where
        l' = toLower l

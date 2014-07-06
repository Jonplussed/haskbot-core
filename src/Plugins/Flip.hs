{-# LANGUAGE OverloadedStrings #-}

module Plugins.Flip (plugin) where

import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Map as M

import Parser.Common (withText)
import Type.Plugin
import Type.SlackMsg

-- constants

name :: Name
name = "flip"

helpText :: HelpText
helpText = "Displeased with something? Type `haskbot flip [any other text]`\
           \ to have Haskbot cathartically toss what ails you."

flippedLets, uprightLets :: String
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

plugin :: SlackMsg -> Plugin
plugin = newPlugin name helpText . parser

-- private functions

parser :: SlackMsg -> InputParser
parser _ = withText $ return . genOutput

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

genOutput :: String -> String
genOutput str = "(╯°□°）╯︵ " ++ flipChars str

flipChars :: String -> String
flipChars "table" = "┻━┻"
flipChars str = foldl' flipLet "" str
  where
    flipLet s l = M.findWithDefault l' l' charMap : s
      where
        l' = toLower l

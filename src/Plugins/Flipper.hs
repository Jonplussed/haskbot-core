module Plugins.Flipper (angryFlip) where

-- Haskell platform libraries

import           Data.Char (toLower)
import           Data.List (foldl')
import qualified Data.Map  as M

-- native libraries

import Settings
import Parsers  (Plugin, commandWithText)

-- constants

flippedLets, uprightLets :: String
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

angryFlip :: Plugin
angryFlip = commandWithText "flip" output

-- private functions

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

output :: String -> String
output str = "(╯°□°）╯︵ " ++ tableFlip str

tableFlip :: String -> String
tableFlip "table" = "┻━┻"
tableFlip str = foldl' flipLet "" str
  where
    flipLet s l = M.findWithDefault l (toLower l) charMap : s

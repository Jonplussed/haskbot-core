module Plugins.TableFlip (tableFlip) where

import           Data.Char      (toLower)
import           Data.List      (foldl')
import qualified Data.Map       as M

import           Parser.Commons (Plugin, commandWithText)

-- constants

flippedLets, uprightLets :: String
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

tableFlip :: Plugin
tableFlip = commandWithText "flip" output

-- private functions

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

module Hasklets.Flipper (angryFlip) where

import qualified Data.Map as M

import Hasklet.Skeleton (Hasklet, hasklet)

--
-- public methods
--

angryFlip :: Hasklet
angryFlip = hasklet trigger reaction

--
-- private methods
--

flippedLets, flipper, trigger, uprightLets :: String
trigger = "^flip (.*)$"
flipper = "(╯°□°）╯︵ "
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

reaction :: String -> [String] -> String
reaction _ matches = flipper ++ (tableFlip $ head matches)

tableFlip :: String -> String
tableFlip "table" = "┻━┻"

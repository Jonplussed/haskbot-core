module Hasklets.Flipper (angryFlip) where

import Data.Char
import Data.List (foldl')
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

flippedLets, trigger, uprightLets :: String
trigger = "^flip (.*)$"
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

reaction :: String -> [String] -> String
reaction _ matches = "(╯°□°）╯︵ " ++ (tableFlip $ head matches)

tableFlip :: String -> String
tableFlip "table" = "┻━┻"
tableFlip str = foldl' flipLet "" str
  where
    flipLet s l = M.findWithDefault l (toLower l) charMap : s

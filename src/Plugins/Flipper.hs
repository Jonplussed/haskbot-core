module Plugins.Flipper (angryFlip) where

-- Haskell platform libraries

import           Data.Char
import           Data.List              (foldl')
import qualified Data.Map               as M
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim

-- native libraries

import Settings

-- public functions

angryFlip :: Parser String
angryFlip = do
    string "flip"
    space
    toFlip <- manyTill anyChar eof
    return $ reaction toFlip

-- private functions

flippedLets, uprightLets :: String
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

reaction :: String -> String
reaction str = "(╯°□°）╯︵ " ++ tableFlip str

tableFlip :: String -> String
tableFlip "table" = "┻━┻"
tableFlip str = foldl' flipLet "" str
  where
    flipLet s l = M.findWithDefault l (toLower l) charMap : s

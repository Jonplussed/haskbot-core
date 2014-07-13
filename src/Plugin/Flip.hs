{-# LANGUAGE OverloadedStrings #-}

module Plugin.Flip (register) where

import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T

import Slack.SlashCom (replySameChan, text)
import Slack.Plugin

-- constants

name :: NameStr
name = "flip"

helpText :: HelpStr
helpText = "Displeased with something? Type `haskbot flip [any other text]`\
           \ to have Haskbot cathartically toss what ails you."

flippedLets, uprightLets :: [Char]
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

register :: TokenStr -> Plugin
register = newPlugin name helpText handler

-- private functions

handler :: HandlerFn
handler slashCom = viaHaskbot chan reply
  where
    chan  = replySameChan slashCom
    reply = flipIt $ text slashCom

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

flipIt :: T.Text -> T.Text
flipIt = T.append "(╯°□°)╯︵" . flipChars

flipChars :: T.Text -> T.Text
flipChars "table" = "┻━┻"
flipChars str = T.reverse $ T.map flipLet str
  where
    flipLet l = M.findWithDefault l' l' charMap
      where
        l' = toLower l

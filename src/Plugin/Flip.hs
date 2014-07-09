{-# LANGUAGE OverloadedStrings #-}

module Plugin.Flip (register) where

import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T

import Slack.Incoming
import Slack.Plugin
import Slack.SlashCom

-- constants

name :: Name
name = "flip"

helpText :: HelpText
helpText = "Displeased with something? Type `haskbot flip [any other text]`\
           \ to have Haskbot cathartically toss what ails you."

flippedLets, uprightLets :: [Char]
uprightLets = "abcdefghijklmnopqrstuvwxyz.!?()[]<>"
flippedLets = "ɐqɔpǝɟƃɥᴉɾʞlɯuodbɹsʇnʌʍxʎz˙¡¿)(][><"

-- public functions

register :: Token -> Plugin
register = newPlugin name helpText handler

-- private functions

handler :: Handler
handler slashCom = return . ViaHaskbot $ Incoming chan reply
  where
    chan  = channel slashCom
    reply = flipIt $ text slashCom

charMap :: M.Map Char Char
charMap = M.fromList $ zip (u ++ f) (f ++ u)
  where
    u = uprightLets
    f = flippedLets

flipIt :: T.Text -> T.Text
flipIt str = T.append "(╯°□°）╯︵ " $ flipChars str

flipChars :: T.Text -> T.Text
flipChars "table" = "┻━┻"
flipChars str = T.reverse $ T.map flipLet str
  where
    flipLet l = M.findWithDefault l' l' charMap
      where
        l' = toLower l

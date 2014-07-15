{-# LANGUAGE OverloadedStrings #-}

module Plugin.Status (register) where

import qualified Data.Text as T

import App.Environment (Haskbot)
import App.MemStore (Key, fromValue, get, set, toKey, toValue)
import Slack.SlashCom (SlashCom, replyViaDM, text, userName)
import Slack.Types (UserName, getUserName, getAtUserName, setUserName)
import Slack.Plugin

-- constants

name :: NameStr
name = "status"

helpText :: HelpStr
helpText = T.pack
  "Set your status with `haskbot status [my status]`; see the statuses of\
  \ others with `haskbot status of [username]`"

-- public functions

register :: TokenStr -> Plugin
register = newPlugin name helpText handler

-- private functions

noStatus :: UserName -> T.Text
noStatus user = T.unwords [getAtUserName user, "has not set a status"]

handler :: HandlerFn
handler slashCom =
  case T.words (text slashCom) of
    ["of", name] -> getStatus name >>= viaHaskbot (replyViaDM slashCom)
    status       -> setStatus slashCom >> noReply

statusKey :: UserName -> Key
statusKey user = toKey . T.append "status-" $ getUserName user

getStatus :: T.Text -> Haskbot T.Text
getStatus name = do
    let user = setUserName name
    val <- get (statusKey user)
    return $ case val of
      Just st -> T.unwords [getAtUserName user, "is", fromValue st]
      Nothing -> noStatus user

setStatus :: SlashCom -> Haskbot ()
setStatus slashCom = set value key
  where
    key   = statusKey $ userName slashCom
    value = toValue $ text slashCom

{-# LANGUAGE OverloadedStrings #-}

module Network.Haskbot.Internal.SlashCommand
( SlashCom (..)
, fromParams
) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Network.Haskbot.Internal.Environment (ActionH)
import Network.Haskbot.SlashCommand (SlashCom (..))
import Network.Haskbot.Types
import Web.Scotty.Trans (param)

-- public functions

fromParams :: ActionH SlashCom
fromParams = newSlashCom <$> param "token"
                         <*> param "team_id"
                         <*> param "channel_id"
                         <*> param "channel_name"
                         <*> param "user_id"
                         <*> param "user_name"
                         <*> param "command"
                         <*> param "text"

-- private functions

newSlashCom :: Text -> Text -> Text -> Text
            -> Text -> Text -> Text -> Text
            -> SlashCom
newSlashCom a b c d e f g =
  SlashCom (setToken a)
           (setTeamID b)
           (setChanID c)
           (setChanName d)
           (setUserID e)
           (setUserName f)
           (setCommand g)

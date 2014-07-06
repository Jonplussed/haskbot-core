{-# LANGUAGE OverloadedStrings #-}

module Protocol.Slack.Request (request) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Web.Scotty hiding (request)

import Type.SlackMsg

-- constants

tokenVar :: String
tokenVar = "SLACK_OUTGOING_TOKEN"

-- public functions

request :: ActionM SlackMsg
request = authorize >> fromParams

-- private functions

authorize :: ActionM ()
authorize = do
    yourToken <- liftIO $ getEnv tokenVar
    myToken   <- param "token"
    if myToken == yourToken
      then return ()
      else fail "unauthorized"

fromParams :: ActionM SlackMsg
fromParams = newSlackMsg <$> param "team_id"
                         <*> param "channel_id"
                         <*> param "channel_name"
                         <*> param "user_id"
                         <*> param "user_name"
                         <*> param "command"
                         <*> param "text"

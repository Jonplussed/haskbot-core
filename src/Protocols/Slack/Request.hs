{-# LANGUAGE OverloadedStrings #-}

module Protocols.Slack.Request
( Request (..)
, request
) where

import Control.Applicative    ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment     (getEnv)
import Web.Scotty             hiding (request)

import Types.User             (HasUser (getUser), fromStrings)

data Request = Request { teamId      :: String
                       , channelId   :: String
                       , channelName :: String
                       , timestamp   :: String
                       , userId      :: String
                       , userName    :: String
                       , text        :: String
                       } deriving (Eq, Show)

instance HasUser Request where
  getUser req = fromStrings (userId req) (userName req)

-- constants

tokenVar :: String
tokenVar = "SLACK_TOKEN"

-- public functions

request :: ActionM Request
request = authorize >> fromParams

-- private functions

authorize :: ActionM ()
authorize = do
    yourToken <- liftIO $ getEnv tokenVar
    myToken   <- param "token"
    if myToken == yourToken
      then return ()
      else fail "unauthorized"

fromParams :: ActionM Request
fromParams = Request <$> param "team_id"
                     <*> param "channel_id"
                     <*> param "channel_name"
                     <*> param "timestamp"
                     <*> param "user_id"
                     <*> param "user_name"
                     <*> param "text"

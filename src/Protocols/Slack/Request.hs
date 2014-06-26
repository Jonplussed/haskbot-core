{-# LANGUAGE OverloadedStrings #-}

module Protocols.Slack.Request
( Request (..)
, request
) where

import Control.Applicative    ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment     (getEnv)

import Web.Scotty             hiding (request)

import Settings               (slackTokenEnvVar)

data Request = Request { username  :: String
                       , text      :: String
                       , timestamp :: String
                       } deriving (Eq, Show)

request :: ActionM Request
request = authorize >> fromParams

-- private functions

authorize :: ActionM ()
authorize = do
    yourToken <- liftIO $ getEnv slackTokenEnvVar
    myToken   <- param "token"
    if myToken == yourToken
      then return ()
      else fail "unauthorized"

fromParams :: ActionM Request
fromParams = Request <$> param "username"
                     <*> param "text"
                     <*> param "timestamp"

{-# LANGUAGE OverloadedStrings #-}

module Application.WebServer (webServer) where

import System.Environment (getEnv)
import Web.Scotty

import qualified Protocol.Slack.Request as Slack
import qualified Protocol.Slack.Response as Slack

webServer :: Int -> IO ()
webServer port = scotty port $ do
    post "/slack" $ Slack.request >>= Slack.response

{-# LANGUAGE OverloadedStrings #-}

module Server
( server
) where

import           Web.Scotty

import qualified Protocols.Slack.Request  as Slack
import qualified Protocols.Slack.Response as Slack
import           Settings                 (portNum)

-- public functions

server :: IO ()
server = scotty portNum $ do
  post "/slack" $ Slack.request >>= Slack.response

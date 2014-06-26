{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import           Web.Scotty

import qualified Protocols.Slack.Request  as Slack
import qualified Protocols.Slack.Response as Slack

-- constants

port :: Int
port = 8000

-- public functions

main :: IO ()
main = scotty port $ do
  post "/slack" $ Slack.request >>= Slack.response

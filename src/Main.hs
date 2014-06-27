{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import           System.Environment       (getEnv)
import           Web.Scotty

import qualified Protocols.Slack.Request  as Slack
import qualified Protocols.Slack.Response as Slack

-- constants

portVar :: String
portVar = "PORT"

-- public functions

main :: IO ()
main = getEnv portVar >>= runServerOn . read

-- private functions

runServerOn :: Int -> IO ()
runServerOn port = scotty port $ do
    post "/slack" $ Slack.request >>= Slack.response

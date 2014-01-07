module Main (main) where

import qualified Connector.Slack as Slack

main :: IO ()
main = Slack.server

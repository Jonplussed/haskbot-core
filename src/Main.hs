module Main (main) where

import qualified Connectors.Slack as Slack

main :: IO ()
main = Slack.server

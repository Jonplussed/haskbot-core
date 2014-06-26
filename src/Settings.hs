module Settings
  ( botName
  , portNum
  , slackTokenEnvVar
  ) where

portNum :: Int
portNum = 3000

botName :: String
botName = "haskbot"

slackTokenEnvVar :: String
slackTokenEnvVar = "SLACK_TOKEN"

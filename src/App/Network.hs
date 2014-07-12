module App.Network where

-- constants

asJson :: String
asJson = "application/json"

slackUrl :: String
slackUrl = "https://bendyworks.slack.com/services/hooks/incoming-webhook"

-- private functions

slackUrlWithToken :: String -> String
slackUrlWithToken t = slackUrl ++ "?token=" ++ t

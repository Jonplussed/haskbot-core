module Connection.Slack where

-- constants

asJson :: String
asJson = "application/json"

slackUrl :: String
slackUrl = "https://bendyworks.slack.com/services/hooks/incoming-webhook"

-- public functions


-- private functions

slackUrlWithToken :: String -> String
slackUrlWithToken t = slackUrl ++ "?token=" ++ t



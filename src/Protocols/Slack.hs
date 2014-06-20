module Protocols.Slack (respond) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Happstack.Server
  ( Response
  , RqData
  , ServerPart
  , badRequest
  , body
  , getDataFn
  , look
  , ok
  , toResponse
  )

data SlackMsg = SlackMsg { channelName :: String
                         , timeStamp   :: String
                         , userName    :: String
                         , msgText     :: String
                         } deriving (Eq, Show)

-- constants

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

-- public functions

respond :: ServerPart Response
respond = do
  isValid <- hasValidToken
  case isValid of
    True  -> replyToSlack
    False -> bad "unauthorized request"

-- private functions

hasValidToken :: ServerPart Bool
hasValidToken = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  return $ tActual == tReceived

slackMsg :: RqData SlackMsg
slackMsg = SlackMsg <$> bl "channel_name"
                    <*> bl "timestamp"
                    <*> bl "user_name"
                    <*> bl "text"
  where bl = body . look

replyToSlack :: ServerPart Response
replyToSlack = do
    r <- getDataFn slackMsg
    case r of (Left e)         -> bad $ unlines e
              (Right slackMsg) -> good $ "hello"

good, bad :: String -> ServerPart Response
good = ok . toResponse
bad  = badRequest . toResponse

module Protocols.Slack (respond) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Printf

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
  , unauthorized
  , setResponseCode
  )

import Hasklets (hasklets)
import Settings

data SlackMsg = SlackMsg { secretToken :: String
                         , channelName :: String
                         , timeStamp   :: String
                         , userName    :: String
                         , msgText     :: String
                         } deriving (Eq, Show)

-- constants

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

-- public functions

respond :: ServerPart String
respond = parseRequest

-- private functions

parseRequest :: ServerPart String
parseRequest = do
    msg <- getDataFn slackMsg
    case msg of
      Left errors -> badRequest $ unlines errors
      Right m     -> validateToken m

validateToken :: SlackMsg -> ServerPart String
validateToken msg = do
    token <- liftIO $ getEnv tokenEnvVar
    if token == secretToken msg
      then craftResponse msg
      else unauthorized "invalid secret token"

craftResponse :: SlackMsg -> ServerPart String
craftResponse msg =
    case applyHasklets msg of
      Right str -> ok $ toJSON str (userName msg)
      Left err -> badRequest $ show err
  where

slackMsg :: RqData SlackMsg
slackMsg = SlackMsg <$> bl "token"
                    <*> bl "channel_name"
                    <*> bl "timestamp"
                    <*> bl "user_name"
                    <*> bl "text"
  where bl = body . look

applyHasklets :: SlackMsg -> Either ParseError String
applyHasklets msg = parse parser str str
  where
    parser = do
        string chatbotName
        try $ char ':'
        spaces
        choice hasklets
    str = msgText msg

toJSON :: String -> String -> String
toJSON = printf "{\"text\":\"%s\", \"username\":\"%s\"}"

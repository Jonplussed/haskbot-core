module Slack.Responder
  ( SlackMsg(..)
  , respondToMsg
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Happstack.Server
  ( FromData (fromData)
  , Response
  , ServerPart
  , badRequest
  , body
  , getData
  , look
  , ok
  , toResponse
  )

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

data SlackMsg =
  SlackMsg { user :: String, text :: String }
  deriving (Eq, Show)

instance FromData SlackMsg where
  fromData = SlackMsg <$> bl "user_name" <*> bl "text"
    where bl = body . look

--
-- public functions
--

respondToMsg :: ServerPart Response
respondToMsg = do
  r <- getData >>= validateToken
  case r of
    (Left e) -> badRequest . toResponse $ unlines e
    (Right msg) -> craftResponse msg

--
-- private functions
--

craftResponse :: SlackMsg -> ServerPart Response
craftResponse msg = ok . toResponse $ user msg ++ ": " ++ text msg

validateToken :: Either [String] SlackMsg -> ServerPart (Either [String] SlackMsg)
validateToken msg = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  let validate m =
        if tActual == tReceived
        then Right m
        else Left ["unauthorized request"]
  return $ msg >>= validate

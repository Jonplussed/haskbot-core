module Protocols.Slack (respond) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

import Chat.Message (Message, message)
import Hasklet.All (unleashUpon)

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

tokenEnvVar :: String
tokenEnvVar = "SLACK_TOKEN"

--
-- public functions
--

respond :: ServerPart Response
respond = do
  isValid <- hasValidToken
  if isValid
  then parseData
  else bad "unauthorized request"

--
-- private functions
--

good, bad :: String -> ServerPart Response
good = ok . toResponse
bad  = badRequest . toResponse

formatMsg :: RqData Message
formatMsg = message <$> bl "user_name" <*> bl "text"
          where bl = body . look

hasValidToken :: ServerPart Bool
hasValidToken = do
  tActual <- liftIO $ getEnv tokenEnvVar
  tReceived <- look "token"
  return $ tActual == tReceived

parseData :: ServerPart Response
parseData = do
  r <- getDataFn formatMsg
  case r of (Left e)    -> bad $ unlines e
            (Right msg) -> good $ unleashUpon msg
